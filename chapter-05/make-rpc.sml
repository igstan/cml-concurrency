(**
 * Based on listing 5.10 in the book.
 *)
structure MakeRPC :> MAKE_RPC =
struct
  open CML

  fun mkRPC f =
    let
      (*
       * Each server operation has its own communication channel used. Client
       * calls are pushed on it, while the server-side processor reads from it.
       *)
      val reqCh = channel ()

      (*
       * When a client calls this operation, allocate a new I-var for
       * the response and push it along with the argument on the requests
       * channel. Finally, block on reading the value from the I-var.
       *)
      fun call arg =
        let
          val reply = SyncVar.iVar ()
        in
          send (reqCh, (arg, reply))
        ; SyncVar.iGet reply
        end

      (*
       * The server-side implementation of the operation simply calls the
       * passed in transition function with the current server state and
       * the supplied operation argument. The result is put in the I-var
       * created by the client-side call and the next server state is
       * returned.
       *)
      fun entryEvt state =
        let
          fun doCall (arg, replyV) =
            let
              val (newState, result) = f (state, arg)
            in
              SyncVar.iPut (replyV, result)
            ; newState
            end
        in
          wrap (recvEvt reqCh, doCall)
        end
    in
      RPC.Proc { call = call, entryEvt = entryEvt }
    end

  fun mkServer initState operations =
    let
      fun loop state =
        let
          (* Call all the known server operations with the current state. *)
          val results = map (fn f => f state) operations
          (* The new state is whatever the first unblocked operation returns. *)
          val newState = select results
        in
          loop newState
        end
    in
      ignore (spawn (fn () => loop initState))
    end
end
