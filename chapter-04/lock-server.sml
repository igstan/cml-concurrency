structure LockServer :> LOCK_SERVER =
struct
  open CML Fn

  structure Map = IntBinaryMap

  datatype lock = LOCK of int

  datatype req_msg =
    REL of int
  | ACQ of {
      lock : int,
      replyCh : unit chan,
      abortEvt : unit event
    }

  datatype server =
    SERVER of {
      idServer : unit -> int,
      reqCh : req_msg chan
    }

  (**
   * Based on listing 4.6 in the book.
   *)
  fun create () =
    let
      val idServer = IDService.mkUIdSrc ()
      val reqCh = channel ()
      fun replyToAcquire (replyCh, abortEvt) =
        select [
          wrap (sendEvt (replyCh, ()), const true),
          wrap (abortEvt, const false)
        ]
      fun server locks =
        case recv reqCh of
          ACQ { lock, replyCh, abortEvt } =>
          let in
            case Map.find (locks, lock) of
              NONE =>
              if replyToAcquire (replyCh, abortEvt)
              then server (Map.insert (locks, lock, ref []))
              else server locks
            | SOME pending =>
              let in
                pending := !pending @ [(replyCh, abortEvt)];
                server locks
              end
          end
        | REL lock =>
          let
            val (locks', pending) = Map.remove (locks, lock)
            fun assign [] = locks'
              | assign (req :: r) =
                if replyToAcquire req
                then (pending := r; locks)
                else assign r
          in
            server (assign (!pending))
          end
    in
      spawn (fn () => server Map.empty)
    ; SERVER { idServer = idServer, reqCh = reqCh }
    end

  fun produceLock (SERVER { idServer, ... }) =
    LOCK (idServer ())

  fun releaseLock (SERVER { reqCh, ... }) (LOCK id) =
    send (reqCh, REL id)

  fun acquireLockEvt (SERVER { reqCh, ... }) (LOCK id) =
    withNack (fn nack =>
      let
        val replyCh = channel ()
      in
        spawn (fn () => send (reqCh, ACQ {
          lock = id,
          replyCh = replyCh,
          abortEvt = nack
        }))
      ; recvEvt replyCh
      end)
end
