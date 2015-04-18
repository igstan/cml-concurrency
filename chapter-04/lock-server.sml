structure LockServer :> LOCK_SERVER =
struct
  open CML Fn

  structure Map = IntBinaryMap

  (**
   * The possible requests a client can send to the lock server. Both are hidden
   * using procedural abstraction. See `releaseLock` and `acquireLockEvt`.
   *)
  datatype req_msg =
    REL of int
  | ACQ of {
      lock : int,
      replyCh : unit chan,
      abortEvt : unit event
    }

  (**
   * The state of a server, consisting of:
   *
   * - a server for generating unique IDs, used to create new locks
   * - a request channel, used by clients to send their requests
   *)
  datatype server =
    SERVER of {
      idServer : unit -> int,
      reqCh : req_msg chan
    }

  (**
   * A lock is identified by a number.
   *)
  datatype lock = LOCK of int * server

  (**
   * Spawns a new lock server.
   *
   * Based on listing 4.6 in the book.
   *)
  fun create () =
    let
      val idServer = IDService.create ()
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

  (**
   * Create a fresh lock under the given server.
   *)
  fun produceLock (server as SERVER { idServer, ... }) =
    LOCK (idServer (), server)

  (**
   * Releases the given lock from the server.
   *)
  fun releaseLock (LOCK (id, SERVER { reqCh, ... })) =
    send (reqCh, REL id)

  (**
   * Send a lock acquisition request to the given server.
   *)
  fun acquireLockEvt (LOCK (id, SERVER { reqCh, ... })) =
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
