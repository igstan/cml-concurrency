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

      (*
       * Whenever a client tries to acquire a lock, we must verify whether the
       * client hasn't changed its mind up until now. We do that by synchronizing
       * also on the `abortEvt` sent from the client.
       *
       * If it is still waiting, then it'll receive a unit value on the reply
       * channel, which will unblock it, meaning that it has the lock now.
       *)
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
              (*
               * The lock has no waiting list, we can try to lease the lock
               * to the requesting client.
               *)
              if replyToAcquire (replyCh, abortEvt)
              then server (Map.insert (locks, lock, ref []))
              else server locks
            | SOME pending =>
              (*
               * If there are pending clients, then add the current one at the
               * end of the waiting queue.
               *)
              let in
                pending := !pending @ [(replyCh, abortEvt)];
                server locks
              end
          end
        | REL lock =>
          let
            (* Remove the lock from the list of held locks. *)
            val (locks', pending) = Map.remove (locks, lock)
            (* Try to lease the lock to the next pending client. *)
            fun assign [] = locks'
              | assign (req :: r) =
                if replyToAcquire req
                then (pending := r; locks)
                (*
                 * This seems risky. If a client holds a lock for a longer
                 * period of time, that lock may build up a huge waiting
                 * list, so it might take a while for this recursive call to
                 * finish. But maybe not significant enough to cause problems
                 * for requests to other locks. It probably all depends on how
                 * fast `replyToAcquire` is.
                 *)
                else assign r
          in
            (* Continue serving with the updated list of held locks. *)
            server (assign (!pending))
          end
    in
      spawnc server Map.empty
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
   *
   * Because the client may use selectively synchronize on this event, it
   * might happen that by the time we can give it the lock, the client is
   * no longer interested in it. At that point we have to release the lock,
   * otherwise any other client awaiting for that lock will be starved.
   *
   * For this reason, we need to know when the client is no longer interested
   * in the lock, and this is accomplished using a negative acknowledgment.
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
