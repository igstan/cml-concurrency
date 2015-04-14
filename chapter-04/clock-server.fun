local
  open CML
in
  signature CLOCK_SERVER_ARG =
  sig
    val timerReqCh : (Time.time * unit chan) chan
    val tick : unit event
  end

  (**
   * Implement `ClockServer` as a functor because I have no idea how to write
   * `timerReqCh` and `tick`.
   *)
  functor ClockServer (C : CLOCK_SERVER_ARG) =
  struct
    fun atTimeEvt time =
      let
        fun event () =
          let
            val replyCh = channel ()
          in
            spawn (fn () => send (C.timerReqCh, (time, replyCh)))
          ; recvEvt replyCh
          end
      in
        guard event
      end

    (*
     * Listing 4.5 in the book.
     *)
    fun server waitingList =
      let
        fun insert (req as (time, _), []) = [req]
          | insert (req as (time, _), (elem as (time', _)) :: rest) =
            if Time.< (time, time')
            then req :: elem :: rest
            else elem :: (insert (req, rest))
        fun wakeup () =
          let
            val now = Time.now ()
            fun wake [] = []
              | wake (waiting as ((time, replyCh) :: rest)) =
                if Time.< (now, time)
                then waiting
                else let in
                  (*
                   * We spawn a new thread here because the wake up message must
                   * be sent asynchronously, which is required because the client
                   * might have synchronized on a choice between the `atTimeEvt`
                   * and some other synchronous operation. If that other operation
                   * is selected before `atTimeEvt`, then the `send` below would
                   * block forever, i.e. deadlock.
                   *
                   * Having a spawned block thread is, however, not a problem,
                   * because it is subject to garbage collection.
                   *)
                  spawn (fn () => send (replyCh, ()))
                ; wake rest
                end
          in
            wake waitingList
          end
      in
        select [
          wrap (recvEvt C.timerReqCh, fn req => server (insert (req, waitingList))),
          wrap (C.tick, fn () => server (wakeup ()))
        ]
      end
  end
end
