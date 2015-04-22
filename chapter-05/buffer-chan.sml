(*
 * Based on listing 5.5 in the book.
 *)
structure BufferChan :> BUFFER_CHAN =
struct
  open CML

  structure Q = Fifo

  datatype 'a buffer_chan =
    BC of {
      getCh : 'a chan,
      putCh : 'a chan
    }

  fun bufferChannel () =
    let
      val getCh = channel ()
      val putCh = channel ()

      fun loop q =
        if Q.isEmpty q
        then loop (Q.enqueue (q, recv putCh))
        else select [
          wrap (recvEvt putCh, fn a => Q.enqueue (q, a)),
          wrap (sendEvt (getCh, Q.head q), fn () => loop (#1 (Q.dequeue q)))
        ]
    in
      spawn (fn () => ignore (loop Q.empty))
    ; BC { getCh = getCh, putCh = putCh }
    end

  fun send (BC { putCh, ... }, a) = CML.send (putCh, a)

  fun recvEvt (BC { getCh, ... }) = CML.recvEvt getCh
end
