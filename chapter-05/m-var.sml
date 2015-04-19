(**
 * Based on listing 5.1 in the book.
 *)
structure MVar : M_VAR =
struct
  open CML

  exception Put

  datatype 'a mvar =
    MVAR of {
      getCh : 'a chan,
      putCh : 'a chan,
      ackCh : bool chan
    }

  fun empty () =
    let
      val getCh = channel ()
      val putCh = channel ()
      val ackCh = channel ()

      fun empty () =
        let
          val x = recv putCh
        in
          send (ackCh, true)
        ; full x
        end

      and full x =
        select [
          wrap (sendEvt (getCh, x), empty),
          wrap (recvEvt putCh, fn _ => (send (ackCh, false); full x))
        ]
    in
      spawn empty
    ; MVAR { getCh = getCh, putCh = putCh, ackCh = ackCh }
    end

  fun getEvt (MVAR { getCh, ... }) = recvEvt getCh

  fun get mvar = sync (getEvt mvar)

  fun put (MVAR { putCh, ackCh, ... }) x =
    let in
      send (putCh, x)
    ; if recv ackCh then () else raise Put
    end
end
