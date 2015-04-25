(**
 * Based on figure 2.7(a) from the book.
 *)
structure IVar :> I_VAR =
struct
  open CML

  exception Put

  datatype 'a ivar =
    IVAR of {
      getCh : 'a chan,
      putCh : 'a chan,
      ackCh : bool chan
    }

  fun iVar () =
    let
      val getCh = channel ()
      val putCh = channel ()
      val ackCh = channel ()

      fun full a =
        select [
          wrap (sendEvt (getCh, a), fn _ => full a),
          wrap (recvEvt putCh, fn _ => (send (ackCh, false); full a))
        ]

      fun empty () =
        let
          val a = recv putCh
        in
          send (ackCh, true)
        ; full a
        end
    in
      spawn empty
    ; IVAR { getCh = getCh, putCh = putCh, ackCh = ackCh }
    end

  fun iPut (IVAR { putCh, ackCh, ... }) a =
    let in
      send (putCh, a)
    ; if recv ackCh then () else raise Put
    end

  fun iGet (IVAR { getCh, ... }) = recv getCh

  fun iGetEvt (IVAR { getCh, ... }) = recvEvt getCh

  fun iGetPoll (IVAR { getCh, ... }) = recvPoll getCh

  fun sameIVar (IVAR a, IVAR b) =
    sameChannel (#getCh a, #getCh b) andalso
    sameChannel (#putCh a, #putCh b) andalso
    sameChannel (#ackCh a, #ackCh b)
end
