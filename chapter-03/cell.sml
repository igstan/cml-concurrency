(*
 * Listing 3.1 in the book.
 *)
structure Cell : CELL =
struct
  open CML

  datatype 'a request =
    GET
  | PUT of 'a

  datatype 'a cell = CELL of {
    reqCh : 'a request chan,
    resCh : 'a chan
  }

  fun cell a =
    let
      val reqCh = channel ()
      val resCh = channel ()

      fun server a =
        case recv reqCh of
          GET => (send (resCh, a); server a)
        | PUT a => server a
    in
      spawn (fn () => server a)
    ; CELL { reqCh = reqCh, resCh = resCh }
    end

  fun get (CELL { reqCh, resCh }) =
    let in
      send (reqCh, GET)
    ; recv resCh
    end

  fun put (CELL { reqCh, ... }) a =
    send (reqCh, PUT a)
end

(*
 * Listing 3.4 in the book.
 *)
structure SelectiveCell : CELL =
struct
  open CML

  datatype 'a cell =
    CELL of {
      getCh : 'a chan,
      putCh : 'a chan
    }

  fun cell a =
    let
      val getCh = channel ()
      val putCh = channel ()
      fun loop a = select [
        wrap (sendEvt (getCh, a), fn () => loop a),
        wrap (recvEvt putCh, loop)
      ]
    in
      spawn (fn () => loop a)
    ; CELL { getCh = getCh, putCh = putCh }
    end

  fun get (CELL { getCh, ... }) = recv getCh

  fun put (CELL { putCh, ... }) a = send (putCh, a)
end

(*
 * This implementation demonstrates how events are first-class abstractions.
 *
 * We are able to use `select` and `wrap` with `DirChan` because the latter
 * exposes two events — `sendEvt` and `recvEvt`. Had it exposed only `recv`
 * and `send`, then we couldn't have used those with `wrap` and `select`.
 *
 * Events are a more lax form of abstraction than procedural abstraction and
 * this allows for composability when writing concurrent code with CML.
 *
 * Having events as first-class values is akin to having functions as first-
 * class values.
 *
 * Listing 3.7 in the book.
 *)
structure DirChanCell : CELL =
struct
  open CML

  type 'a cell = 'a DirChan.in_chan * 'a DirChan.out_chan

  fun cell a =
    let
      val (getInCh, getOutCh) = DirChan.channel ()
      val (putInCh, putOutCh) = DirChan.channel ()
      (*
       * We need selective communication here because we can't know whether
       * a client will try to read or write a value. So, whichever comes first
       * is the winner.
       *)
      fun loop a = select [
        wrap (DirChan.sendEvt (getOutCh, a), fn () => loop a),
        wrap (DirChan.recvEvt putInCh, loop)
      ]
    in
      spawn (fn () => loop a)
    ; (getInCh, putOutCh)
    end

  fun get (ch, _) = DirChan.recv ch

  fun put (_, ch) a = DirChan.send (ch, a)
end
