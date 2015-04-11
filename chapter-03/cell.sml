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
      spawn (fn () => server a);
      CELL { reqCh = reqCh, resCh = resCh }
    end

  fun get (CELL { reqCh, resCh }) =
    let in
      send (reqCh, GET);
      recv resCh
    end

  fun put (CELL { reqCh, ... }) a =
    send (reqCh, PUT a)
end
