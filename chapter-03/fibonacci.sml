structure Fibonacci =
struct
  open CML

  fun forever start f =
    let
      fun loop s = loop (f s)
    in
      ignore (spawn (fn () => loop start))
    end

  (**
   * Add numbers from two streams.
   *)
  fun add inCh1 inCh2 outCh =
    let
      fun sum () =
        let
          val (a, b) = select [
            wrap (recvEvt inCh1, fn a => (a, recv inCh2)),
            wrap (recvEvt inCh2, fn b => (recv inCh1, b))
          ]
          val r = a + b handle
            Overflow =>
              let in
                print ("Overflow: "^ Int.toString a ^" + "^ Int.toString b ^"\n")
              ; ~1
              end
        in
          send (outCh, r)
        end
    in
      forever () sum
    end

  fun delay start inCh outCh =
    let
      fun loop NONE     = SOME (recv inCh)
        | loop (SOME a) = (send (outCh, a); NONE)
    in
      forever start loop
    end

  fun copy inCh outCh1 outCh2 =
    let
      fun loop () =
        let
          val a = recv inCh
        in
          select [
            wrap (sendEvt (outCh1, a), fn () => send (outCh2, a)),
            wrap (sendEvt (outCh2, a), fn () => send (outCh1, a))
          ]
        end
    in
      forever () loop
    end

  fun fibonacci () =
    let
      val outCh = channel ()
      val c1 = channel ()
      val c2 = channel ()
      val c3 = channel ()
      val c4 = channel ()
      val c5 = channel ()
    in
      delay (SOME 0) c4 c5
    ; copy c2 c3 c4
    ; add c3 c5 c1
    ; copy c1 c2 outCh
    ; send (c1, 1)
    ; outCh
    end
end
