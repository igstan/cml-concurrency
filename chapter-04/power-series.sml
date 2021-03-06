(*
 * The main departure from the example in the book is that I'm using `int`
 * instead of `rational` here. Not sure how much of a difference that makes.
 *)
structure PowerSeries =
struct
  open CML

  (**
   * Power series addition.
   *)
  fun psAdd (psF, psG) =
    let
      val (inCh, outCh) = DirChan.channel ()
      fun add (f, g) = DirChan.send (outCh, f + g)
    in
      Pattern.repeat (fn _ => Pattern.combine add (DirChan.recvEvt psF, DirChan.recvEvt psG))
    ; inCh
    end

  (**
   * Power series constant multiplication.
   *)
  fun psConstMul (c, psF) =
    let
      val (inCh, outCh) = DirChan.channel ()
    in
      Pattern.repeat (fn _ => DirChan.send (outCh, c * DirChan.recv psF))
    ; inCh
    end

  (**
   * Power series term multiplication.
   *)
  fun psTermMul psF =
    let
      val (inCh, outCh) = DirChan.channel ()
      fun loop () = (DirChan.send (outCh, DirChan.recv psF); loop ())
    in
      spawn (fn () => DirChan.send (outCh, 0); loop ())
    ; inCh
    end

  (**
   * Copy a power series stream.
   *
   * Listing 4.3 in the book, with small changes.
   *)
  fun psCopy psF =
    let
      val (in1, out1) = DirChan.channel ()
      val (in2, out2) = DirChan.channel ()
      fun step _ =
        let
          val f = DirChan.recv psF
        in
          (* Q: Why does it have to sync on both sends here? *)
          Pattern.combine ignore (DirChan.sendEvt (out1, f), DirChan.sendEvt (out2, f))
        end
    in
      Pattern.repeat step
    ; (in1, in2)
    end

  (**
   * Power series multiplication.
   *)
  fun psMul (psF, psG) =
    let
      val (inCh, outCh) = DirChan.channel ()
      fun mul _ =
        let
          val f0 = DirChan.recv psF
          val g0 = DirChan.recv psG
          val (psF', psF'') = psCopy psF
          val (psG', psG'') = psCopy psG
          val psFG = psAdd (
            psTermMul (psMul (psF', psG')),
            psAdd (
              psConstMul (f0, psG''),
              psConstMul (g0, psF'')
            )
          )
          fun loop _ = (DirChan.send (outCh, DirChan.recv psFG); loop ())
        in
          DirChan.send (outCh, f0 * g0)
        ; loop ()
        end
    in
      spawn mul
    ; inCh
    end
end
