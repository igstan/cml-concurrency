structure MergeSort =
struct
  open CML

  (*
   * Listing 4.2 in the book.
   *)
  fun split (inCh, outCh1, outCh2) =
    let
      fun loop (value, outCh1, outCh2) =
        case value of
          NONE => (send (outCh1, NONE) ; send (outCh2, NONE))
        | some => (send (outCh1, some) ; loop (recv inCh, outCh2, outCh1))
    in
      loop (recv inCh, outCh1, outCh2)
    end

  (*
   * Listing 4.2 in the book.
   *)
  fun merge (inCh1, inCh2, outCh) =
    let
      (**
       * Drain all elements from `fromCh` into `toCh`
       *)
      fun drain (fromCh, toCh) =
        let
          fun loop NONE = send (toCh, NONE)
            | loop some = (send (toCh, some); loop (recv fromCh))
        in
          loop (recv fromCh)
        end
      fun loop (from1, from2) =
        case (from1, from2) of
          (NONE, NONE) => send (outCh, NONE)
        | (_, NONE) => (send (outCh, from1); drain (inCh1, outCh))
        | (NONE, _) => (send (outCh, from2); drain (inCh2, outCh))
        | (SOME a, SOME b) =>
          if a < b
          then (send (outCh, from1); loop (recv inCh1, from2))
          else (send (outCh, from2); loop (from1, recv inCh2))
    in
      loop (recv inCh1, recv inCh2)
    end

  (*
   * Listing 4.1 in the book.
   *)
  fun mergeSort () =
    let
      val ch = channel ()
      fun sort () =
        let in
          case recv ch of
            NONE => ()
          | v1 =>
            case recv ch of
              NONE => send (ch, v1)
            | v2 =>
              let
                val ch1 = mergeSort ()
                val ch2 = mergeSort ()
              in
                send (ch1, v1)
              ; send (ch2, v2)
              ; split (ch, ch1, ch2)
              ; merge (ch1, ch2, ch)
              end
        ; send (ch, NONE)
        end
    in
      spawn sort
    ; ch
    end
end
