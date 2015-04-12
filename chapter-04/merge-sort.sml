structure MergeSort =
struct
  open CML

  (**
   * Listing 4.2 in the book.
   *
   * Distributes, in an alternative fashion, elements from `inCh` to `outCh1`
   * and `outCh2`. The state of the alternation is kept implicit in the order
   * of arguments passed to `loop`.
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
      (* Drain all elements from `fromCh` into `toCh` *)
      fun drain (fromCh, toCh) =
        let
          fun loop NONE = send (toCh, NONE)
            | loop some = (send (toCh, some); loop (recv fromCh))
        in
          loop (recv fromCh)
        end

      (* Merge two sorted split-streams, element by element. *)
      fun loop (from1, from2) =
        case (from1, from2) of
          (* Both split-streams have been consume, sorting is done. *)
          (NONE, NONE) => send (outCh, NONE)

          (*
           * If any of the split-streams has been consumed, drain the other
           * one into the result channel, as it's already sorted.
           *)
        | (_, NONE) => (send (outCh, from1); drain (inCh1, outCh))
        | (NONE, _) => (send (outCh, from2); drain (inCh2, outCh))

          (* Merge two elements by comparison, i.e., by sorting them. *)
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
      (*
       * The channel where unsorted elements are pushed from the client and
       * where the sorted elements are pushed from the user. `NONE` is used
       * to mark the end of both streams.
       *)
      val ch = channel ()

      (* The sorting thread. *)
      fun sort () =
        let in
          case recv ch of
            (* No elements; nothing to sort. *)
            NONE => ()
          | v1 =>
            case recv ch of
              (* A single element; stream is already sorted. *)
              NONE => send (ch, v1)
              (* At least two elements to sort; we need to sort. *)
            | v2 =>
              let
                (* Create the children sorting threads. *)
                val ch1 = mergeSort ()
                val ch2 = mergeSort ()
              in
                (*
                 * Send to the sorting threads the two elements we already have.
                 * This is basically the first step in the splitting process.
                 *)
                send (ch1, v1)
              ; send (ch2, v2)

                (*
                 * Continue splitting the rest of the stream into the children
                 * threads. This phase drains all the input elements.
                 *)
              ; split (ch, ch1, ch2)

                (*
                 * All elements have been read and split; we can finally merge
                 * the two sorting threads.
                 *)
              ; merge (ch1, ch2, ch)
              end

          (*
           * Mark the end of the sorted sequence.
           *)
        ; send (ch, NONE)
        end
    in
      spawn sort
    ; ch
    end
end
