structure Chapter04 =
struct
  open Fn infix 1 |>
  open CML Show

  val schedulingQuantum = SOME (Time.fromMilliseconds 10)

  fun mergeSort numbers =
    let
      fun sortList numbers =
        let
          val sorter = MergeSort.mergeSort ()
        in
          (* send the numbers to sort *)
          List.app (fn i => send (sorter, SOME i)) numbers

          (* mark the end of the stream *)
        ; send (sorter, NONE)

          (* get the sorted numbers back *)
        ; Stream.takeList (length numbers) sorter
        end

      fun entry () =
        sortList numbers |> list (option int) |> println
    in
      RunCML.doit (entry, schedulingQuantum)
    end

  fun idService () =
    let
      fun entry () =
        let
          val srvc = IDService.create ()
          val ids = List.tabulate (5, fn _ => srvc ())
        in
          ids |> list int |> println
        end
    in
      RunCML.doit (entry, schedulingQuantum)
    end
end
