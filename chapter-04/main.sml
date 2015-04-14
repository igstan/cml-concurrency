structure Chapter04 =
struct
  open Fn infix 1 |>
  open CML Show

  val schedulingQuantum = SOME (Time.fromMilliseconds 10)

  fun mergeSort () =
    let
      val numbers = [4, 3, 2, 1, 5, 6, 7, 8]

      fun sortList numbers =
        let
          val sorter = MergeSort.mergeSort ()
        in
          List.app (fn i => send (sorter, SOME i)) numbers
        ; send (sorter, NONE)
        ; Stream.takeList (length numbers) sorter
        end

      fun entry () =
        sortList numbers |> list (option int) |> println
    in
      RunCML.doit (entry, schedulingQuantum)
    end
end
