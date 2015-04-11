structure Chapter04 =
struct
  open CML

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
        let
          fun println s = print (s ^ "\n")
          val printNumber = println o Int.toString o valOf
        in
          List.app printNumber (sortList numbers)
        end
    in
      RunCML.doit (entry, schedulingQuantum)
    end
end
