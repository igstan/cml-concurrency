structure Main =
struct
  open CML
  open TextIO

  val schedulingQuantum = SOME (Time.fromMilliseconds 10)

  fun cells () =
    let
      fun entry () =
        let
          val cell = Cell.cell 0
        in
          Cell.put cell 1;
          Cell.put cell 2;
          Cell.put cell 3;
          Cell.put cell 4;
          print ("cell: "^ Int.toString (Cell.get cell) ^"\n")
        end
    in
      RunCML.doit (entry, schedulingQuantum)
    end
end
