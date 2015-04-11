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

  fun counters () =
    let
      fun entry () =
        let
          val c = Counter.counter 0
          val a = recv c
          val b = recv c
          val c = recv c
        in
          print ("sum: "^ Int.toString (a + b + c) ^"\n")
        end
    in
      RunCML.doit (entry, schedulingQuantum)
    end

  fun primes () =
    let
      fun entry () =
        let
          val ps = Sieve.primes 100
        in
          List.app (fn p => print ("prime: "^ Int.toString p ^"\n")) ps
        end
    in
      RunCML.doit (entry, schedulingQuantum)
    end

  fun streams () =
    let
      fun entry () =
        let
          val nums = Stream.iterate (fn i => i + 1) 0
          val odds = Stream.filter (fn a => (a mod 2) = 0) nums
          val res = Stream.take 10 odds
        in
          List.app (fn p => print ("num: "^ Int.toString p ^"\n")) res
        end
    in
      RunCML.doit (entry, schedulingQuantum)
    end
end
