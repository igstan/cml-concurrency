structure Chapter05 =
struct
  open Fn infix 1 |>
  open CML Show

  val schedulingQuantum = SOME (Time.fromMilliseconds 10)

  fun mVariables () =
    let
      fun entry () =
        let
          val m = MVar.empty ()
        in
          MVar.put m 42
        ; MVar.get m |> int |> println
        end handle
          Put => println "MVar Put Exception"
    in
      RunCML.doit (entry, schedulingQuantum)
    end
end
