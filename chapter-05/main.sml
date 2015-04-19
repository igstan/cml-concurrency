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

  fun iVariables () =
    let
      open IVar
      fun entry () =
        let
          val i = iVar ()
        in
          iPut i 42
        ; iGet i |> int |> println
        ; iGet i |> int |> println
        ; iPut i 43 handle Put => println "IVar Put Exception"
        end
    in
      RunCML.doit (entry, schedulingQuantum)
    end
end
