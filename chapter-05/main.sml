structure Chapter05 =
struct
  open Fn infix 1 |>
  open CML Show

  val schedulingQuantum = SOME (Time.fromMilliseconds 10)
end
