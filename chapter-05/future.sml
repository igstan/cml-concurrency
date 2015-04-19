structure Future =
struct
  open CML SyncVar

  datatype 'a result =
    SUCCESS of 'a
  | FAILURE of exn

  fun future f a =
    let
      fun toResult () = (SUCCESS (f a)) handle e => FAILURE e
      fun fromResult (SUCCESS a) = a
        | fromResult (FAILURE e) = e
      val cell = iVar ()
    in
      spawn (fn () => iPut (cell, toResult ()))
    ; wrap (iGetEvt cell, fromResult)
    end
end
