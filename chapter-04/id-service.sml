structure IDService =
struct
  open CML

  fun mkUIdSrc () =
    let
      val ch = channel ()
      fun loop i = (send (ch, i) ; loop (i + 1))
    in
      spawn (fn () => loop 0)
    ; fn () => recv ch
    end
end
