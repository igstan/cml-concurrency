structure IDService =
struct
  open CML

  fun create () =
    let
      val ch = channel ()
      fun loop i = (send (ch, i) ; loop (i + 1))
    in
      spawnc loop 0
    ; fn () => recv ch
    end
end
