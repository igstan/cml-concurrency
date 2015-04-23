structure Counter : COUNTER =
struct
  open CML

  fun counter start =
    let
      val ch = channel ()
      fun count i =
        let in
          send (ch, i)
        ; count (i + 1)
        end
    in
      spawnc count start
    ; ch
    end
end
