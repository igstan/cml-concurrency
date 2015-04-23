structure Stream :> STREAM =
struct
  open CML

  type 'a stream = 'a chan

  fun iterate producer start =
    let
      val ch = channel ()
      fun count prev =
        let in
          send (ch, prev)
        ; count (producer prev)
        end
    in
      spawnc count start
    ; ch
    end

  fun takeList n stream =
    let
      fun loop 0 result = rev result
        | loop i result = loop (i - 1) ((recv stream) :: result)
    in
      loop n []
    end

  fun filter predicate inChan =
    let
      val outChan = channel ()

      fun loop () =
        let
          val elem = recv inChan
        in
          if predicate elem then send (outChan, elem) else ()
        ; loop ()
        end
    in
      spawn loop
    ; outChan
    end
end
