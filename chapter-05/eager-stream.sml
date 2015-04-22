structure EagerStream =
struct
  open CML

  datatype 'a stream =
    NIL
  | HD of 'a * 'a stream event

  type 'a stream_elem = 'a stream SyncVar.ivar

  fun stream () =
    let
      val iv = SyncVar.iVar ()
    in
      (iv, SyncVar.iGetEvt iv)
    end

  fun extendStream iv v =
    let
      val iv' = SyncVar.iVar ()
    in
      SyncVar.iPut (iv, HD (v, SyncVar.iGetEvt iv'))
    ; iv'
    end

  fun terminateStream iv = SyncVar.iPut (iv, NIL)

  fun take n stream =
    let
      fun loop (0, _, result) = rev result
        | loop (_, NIL, result) = rev result
        | loop (i, HD (head, tail), result) = loop (i - 1, sync tail, head :: result)
    in
      loop (n, sync stream, [])
    end

  fun fromTo i j =
    let
      val (iv, strm) = stream ()
      fun loop (iv, i) =
        if i < j
        then loop (extendStream iv i, i + 1)
        else terminateStream iv
    in
      spawn (fn _ => loop (iv, i))
    ; strm
    end
end
