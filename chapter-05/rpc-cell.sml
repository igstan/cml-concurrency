structure RPCCell :> CELL =
struct
  open RPC MakeRPC

  datatype 'a cell = Cell of { get : unit -> 'a, put : 'a -> unit }

  fun cell initValue =
    let
      val Proc get = mkRPC (fn (x, _) => (x, x))
      val Proc put = mkRPC (fn (_, x) => (x, ()))
    in
      mkServer initValue [#entryEvt get, #entryEvt put]
    ; Cell { get = #call get, put = #call put }
    end

  fun get (Cell { get, ... }) = get ()

  fun put (Cell { put, ... }) a = put a
end
