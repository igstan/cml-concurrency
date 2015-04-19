signature M_VAR =
sig
  exception Put

  type 'a mvar

  val empty : unit -> 'a mvar
  val getEvt : 'a mvar -> 'a CML.event
  val get : 'a mvar -> 'a
  val put : 'a mvar -> 'a -> unit
end
