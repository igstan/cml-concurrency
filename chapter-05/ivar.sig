(**
 * Listing 5.2 in the book.
 *)
signature I_VAR =
sig
  exception Put

  type 'a ivar

  val iVar : unit -> 'a ivar
  val iPut : 'a ivar -> 'a -> unit
  val iGet : 'a ivar -> 'a
  val iGetEvt : 'a ivar -> 'a CML.event
  val iGetPoll : 'a ivar -> 'a option
  val sameIVar : ('a ivar * 'a ivar) -> bool
end
