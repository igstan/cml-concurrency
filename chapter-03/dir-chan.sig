(*
 * Listing 3.5 in the book.
 *)
signature DIR_CHAN =
sig
  type 'a in_chan
  type 'a out_chan

  val channel : unit -> ('a in_chan * 'a out_chan)

  val recv : 'a in_chan -> 'a
  val send : ('a out_chan * 'a) ->unit

  val recvEvt : 'a in_chan -> 'a CML.event
  val sendEvt : ('a out_chan * 'a) -> unit CML.event
end
