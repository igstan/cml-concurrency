(*
 * Listing 5.4 in the book.
 *)
signature BUFFER_CHAN =
sig
  type 'a buffer_chan

  val bufferChannel : unit -> 'a buffer_chan
  val send : ('a buffer_chan * 'a) -> unit
  val recvEvt : 'a buffer_chan -> 'a CML.event
end
