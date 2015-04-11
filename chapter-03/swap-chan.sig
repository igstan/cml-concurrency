signature SWAP_CHAN =
sig
  type 'a swap_chan

  val channel : unit -> 'a swap_chan
  val swap : ('a swap_chan * 'a) -> 'a CML.event
end
