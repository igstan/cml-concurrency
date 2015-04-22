signature MULTICAST =
sig
  type 'a mchan
  type 'a port

  val mChannel : unit -> 'a mchan
  val port : 'a mchan -> 'a port
  val multicast : ('a mchan * 'a) -> unit
  val recvEvt : 'a port -> 'a CML.event
end
