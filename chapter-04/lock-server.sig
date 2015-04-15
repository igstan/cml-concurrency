signature LOCK_SERVER =
sig
  type lock
  type server

  val create : unit -> server
  val produceLock : server -> lock
  val releaseLock : server -> lock -> unit
  val acquireLockEvt : server -> lock -> unit CML.event
end
