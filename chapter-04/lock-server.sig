signature LOCK_SERVER =
sig
  type lock
  type server

  val create : unit -> server
  val produceLock : server -> lock
  val releaseLock : lock -> unit
  val acquireLockEvt : lock -> unit CML.event
end
