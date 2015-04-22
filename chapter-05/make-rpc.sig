structure RPC =
struct
  (**
   * Represents a single server operation.
   *
   *  - 'a denotes the server state
   *  - 'b denotes the input of the operation
   *  - 'c denotes the output of the operation
   *)
  datatype ('a, 'b, 'c) proc =
    Proc of {
      (** The function used by the client. *)
      client : 'b -> 'c,
      (** The function used by the server to advance its state. *)
      serverEvt : 'a -> 'a CML.event
    }
end

(**
 * Based on listing 5.9 in the book.
 *)
signature MAKE_RPC =
sig
  (**
   * Creates a new server operation.
   *
   * The first argument is a function which takes the server's state and the
   * operation input and returns a new server state and the operation's output.
   *)
  val mkRPC : (('a * 'b) -> ('a * 'c)) -> ('a, 'b, 'c) RPC.proc

  (**
   * Creates a server given an initial state and a list of server operations.
   *)
  val mkServer : 'a -> ('a -> 'a CML.event) list -> unit
end
