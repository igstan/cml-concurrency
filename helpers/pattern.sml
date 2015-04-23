(**
 * Common patterns of CML usage encoded as functions.
 *)
structure Pattern =
struct
  open CML

  (**
   * Spawns a new thread with a single output channel. It saves client code
   * from creating the channel and having the sequential composition int the
   * let body. Consider this shorter version:
   *
   * ```sml
   * spawnThread (fn chan => send (chan, "Hello!"))
   * ```
   *
   * Versus the normal, longer one:
   *
   * ```sml
   * let
   *   val chan = channel ()
   * in
   *   spawn (fn () => send (chan, "Hello!"))
   * ; chan
   * end
   * ```
   *)
  fun spawnThread thread =
    let
      val chan = channel ()
    in
      spawnc thread chan
    ; chan
    end

  (**
   * A stateful infinite loop. The next state is computed from the previous
   * one by `f`. An initial state must be supplied.
   *
   * Similar in idea to the following infinite list, where `x` is the initial
   * state.
   *
   * ```
   * [x, f x, f (f x), f (f (f x)), f (f (f (f x))), ... ]
   * ```
   *)
  fun forever initial f =
    let
      fun loop state = loop (f state)
    in
      ignore (spawnc loop initial)
    end

  (**
   * A stateless infinite loop, i.e., it has no intermediate states.
   *)
  fun repeat f = forever () f

  (**
   * Nondeterministically synchronize on two events and then apply `f` to the
   * produced values.
   *)
  fun combine f (evA, evB) =
    select [
      wrap (evA, fn a => f (a, sync evB)),
      wrap (evB, fn b => f (sync evA, b))
    ]
end
