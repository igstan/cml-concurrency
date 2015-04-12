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
   *   spawn (fn _ => send (chan, "Hello!"))
   * ; chan
   * end
   * ```
   *)
  fun spawnThread thread =
    let
      val chan = channel ()
    in
      spawn (fn _ => thread chan)
    ; chan
    end
end
