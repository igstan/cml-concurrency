signature STREAM =
sig
  type 'a stream

  (**
   * Build a stream of `'a` elements, computed from the previous value.
   *)
  val iterate : ('a -> 'a) -> 'a -> 'a stream

  (**
   * Consume a finite amount of elements from a stream and put them in a list.
   *)
  val take : int -> 'a stream -> 'a list

  (**
   * Same as `List.filter`, only on streams.
   *)
  val filter : ('a -> bool) -> 'a stream -> 'a stream
end
