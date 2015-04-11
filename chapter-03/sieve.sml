structure Sieve =
struct
  open CML

  (**
   * Produces an int stream where multiples of `prime` are removed.
   *)
  fun filter prime inChan =
    let
      val outChan = channel ()

      fun loop () =
        let
          val i = recv inChan
        in
          if (i mod prime) <> 0 then send (outChan, i) else ()
        ; loop ()
        end
    in
      spawn loop;
      outChan
    end

  fun sieve () =
    let
      val primes = channel ()
      fun head chan =
        let
          val prime = recv chan
        in
          send (primes, prime);
          head (filter prime chan)
        end
    in
      spawn (fn () => head (Counter.counter 2));
      primes
    end

  fun primes n =
    let
      val chan = sieve ()
      fun loop 0 result = rev result
        | loop i result = loop (i - 1) ((recv chan) :: result)
    in
      loop n []
    end
end
