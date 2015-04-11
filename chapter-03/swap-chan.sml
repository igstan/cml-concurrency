(*
 * Listing 3.8 in the book, with small changes.
 *)
structure SwapChan : SWAP_CHAN =
struct
  open CML

  type 'a swap_chan = ('a * 'a chan) chan

  fun channel () = CML.channel ()

  fun swap (chan, msgOut) =
    let
      fun action () =
        let
          val inChan = CML.channel ()
        in
          choose [
            wrap (
              recvEvt chan,
              fn (msgIn, outChan) => (send (outChan, msgOut); msgIn)
            ),
            wrap (
              sendEvt (chan, (msgOut, inChan)),
              fn () => recv inChan
            )
          ]
        end
    in
      guard action
    end
end
