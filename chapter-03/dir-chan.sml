structure DirChan : DIR_CHAN =
struct
  datatype 'a in_chan = IN of 'a CML.chan
  datatype 'a out_chan = OUT of 'a CML.chan

  fun channel () =
    let
      val ch = CML.channel ()
    in
      (IN ch, OUT ch)
    end

  fun recv (IN ch) = CML.recv ch
  fun send (OUT ch, a) = CML.send (ch, a)

  fun recvEvt (IN ch) = CML.recvEvt ch
  fun sendEvt (OUT ch, a) = CML.sendEvt (ch, a)
end
