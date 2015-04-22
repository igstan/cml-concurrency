structure StreamMulticast :> MULTICAST =
struct
  open CML

  datatype 'a request =
    Message of 'a
  | NewPort

  datatype 'a port = Port of 'a chan

  datatype 'a mchan = MChan of 'a request chan * 'a port chan

  datatype 'a mc_state = MCState of 'a * 'a mc_state SyncVar.ivar

  fun mChannel () =
    let
      val reqCh = channel ()
      val replyCh = channel ()

      fun mkPort iv =
        let
          val outCh = channel ()
          fun tee iv =
            let
              val MCState (v, nextIV) = SyncVar.iGet iv
            in
              send (outCh, v)
            ; tee nextIV
            end
        in
          spawn (fn () => tee iv)
        ; Port outCh
        end

      fun server iv =
        case recv reqCh of
          NewPort => (send (replyCh, mkPort iv) ; server iv)
        | Message m =>
          let
            val nextIV = SyncVar.iVar ()
          in
            SyncVar.iPut (iv, MCState (m, nextIV))
          ; server nextIV
          end
    in
      spawn (fn () => server (SyncVar.iVar ()))
    ; MChan (reqCh, replyCh)
    end

  fun port (MChan (reqCh, replyCh)) =
    let in
      send (reqCh, NewPort)
    ; recv replyCh
    end

  fun multicast (MChan (reqCh, _), m) = CML.send (reqCh, Message m)

  fun recvEvt (Port chan) = CML.recvEvt chan
end
