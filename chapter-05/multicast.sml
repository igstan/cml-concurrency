structure Multicast :> MULTICAST =
struct
  open CML

  structure MB = Mailbox

  datatype 'a request =
    Message of 'a
  | NewPort

  datatype 'a port = Port of 'a MB.mbox

  datatype 'a mchan = MChan of 'a request chan * 'a port chan

  fun mChannel () =
    let
      val reqCh = channel ()
      val replyCh = channel ()

      fun mkPort outFn =
        let
          val mbox = MB.mailbox ()
          val inCh = channel ()
          fun tee () =
            let
              val m = recv inCh
            in
              MB.send (mbox, m)
            ; outFn m
            ; tee ()
            end
        in
          spawn tee
        ; (fn msg => send (inCh, msg), Port mbox)
        end

      fun server outFn =
        case recv reqCh of
          Message m => (outFn m; server outFn)
        | NewPort =>
          let
            val (outFn', port) = mkPort outFn
          in
            send (replyCh, port)
          ; server outFn'
          end
    in
      spawn (fn () => server ignore)
    ; MChan (reqCh, replyCh)
    end

  fun port (MChan (reqCh, replyCh)) =
    let in
      send (reqCh, NewPort)
    ; recv replyCh
    end

  fun multicast (MChan (reqCh, _), m) = send (reqCh, Message m)

  fun recvEvt (Port mbox) = MB.recvEvt mbox
end
