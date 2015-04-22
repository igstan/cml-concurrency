(**
 * A single producer, multi-consumer buffered channel. Uses `Mailbox` internally
 * to buffer and distribute messages to consumers.
 *
 * Based on listing 5.7 in the book.
 *)
structure MailboxMulticast :> MULTICAST =
struct
  open CML

  structure MBox = Mailbox

  datatype 'a request =
    Message of 'a
  | NewPort

  datatype 'a port = Port of 'a MBox.mbox

  datatype 'a mchan = MChan of 'a request chan * 'a port chan

  fun mChannel () =
    let
      (**
       * Create a new port whose previous port is represented by `outFn`,
       * which is a function that sends a message to the previous port.
       *)
      fun mkPort outFn =
        let
          (*
           * Allocate a mailbox for this port's listener. We need a mailbox
           * because listener consumption rates are independent of each other,
           * and also independent from the rate of production, which means
           * that we need a way to buffer incoming messages.
           *)
          val mbox = MBox.mailbox ()

          (*
           * A channel where messages sent to this port are pushed.
           *)
          val inCh = channel ()

          (*
           * Create a thread which consumes published messages and forwards
           * them to this port's listener and to the next port in the chain.
           *)
          fun tee () =
            let
              val m = recv inCh
            in
              (* Send the message to this port's listener. *)
              MBox.send (mbox, m)
              (* Send the message to the next chained port. *)
            ; outFn m
              (* Listen for the next message. *)
            ; tee ()
            end
        in
          spawn tee
        ; (fn msg => send (inCh, msg), Port mbox)
        end

      val reqCh = channel ()
      val replyCh = channel ()

      (**
       * The server encapsulating the multicast state. The `outFn` param
       * is the state of the server and is a function that can send a
       * message to the first port in the chain of ports.
       *
       * The chain of ports is akin to a linked list, where the links are
       * represented by such `outFn` functions. The server pushes received
       * message to the head port, which is then responsible for pushing
       * the message further along the chain.
       *)
      fun server outFn =
        case recv reqCh of
          Message m => (outFn m ; server outFn)
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

  fun recvEvt (Port mbox) = MBox.recvEvt mbox
end
