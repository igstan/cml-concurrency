(**
 * A single producer, multi-consumer buffered channel using functional streams.
 * The streams are implemented on top of `SyncVar.ivar`.
 *
 * This implementation, as opposed to the Mailbox-based one, does not suffer
 * of space leaks. The Mailbox-based one will continue to keep accumulate
 * messages in a Mailbox even when a port's listener is no longer interested
 * in that port. Using functional streams we can use a single stream for
 * multiple ports, but each port will have to keep track of its position in
 * stream. Additionally, stream elements that have been read are subject to
 * garbage collection.
 *
 * Based on listing 5.8 in the book.
 *)
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
          (*
           * A channel from which this port's listener can read.
           *)
          val outCh = channel ()

          (*
           * Create a thread which consumes published messages and forwards
           * them to this port's listener and to the next port in the chain.
           *)
          fun tee iv =
            let
              (* Read the value of the current position in the stream. *)
              val MCState (v, nextIV) = SyncVar.iGet iv
            in
              (* Send the value to this port's listener. *)
              send (outCh, v)
              (* Read the next value in the stream. *)
            ; tee nextIV
            end
        in
          spawn (fn () => tee iv)
        ; Port outCh
        end

      (**
       * The state of the server, i.e. `iv`, is the head of a stream and is
       * represented by an I-variable holding the value at the head of the
       * stream and the tail of the stream, which is again an I-variable.
       *
       * Each created port is given a reference to the stream, and the ports
       * themselves hold the state of where that port's listener is.
       *
       * When a new message is pushed, the server state changes by pushing
       * a new element at the end of the stream.
       *)
      fun server iv =
        case recv reqCh of
          NewPort => (send (replyCh, mkPort iv) ; server iv)
        | Message m =>
          let
            (* New stream's tail. *)
            val nextIV = SyncVar.iVar ()
          in
            (*
             * Assign the message to the current tail of the stream and
             * create a new tail for the stream, which will be populated by
             * subsequence messages.
             *)
            SyncVar.iPut (iv, MCState (m, nextIV))
            (* Listen for the next message using the new stream. *)
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
