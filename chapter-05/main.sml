structure Chapter05 =
struct
  open Fn infix 1 |>
  open CML Show

  val schedulingQuantum = SOME (Time.fromMilliseconds 10)

  fun mVariables () =
    let
      fun entry () =
        let
          val m = MVar.empty ()
        in
          MVar.put m 42
        ; MVar.get m |> int |> println
        ; MVar.put m 43
        ; MVar.put m 44 handle Put => println "MVar Put Exception"
        end
    in
      RunCML.doit (entry, schedulingQuantum)
    end

  fun iVariables () =
    let
      open IVar
      fun entry () =
        let
          val i = iVar ()
        in
          iPut i 42
        ; iGet i |> int |> println
        ; iGet i |> int |> println
        ; iPut i 43 handle Put => println "IVar Put Exception"
        end
    in
      RunCML.doit (entry, schedulingQuantum)
    end

  fun streams () =
    let
      fun entry () =
        let
          val strm = EagerStream.fromTo 0 9
          val nums = EagerStream.take 10 strm
        in
          list int nums |> println
        end
    in
      RunCML.doit (entry, schedulingQuantum)
    end

  local
    structure Multicast = MailboxMulticast
    (* structure Multicast = StreamMulticast *)
  in
    fun multicast () =
      let
        fun entry () =
          let
            val mchan = Multicast.mChannel ()
            fun child () =
              Multicast.port mchan
              |> Multicast.recvEvt
              |> sync
              |> int
              |> println
          in
            spawn child
          ; spawn child
          ; spawn child
          ; Multicast.multicast (mchan, 42)
          end
      in
        RunCML.doit (entry, schedulingQuantum)
      end
  end

  fun cells () =
    let
      fun entry () =
        let
          open RPCCell
          val cell = cell 0
        in
          put cell 1
        ; put cell 2
        ; put cell 3
        ; put cell 4
        ; print ("cell: "^ Int.toString (get cell) ^"\n")
        end
    in
      RunCML.doit (entry, schedulingQuantum)
    end
end
