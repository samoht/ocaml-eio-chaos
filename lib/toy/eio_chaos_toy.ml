open! Hermetic

type fault = Timeout | Refused

let fault_name = function Timeout -> "timeout" | Refused -> "refused"

let pp_fault ppf = function
  | Timeout -> Fmt.string ppf "times out"
  | Refused -> Fmt.string ppf "is refused"

type Eio.Exn.err += Toy of string

let () =
  Eio.Exn.register_pp (fun ppf -> function
    | Toy e ->
        Fmt.pf ppf "Toy %s" e;
        true
    | _ -> false)

type t = { mutable chaos : Eio_chaos.t }

module Net = struct
  type nonrec t = t
  type tag = [ `Generic ]

  let refuse () = raise (Eio.Exn.create (Toy "not supported"))
  let listen _ ~reuse_addr:_ ~reuse_port:_ ~backlog:_ ~sw:_ _ = refuse ()
  let connect _ ~bind_to:_ ~options:_ ~sw:_ _ = refuse ()
  let datagram_socket _ ~reuse_addr:_ ~reuse_port:_ ~sw:_ _ = refuse ()
  let getnameinfo _ _ = refuse ()

  let getaddrinfo t ~service:_ host =
    let op = Eio_chaos.Op.v ~device:"dns" ~kind:"lookup" ~changes:Never host in
    match
      Eio_chaos.decide t.chaos op ~name:fault_name ~pp:pp_fault
        [ Timeout; Refused ]
    with
    | Some f -> raise (Eio.Exn.create (Toy (fault_name f)))
    | None ->
        Eio_chaos.spend t.chaos Mtime.Span.(1 * ms);
        [ `Tcp (Eio.Net.Ipaddr.V4.loopback, 80) ]
end

let net t = Eio_chaos.Net (Eio.Resource.T (t, Eio.Net.Pi.network (module Net)))

module Device = Eio_chaos.Device (struct
  type nonrec t = t

  let name = "dns"
  let noun = "lookup"

  let start t ~os:_ ~sw:_ ~clock:_ ~mono_clock:_ chaos =
    t.chaos <- chaos;
    net t

  let run t ~cut:_ ~faults:_ = net t
  let cut _ = []
  let restart t ~power:_ ~cut:_ = net t
  let pp_op = Eio_chaos.Op.pp
  let indeterminate _ = false
end)

let resolver () = Device.v { chaos = Eio_chaos.none }
