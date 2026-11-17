type fault = Timeout | Refused

let fault_name = function Timeout -> "timeout" | Refused -> "refused"

type Eio.Exn.err += Dns of string

let () =
  Eio.Exn.register_pp (fun ppf -> function
    | Dns e ->
        Fmt.pf ppf "Dns %s" e;
        true
    | _ -> false)

let error e = Eio.Exn.create (Dns e)

type t = {
  mutable chaos : Eio_chaos.t;
  mutable runs : string list list; (* latest first, each latest first *)
  mutable restarts : bool list; (* latest first *)
}

let v () = { chaos = Eio_chaos.none; runs = []; restarts = [] }
let runs t = List.rev_map List.rev t.runs
let restarts t = List.rev t.restarts

module Net = struct
  type nonrec t = t
  type tag = [ `Generic ]

  let no () = raise (error "not supported")
  let listen _ ~reuse_addr:_ ~reuse_port:_ ~backlog:_ ~sw:_ _ = no ()
  let connect _ ~bind_to:_ ~options:_ ~sw:_ _ = no ()
  let datagram_socket _ ~reuse_addr:_ ~reuse_port:_ ~sw:_ _ = no ()
  let getnameinfo _ _ = no ()

  let getaddrinfo t ~service:_ host =
    let op =
      {
        Eio_chaos.Op.device = "net";
        kind = "resolve";
        target = host;
        offset = 0;
        length = 0;
        detail = "";
        structure = Every;
        changes = Never;
      }
    in
    match Eio_chaos.decide t.chaos op ~name:fault_name [ Timeout; Refused ] with
    | Some f ->
        let key = Fmt.str "resolve:%s=%s" host (fault_name f) in
        (match t.runs with
        | r :: rs -> t.runs <- (key :: r) :: rs
        | [] -> t.runs <- [ [ key ] ]);
        raise (error (fault_name f))
    | None -> [ `Tcp (Eio.Net.Ipaddr.V4.loopback, 80) ]
end

let handler = Eio.Net.Pi.network (module Net)
let capability t = Eio_chaos.Net (Eio.Resource.T (t, handler))

module Part = struct
  type nonrec t = t

  let name = "net"
  let noun = "network operation"
  let pp_op = Eio_chaos.Op.pp

  let start t ~os:_ ~sw:_ ~clock:_ ~mono_clock:_ chaos =
    t.chaos <- chaos;
    t.runs <- [] :: t.runs;
    capability t

  let run t ~cut:_ ~faults:_ = capability t
  let cut _ = []

  let restart t ~power ~cut:_ =
    t.restarts <- power :: t.restarts;
    capability t
end

let part t =
  let module P = Eio_chaos.Part (Part) in
  P.v t
