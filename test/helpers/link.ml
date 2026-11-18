type fault = Refused | Cut | Slow | Mute | Down | Trickle

let fault_name = function
  | Refused -> "refused"
  | Cut -> "cut"
  | Slow -> "slow"
  | Mute -> "mute"
  | Down -> "down"
  | Trickle -> "trickle"

type Eio.Exn.err += Link of string

let () =
  Eio.Exn.register_pp (fun ppf -> function
    | Link e ->
        Fmt.pf ppf "Link %s" e;
        true
    | _ -> false)

let error e = Eio.Exn.create (Link e)

type t = {
  mutable chaos : Eio_chaos.t;
  mutable links : int;
  mutable lasting : fault option; (* the condition of this run *)
  mutable listed_open : bool;
  mutable listed_recv : bool;
  mutable fails : int; (* receives failed in a row *)
  mutable runs : string list list; (* latest first, each latest first *)
  mutable places : int list; (* latest first *)
}

let v () =
  {
    chaos = Eio_chaos.none;
    links = 0;
    lasting = None;
    listed_open = false;
    listed_recv = false;
    fails = 0;
    runs = [];
    places = [];
  }

let down t = t.lasting = Some Down
let runs t = List.rev_map List.rev t.runs
let places t = List.rev t.places

(* What one end sends: its bytes, how many the other end received, and
   where each send ends, with its number. *)
type dir = {
  buf : Buffer.t;
  mutable read : int;
  mutable ends : (int * int) list;
}

type stream = {
  id : int;
  owner : t;
  ab : dir;
  ba : dir;
  mutable cut : bool;
  mutable muted : bool;
  mutable pending : (int * [ `A | `B ]) option; (* a cut that waits *)
  cond : Eio.Condition.t;
}

let log t key =
  match t.runs with
  | r :: rs -> t.runs <- (key :: r) :: rs
  | [] -> t.runs <- [ [ key ] ]

let ask t ?lasting ?mild ?split ~kind ~target ~instance ~structure ~offset
    ~length once =
  let op =
    {
      Eio_chaos.Op.device = "link";
      kind;
      target;
      instance;
      offset;
      length;
      detail = "";
      structure;
      changes = Never;
    }
  in
  let f =
    Eio_chaos.decide t.chaos op ~name:fault_name ?lasting ?mild ?split once
  in
  Option.iter
    (fun f ->
      log t (Fmt.str "%s:%s=%s" kind target (fault_name f));
      t.places <- Eio_chaos.place t.chaos :: t.places)
    f;
  f

let end_name = function `A -> "a" | `B -> "b"
let outgoing l = function `A -> l.ab | `B -> l.ba
let incoming l = function `A -> l.ba | `B -> l.ab

(* {1 The part} *)

module Net = struct
  type nonrec t = t
  type tag = [ `Generic ]

  let no () = raise (error "links are reached through Link")
  let listen _ ~reuse_addr:_ ~reuse_port:_ ~backlog:_ ~sw:_ _ = no ()
  let connect _ ~bind_to:_ ~options:_ ~sw:_ _ = no ()
  let datagram_socket _ ~reuse_addr:_ ~reuse_port:_ ~sw:_ _ = no ()
  let getaddrinfo _ ~service:_ _ = no ()
  let getnameinfo _ _ = no ()
end

let handler = Eio.Net.Pi.network (module Net)
let capability t = Eio_chaos.Net (Eio.Resource.T (t, handler))

module Part_ = struct
  type nonrec t = t

  let name = "link"
  let noun = "link operation"
  let pp_op = Eio_chaos.Op.pp
  let indeterminate = function End_of_file -> true | _ -> false

  let fresh t =
    t.links <- 0;
    t.listed_open <- false;
    t.listed_recv <- false;
    t.fails <- 0

  let start t ~os:_ ~sw:_ ~clock:_ ~mono_clock:_ chaos =
    t.chaos <- chaos;
    t.lasting <- None;
    t.runs <- [] :: t.runs;
    fresh t;
    capability t

  let run t ~cut:_ ~faults =
    t.lasting <-
      List.find_opt (fun f -> List.mem (fault_name f) faults) [ Down; Trickle ];
    fresh t;
    capability t

  let cut _ = []

  let restart t ~power:_ ~cut:_ =
    t.lasting <- None;
    fresh t;
    capability t
end

module P = Eio_chaos.Part (Part_)

let part t = P.v t

let get (env : Eio_chaos.env) =
  match P.find env#chaos with
  | Some t -> t
  | None -> invalid_arg "Link: env is not that of a run"

(* {1 Operations} *)

let open_ env =
  let t = get env in
  t.links <- t.links + 1;
  let id = t.links in
  let lasting =
    if t.listed_open then []
    else begin
      t.listed_open <- true;
      [ Down ]
    end
  in
  let name = string_of_int id in
  let f =
    ask t ~lasting ~kind:"open" ~target:name ~instance:name ~structure:Every
      ~offset:0 ~length:0 [ Refused ]
  in
  if f = Some Refused then raise (error "refused");
  if down t then raise (error "down");
  {
    id;
    owner = t;
    ab = { buf = Buffer.create 16; read = 0; ends = [] };
    ba = { buf = Buffer.create 16; read = 0; ends = [] };
    cut = false;
    muted = false;
    pending = None;
    cond = Eio.Condition.create ();
  }

let target l e = Fmt.str "%d%s" l.id (end_name e)

let send (env : Eio_chaos.env) l e s =
  let t = l.owner in
  if l.cut then raise End_of_file;
  let d = outgoing l e in
  let offset = Buffer.length d.buf in
  match
    ask t ~mild:[ Slow ] ~kind:"send" ~target:(target l e)
      ~instance:(string_of_int l.id) ~structure:Thin ~offset
      ~length:(String.length s) [ Cut ]
  with
  | Some Cut ->
      l.cut <- true;
      Eio.Condition.broadcast l.cond;
      raise End_of_file
  | f ->
      if f = Some Slow then Eio.Time.sleep env#clock 0.01;
      let i = Eio_chaos.index t.chaos in
      if not l.muted then begin
        Buffer.add_string d.buf s;
        d.ends <- (offset + String.length s, i) :: d.ends;
        Eio.Condition.broadcast l.cond
      end

let failed t =
  t.fails <- t.fails + 1;
  if t.fails >= 100 then
    Eio_chaos.hangs t.chaos "a receive failed 100 times in a row";
  raise End_of_file

let recv (_ : Eio_chaos.env) l e n =
  let t = l.owner in
  let d = incoming l e in
  let offset = d.read in
  (match l.pending with
  | Some (p, e') when e' = e && p <= offset ->
      l.pending <- None;
      l.cut <- true
  | _ -> ());
  if l.cut then failed t
  else
    let lasting =
      if t.listed_recv then []
      else begin
        t.listed_recv <- true;
        [ Trickle ]
      end
    in
    let trickle = t.lasting = Some Trickle in
    let f =
      ask t ~lasting ~split:trickle ~kind:"recv" ~target:(target l e)
        ~instance:(string_of_int l.id) ~structure:Thin ~offset ~length:n
        [ Cut; Mute ]
    in
    let i = Eio_chaos.index t.chaos in
    let cap =
      match (f, l.pending) with
      | Some Cut, _ when Eio_chaos.place t.chaos > 0 ->
          let k = Eio_chaos.place t.chaos in
          l.pending <- Some (offset + k, e);
          k
      | Some Cut, _ ->
          l.cut <- true;
          failed t
      | Some Mute, _ ->
          l.muted <- true;
          n
      | _, Some (p, e') when e' = e -> p - offset
      | _ -> n
    in
    let cap = if trickle then 1 else cap in
    let rec wait () =
      if l.cut then failed t
      else if l.muted || Buffer.length d.buf = d.read then begin
        Eio.Condition.await_no_mutex l.cond;
        wait ()
      end
    in
    Eio_chaos.inside t.chaos "recv" wait;
    let k = min cap (Buffer.length d.buf - d.read) in
    let s = Buffer.sub d.buf d.read k in
    d.read <- d.read + k;
    t.fails <- 0;
    Eio_chaos.places t.chaos i
      (List.filter_map
         (fun (e, j) ->
           if offset <= e && e < offset + max k 1 then Some (e - offset, j)
           else None)
         (List.rev d.ends));
    s
