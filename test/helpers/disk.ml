type fault = Eio_error | Full | Cut

let fault_name = function Eio_error -> "eio" | Full -> "full" | Cut -> "cut"

type Eio.Exn.err += Disk of string

let () =
  Eio.Exn.register_pp (fun ppf -> function
    | Disk e ->
        Fmt.pf ppf "Disk %s" e;
        true
    | _ -> false)

let error e = Eio.Exn.create (Disk e)

type t = {
  mutable chaos : Eio_chaos.t;
  mutable cache : (string * string) list;
  mutable durable : (string * string) list;
  mutable promised : (string * string) list;
  mutable gen : int; (* a capability of an older generation is dead *)
  ranks : (string * string, int) Hashtbl.t;
  mutable runs : string list list; (* latest first, each latest first *)
  mutable restarts : bool list; (* latest first *)
}

let v () =
  {
    chaos = Eio_chaos.none;
    cache = [];
    durable = [];
    promised = [];
    gen = 0;
    ranks = Hashtbl.create 8;
    runs = [];
    restarts = [];
  }

let runs t = List.rev_map List.rev t.runs
let restarts t = List.rev t.restarts

let ack t name =
  match List.assoc_opt name t.cache with
  | Some v -> t.promised <- (name, v) :: List.remove_assoc name t.promised
  | None -> invalid_arg name

let norm p =
  if String.starts_with ~prefix:"/" p then String.sub p 1 (String.length p - 1)
  else p

(* Asks the run whether [kind] on [target] fails, and logs the answer. *)
let ask (t, gen) kind target ~length allowed =
  if gen <> t.gen then raise (error "the power was cut");
  let at =
    1 + Option.value ~default:0 (Hashtbl.find_opt t.ranks (kind, target))
  in
  Hashtbl.replace t.ranks (kind, target) at;
  let op = { Eio_chaos.Op.device = "fs"; kind; target; at; length } in
  let f = Eio_chaos.decide t.chaos op ~name:fault_name allowed in
  Option.iter
    (fun f ->
      let key = Fmt.str "fs:%s:%s@%d=%s" kind target at (fault_name f) in
      match t.runs with
      | r :: rs -> t.runs <- (key :: r) :: rs
      | [] -> t.runs <- [ [ key ] ])
    f;
  f

module Dir = struct
  type nonrec t = t * int

  let no _ = raise (error "not supported")
  let open_in d ~sw:_ ~follow:_ _ = no d
  let open_out d ~sw:_ ~follow:_ ~append:_ ~create:_ _ = no d
  let mkdir d ~perm:_ _ = no d
  let open_subtree d ~sw:_ _ = no d
  let with_dir_entries d _ _ = no d
  let stat d ~follow:_ _ = no d
  let rmdir d _ = no d
  let rename d _ _ _ = no d
  let chmod d ~follow:_ ~perm:_ _ = no d
  let chown ~follow:_ ?uid:_ ?gid:_ d _ = no d
  let unlink d _ = no d
  let pp ppf _ = Fmt.string ppf "<disk>"
  let native _ _ = None

  let split p =
    match String.rindex_opt p '/' with
    | None -> if p = "" then None else Some ("", p)
    | Some i ->
        Some (String.sub p 0 i, String.sub p (i + 1) (String.length p - i - 1))

  let join a b = if a = "" then b else a ^ "/" ^ b

  let symlink ~link_to ((t, _) as d) path =
    let k = norm path in
    match
      ask d "write" k ~length:(String.length link_to) [ Eio_error; Full ]
    with
    | Some f -> raise (error (fault_name f))
    | None -> t.cache <- (k, link_to) :: List.remove_assoc k t.cache

  let read_link ((t, _) as d) path =
    let k = norm path in
    match ask d "read" k ~length:0 [ Eio_error ] with
    | Some f -> raise (error (fault_name f))
    | None -> (
        match List.assoc_opt k t.cache with
        | Some v -> v
        | None -> raise (error ("no " ^ k)))

  let sync_dir ((t, _) as d) _ =
    match ask d "sync" "/" ~length:0 [ Eio_error; Cut ] with
    | Some Cut -> Eio_chaos.power_cut t.chaos
    | Some f -> raise (error (fault_name f))
    | None -> t.durable <- t.cache

  let read_dir ((t, _) as d) _ =
    ignore (ask d "list" "/" ~length:0 [] : fault option);
    List.sort compare (List.map fst t.cache)
end

let handler = Eio.Resource.handler [ H (Eio.Fs.Pi.Dir, (module Dir)) ]

let capability t =
  let d : Eio.Fs.dir_ty Eio.Path.t =
    (Eio.Resource.T ((t, t.gen), handler), "")
  in
  Eio_chaos.Fs (d, d)

module Part = struct
  type nonrec t = t

  let name = "fs"

  let start t ~os:_ ~sw:_ ~clock:_ ~mono_clock:_ chaos =
    t.chaos <- chaos;
    t.cache <- [];
    t.durable <- [];
    t.promised <- [];
    t.gen <- t.gen + 1;
    Hashtbl.reset t.ranks;
    t.runs <- [] :: t.runs;
    capability t

  let restart t ~power =
    t.restarts <- power :: t.restarts;
    if power then begin
      t.cache <- t.durable;
      t.gen <- t.gen + 1;
      List.iter
        (fun (k, v) ->
          if List.assoc_opt k t.durable <> Some v then
            Eio_chaos.lost t.chaos
              (Fmt.str "%s held %S when acknowledged, and is now %s" k v
                 (match List.assoc_opt k t.durable with
                 | None -> "missing"
                 | Some w -> Fmt.str "%S" w)))
        (List.rev t.promised)
    end;
    t.promised <- [];
    capability t
end

let part t =
  let module P = Eio_chaos.Part (Part) in
  P.v t
