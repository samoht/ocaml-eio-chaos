type fault = Eio_error | Full | Full_from | Evict

let fault_name = function
  | Eio_error -> "eio"
  | Full -> "full"
  | Full_from -> "full-from"
  | Evict -> "evict"

let pp_fault ppf = function
  | Eio_error -> Fmt.string ppf "fails with EIO"
  | Full -> Fmt.string ppf "fails with ENOSPC"
  | Full_from -> Fmt.string ppf "the disk is full"
  | Evict -> Fmt.string ppf "fails with EIO, its cache dropped"

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
  mutable keep : Eio_chaos.keep option;
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
    keep = None;
    runs = [];
    restarts = [];
  }

let runs t = List.rev_map List.rev t.runs
let restarts t = List.rev t.restarts
let op_time = Mtime.Span.(1 * us)
let sync_time = Mtime.Span.(1 * ms)

let norm p =
  if String.starts_with ~prefix:"/" p then String.sub p 1 (String.length p - 1)
  else p

let pp_op ppf op =
  Fmt.pf ppf "%s %s" (Eio_chaos.Op.kind op) (Eio_chaos.Op.target op)

(* Asks the run whether [kind] on [target] fails, logs the answer, and takes
   the operation's time if it does not. *)
let ask (t, gen) ?(time = op_time) ?lasting ?durable ?alone ~structure ~changes
    kind target ~length once =
  if gen <> t.gen then raise (error "the power was cut");
  let op =
    Eio_chaos.Op.v ~length ~structure ~device:"fs" ~kind ~changes target
  in
  let f =
    Eio_chaos.decide t.chaos op ~name:fault_name ~pp:pp_fault ?lasting ?durable
      ?alone once
  in
  (match f with
  | Some f ->
      let key = Fmt.str "%s:%s=%s" kind target (fault_name f) in
      t.runs <-
        (match t.runs with r :: rs -> (key :: r) :: rs | [] -> [ [ key ] ])
  | None -> Eio_chaos.spend t.chaos time);
  f

let fail = function
  | Some (Eio_error | Evict) -> raise (error "eio")
  | Some (Full | Full_from) -> raise (error "full")
  | None -> ()

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
    fail
      (ask d ~structure:Thin ~changes:Always ~lasting:[ Full_from ]
         ~durable:[ Eio_error ] "write" k ~length:(String.length link_to)
         [ Eio_error; Full ]);
    t.cache <- (k, link_to) :: List.remove_assoc k t.cache

  let read_link ((t, _) as d) path =
    let k = norm path in
    fail (ask d ~structure:Thin ~changes:Never "read" k ~length:1 [ Eio_error ]);
    match List.assoc_opt k t.cache with
    | Some v -> v
    | None -> raise (error ("no " ^ k))

  let sync_dir ((t, _) as d) _ =
    let f =
      ask d ~time:sync_time ~structure:Every ~changes:Maybe ~alone:[ Evict ]
        "sync" "/" ~length:0 [ Eio_error ]
    in
    match f with
    | Some Evict ->
        t.cache <- t.durable;
        fail f
    | Some _ -> fail f
    | None ->
        if t.cache <> t.durable then
          Eio_chaos.changed t.chaos (Eio_chaos.index t.chaos);
        t.durable <- t.cache

  let read_dir ((t, _) as d) _ =
    ignore
      (ask d ~structure:Every ~changes:Never "list" "/" ~length:0 []
        : fault option);
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
  let noun = "disk operation"
  let pp_op = pp_op
  let indeterminate _ = false

  let start t ~os:_ ~sw:_ ~clock:_ ~mono_clock:_ chaos =
    t.chaos <- chaos;
    t.cache <- [];
    t.durable <- [];
    t.promised <- [];
    t.gen <- t.gen + 1;
    t.keep <- None;
    t.runs <- [] :: t.runs;
    capability t

  (* What [setup] left is durable. *)
  let run t ~cut ~faults:_ =
    t.durable <- t.cache;
    t.keep <- cut;
    capability t

  let cut t =
    (match t.keep with
    | Some Lose_unsynced | None -> t.cache <- t.durable
    | Some Keep_unsynced -> ()
    | Some (Choose f) ->
        t.cache <-
          List.filter_map
            (fun (k, v) ->
              if List.assoc_opt k t.durable = Some v then Some (k, v)
              else if f 2 = 1 then Some (k, v)
              else Option.map (fun w -> (k, w)) (List.assoc_opt k t.durable))
            t.cache);
    t.durable <- t.cache;
    t.gen <- t.gen + 1;
    [ ("kept", String.concat " " (List.map fst t.cache)) ]

  let restart t ~power ~cut =
    t.restarts <- power :: t.restarts;
    if not power then t.gen <- t.gen + 1;
    List.iter
      (fun (k, v) ->
        if List.assoc_opt k t.cache <> Some v then
          Eio_chaos.lost t.chaos
            (Fmt.str "%s: it held %S when acknowledged, and is now %s" k v
               (match List.assoc_opt k t.cache with
               | None -> "missing"
               | Some w -> Fmt.str "%S" w)))
      (List.rev t.promised);
    t.promised <- [];
    t.keep <- cut;
    capability t
end

module P = Eio_chaos.Part (Part)

let part t = P.v t

let ack env name =
  match P.find env#chaos with
  | None -> invalid_arg "Disk.ack: not a run of a disk"
  | Some t -> (
      match List.assoc_opt name t.cache with
      | Some v ->
          t.promised <- (name, v) :: List.remove_assoc name t.promised;
          Eio_chaos.changed t.chaos (Eio_chaos.index t.chaos)
      | None -> invalid_arg name)
