open! Hermetic

(* [sample] and [sampled_share] are copied from eio-net's lib/chaos/plan.ml. *)

type pos = { device : string; kind : string; target : string; at : int }
type fault = { pos : pos; name : string }
type scenario = fault list

type step = {
  index : int;
  op : Op.t;
  allowed : string list;
  met : string option;
}

type trace = step list

let pos (op : Op.t) =
  { device = op.device; kind = op.kind; target = op.target; at = op.at }

let none = []

let fault_key f =
  Fmt.str "%s:%s:%s@%d=%s" f.pos.device f.pos.kind f.pos.target f.pos.at f.name

let key = function
  | [] -> "none"
  | s -> String.concat "+" (List.map fault_key s)

let fault_text f =
  Fmt.str "%s at %s %s %s @%d" f.name f.pos.device f.pos.kind f.pos.target
    f.pos.at

let text = function
  | [] -> "none"
  | s -> String.concat "; then " (List.map fault_text s)

let is_number s =
  s <> "" && String.for_all (function '0' .. '9' -> true | _ -> false) s

let cut s i = (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))

(* DEVICE:KIND:TARGET@AT=NAME, as [fault_key] writes it. *)
let is_fault_key k =
  match String.index_opt k ':' with
  | None -> false
  | Some i -> (
      let device, rest = cut k i in
      match String.index_opt rest ':' with
      | None -> false
      | Some j -> (
          let kind, rest = cut rest j in
          match String.rindex_opt rest '=' with
          | None -> false
          | Some e -> (
              let body, name = cut rest e in
              match String.rindex_opt body '@' with
              | None -> false
              | Some a ->
                  let _target, at = cut body a in
                  device <> "" && kind <> "" && name <> "" && is_number at)))

(* A target may hold a [+]: a key of two failures is one that some [+]
   splits into two keys of one. *)
let is_key k =
  is_fault_key k
  ||
  let rec from i =
    match String.index_from_opt k i '+' with
    | None -> false
    | Some j ->
        let a, b = cut k j in
        (is_fault_key a && is_fault_key b) || from (j + 1)
  in
  from 0

let parse_replay r =
  let hex = function '0' .. '9' | 'a' .. 'f' -> true | _ -> false in
  match String.index_opt r '/' with
  | Some 8 ->
      let h, k = cut r 8 in
      if String.for_all hex h && is_key k then Some (h, k) else None
  | _ -> None

(* The length class of an operation: its length rounded down to a power of
   two. *)
let length_class n =
  let rec go c n = if n <= 1 then c else go (c + 1) (n lsr 1) in
  go 0 n

let group (s : step) = (s.op.device, s.op.kind, s.op.target)

(* Of the operations of one kind on one target: the first three, the last,
   and the first of each length class. *)
let structural trace =
  let total = Hashtbl.create 64 and rank = Hashtbl.create 64 in
  let seen = Hashtbl.create 64 in
  List.iter
    (fun s ->
      let g = group s in
      Hashtbl.replace total g
        (1 + Option.value ~default:0 (Hashtbl.find_opt total g)))
    trace;
  List.partition
    (fun s ->
      let g = group s in
      let i = Option.value ~default:0 (Hashtbl.find_opt rank g) in
      Hashtbl.replace rank g (i + 1);
      let c = (g, length_class s.op.length) in
      let first = not (Hashtbl.mem seen c) in
      Hashtbl.replace seen c ();
      i < 3 || i = Hashtbl.find total g - 1 || first)
    trace

let singles steps =
  List.concat_map
    (fun s -> List.map (fun name -> [ { pos = pos s.op; name } ]) s.allowed)
    steps

let linear trace = singles (fst (structural trace))
let others trace = singles (snd (structural trace))

let seconds f ~first ~after =
  match
    List.find_opt (fun s -> s.met = Some f.name && pos s.op = f.pos) after
  with
  | None -> ([], [])
  | Some s1 ->
      let places = Hashtbl.create 64 in
      List.iter (fun s -> Hashtbl.replace places (pos s.op) ()) first;
      let later = List.filter (fun s -> s.index > s1.index) after in
      let changed, same =
        List.partition (fun s -> not (Hashtbl.mem places (pos s.op))) later
      in
      let pairs steps =
        List.concat_map
          (fun s ->
            List.map (fun name -> [ f; { pos = pos s.op; name } ]) s.allowed)
          steps
      in
      (pairs changed, pairs same)

let sample rng n l =
  let len = List.length l in
  if len <= n then l
  else
    let a = Array.of_list l in
    List.init n (fun i ->
        let lo = i * len / n and hi = (i + 1) * len / n in
        a.(lo + Random.State.int rng (hi - lo)))

let sampled_share ~budget ~linear ~classes =
  let spend = min (budget - linear) (max 200 (linear / 2)) in
  max 0 spend / max 1 classes

let pp_pos ppf p = Fmt.pf ppf "%s %s %s @%d" p.device p.kind p.target p.at

let digest trace =
  let s =
    Fmt.str "%a"
      Fmt.(list ~sep:(any ";") pp_pos)
      (List.map (fun s -> pos s.op) trace)
  in
  String.sub (Digest.to_hex (Digest.string s)) 0 8

let diverge a b =
  let rec go a b =
    match (a, b) with
    | x :: a, y :: b ->
        if pos x.op = pos y.op then go a b
        else
          Fmt.kstr Option.some "%a in the first, %a in the second" Op.pp x.op
            Op.pp y.op
    | x :: _, [] ->
        Fmt.kstr Option.some "%a in the first, nothing more in the second" Op.pp
          x.op
    | [], y :: _ ->
        Fmt.kstr Option.some "nothing more in the first, %a in the second" Op.pp
          y.op
    | [], [] -> None
  in
  go a b
