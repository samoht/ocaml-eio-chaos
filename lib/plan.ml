open! Hermetic

type pos = { device : string; kind : string; rank : int; at : int }

let pos_key ~single p =
  let k =
    if single then Fmt.str "%s:%d" p.kind p.rank
    else Fmt.str "%s:%s:%d" p.device p.kind p.rank
  in
  if p.at > 0 then Fmt.str "%s@%d" k p.at else k

(* {1 Traces} *)

type named = { name : string; text : string Lazy.t }

type allowed = {
  once : named list;
  lasting : named list;
  durable : named list;
  alone : named list;
  mild : named list;
}

type faults =
  | No_faults
  | Faults : {
      name : 'f -> string;
      pp : 'f Fmt.t;
      once : 'f list;
      lasting : 'f list;
      durable : 'f list;
      alone : 'f list;
      mild : 'f list;
    }
      -> faults

type step = {
  index : int;
  op : Op.t;
  rank : int;
  faults : faults;
  mutable places : (int * int) list;
}

type trace = step list

let pos s =
  { device = Op.device s.op; kind = Op.kind s.op; rank = s.rank; at = 0 }

let at (p : pos) (s : step) =
  s.rank = p.rank
  && String.equal (Op.kind s.op) p.kind
  && String.equal (Op.device s.op) p.device

let allowed s =
  match s.faults with
  | No_faults ->
      { once = []; lasting = []; durable = []; alone = []; mild = [] }
  | Faults f ->
      let named l =
        List.map
          (fun x -> { name = f.name x; text = lazy (Fmt.to_to_string f.pp x) })
          l
      in
      {
        once = named f.once;
        lasting = named f.lasting;
        durable = named f.durable;
        alone = named f.alone;
        mild = named f.mild;
      }

(* The length class of an operation: its length rounded down to a power of
   two. *)
let length_class n =
  let rec go c n = if n <= 1 then c else go (c + 1) (n lsr 1) in
  go 0 n

let group (op : Op.t) = (Op.device op, Op.kind op, Op.target op)

(* The rest of a target's name past that of its instance. *)
let suffix (op : Op.t) =
  let i = Op.instance op and target = Op.target op in
  if String.starts_with ~prefix:i target then
    String.sub target (String.length i) (String.length target - String.length i)
  else target

(* Of the instances of each device, in the order the trace met them: the first
   three, the last, and the first of each shape, the kinds, details and
   length classes of the operations of each of its targets. *)
let instances (trace : trace) =
  let shapes = Hashtbl.create 16 and order = ref [] in
  List.iter
    (fun s ->
      let op = s.op in
      if Op.instance op <> "" then begin
        let k = (Op.device op, Op.instance op) in
        let l =
          match Hashtbl.find_opt shapes k with
          | Some l -> l
          | None ->
              order := k :: !order;
              []
        in
        Hashtbl.replace shapes k
          ((Op.kind op, suffix op, Op.detail op, length_class (Op.length op))
          :: l)
      end)
    trace;
  let devices = Hashtbl.create 4 in
  List.iter
    (fun ((d, _) as k) ->
      Hashtbl.replace devices d
        (k :: Option.value ~default:[] (Hashtbl.find_opt devices d)))
    !order;
  let keep = Hashtbl.create 16 in
  Hashtbl.iter
    (fun _ ks ->
      let n = List.length ks and seen = Hashtbl.create 8 in
      List.iteri
        (fun j k ->
          let shape = List.sort_uniq compare (Hashtbl.find shapes k) in
          let first = not (Hashtbl.mem seen shape) in
          Hashtbl.replace seen shape ();
          if j < 3 || j = n - 1 || first then Hashtbl.replace keep k ())
        ks)
    devices;
  fun (op : Op.t) ->
    Op.instance op = "" || Hashtbl.mem keep (Op.device op, Op.instance op)

(* Of the operations of one kind on one target that thin: the first three,
   the last, and the first of each length class, so a log of many records
   of varying length costs runs by its classes, not its records. An
   operation after another kind takes that one's place. *)
let structural (trace : trace) =
  let per = Hashtbl.create 7 in
  List.iter
    (fun s ->
      match Op.structure s.op with
      | Thin ->
          let k = group s.op in
          Hashtbl.replace per k
            (s :: Option.value ~default:[] (Hashtbl.find_opt per k))
      | Every | After _ -> ())
    trace;
  let keep = Hashtbl.create 7 in
  Hashtbl.iter
    (fun _ steps ->
      let steps = List.rev steps in
      let n = List.length steps and seen = Hashtbl.create 8 in
      List.iteri
        (fun i s ->
          let c = length_class (Op.length s.op) in
          let first = not (Hashtbl.mem seen c) in
          Hashtbl.replace seen c ();
          if i < 3 || i = n - 1 || first then Hashtbl.replace keep s.index ())
        steps)
    per;
  let last = Hashtbl.create 7 and kept = instances trace in
  List.partition
    (fun s ->
      kept s.op
      &&
      match Op.structure s.op with
      | Every -> true
      | Thin ->
          Hashtbl.replace last (group s.op) s.index;
          Hashtbl.mem keep s.index
      | After kind -> (
          match
            Hashtbl.find_opt last (Op.device s.op, kind, Op.target s.op)
          with
          | None -> true
          | Some i -> Hashtbl.mem keep i))
    trace

(* The operations sure to be structural as a run goes, whatever follows: one
   that does not thin; one of the first three of its kind on its target, or
   the first of its length class; one after none of those, or after one sure
   to be. *)
module Sure = struct
  type t = {
    seen : (string * string * string, int * (int, unit) Hashtbl.t) Hashtbl.t;
    last : (string * string * string, bool) Hashtbl.t;
    instances : (string * string, bool) Hashtbl.t;
    counts : (string, int) Hashtbl.t;
  }

  let v () =
    {
      seen = Hashtbl.create 7;
      last = Hashtbl.create 7;
      instances = Hashtbl.create 7;
      counts = Hashtbl.create 2;
    }

  (* One of the first three instances of its device. *)
  let instance t (op : Op.t) =
    Op.instance op = ""
    ||
    let k = (Op.device op, Op.instance op) in
    match Hashtbl.find_opt t.instances k with
    | Some sure -> sure
    | None ->
        let n =
          Option.value ~default:0 (Hashtbl.find_opt t.counts (Op.device op))
        in
        Hashtbl.replace t.counts (Op.device op) (n + 1);
        Hashtbl.replace t.instances k (n < 3);
        n < 3

  let add t (op : Op.t) =
    instance t op
    &&
    match Op.structure op with
    | Every -> true
    | Thin ->
        let k = group op in
        let n, classes =
          match Hashtbl.find_opt t.seen k with
          | Some v -> v
          | None -> (0, Hashtbl.create 4)
        in
        let c = length_class (Op.length op) in
        let sure = n < 3 || not (Hashtbl.mem classes c) in
        Hashtbl.replace classes c ();
        Hashtbl.replace t.seen k (n + 1, classes);
        Hashtbl.replace t.last k sure;
        sure
    | After kind ->
        Option.value ~default:true
          (Hashtbl.find_opt t.last (Op.device op, kind, Op.target op))
end

(* {1 Sampling} *)

let sample rng n l =
  let len = List.length l in
  if len <= n then l
  else
    let a = Array.of_list l in
    List.init n (fun i ->
        let lo = i * len / n and hi = (i + 1) * len / n in
        a.(lo + Random.State.int rng (hi - lo)))

let interleave rng l =
  let rec merge a b =
    match (a, b) with
    | [], l | l, [] -> l
    | x :: a, y :: b -> x :: y :: merge a b
  in
  let a = Array.of_list l in
  let rec order lo hi =
    if hi - lo <= 1 then Array.to_list (Array.sub a lo (hi - lo))
    else
      let mid = (lo + hi) / 2 in
      let x = order lo mid and y = order mid hi in
      if Random.State.bool rng then merge x y else merge y x
  in
  order 0 (Array.length a)

let sampled_share ~budget ~linear ~classes =
  let spend = min (budget - linear) (max (budget / 40) (linear / 2)) in
  max 0 spend / max 1 classes

module Pairs = struct
  type 'a t = {
    cap : int;
    ranks : 'a list array;
        (* by rank, latest first: those on a changed operation, then the
           others *)
    mutable size : int;
    mutable top : int; (* no rank above holds one *)
  }

  let v cap =
    let cap = max 0 cap in
    { cap; ranks = Array.make (2 * cap) []; size = 0; top = 0 }

  let add_at t r x =
    if r < Array.length t.ranks then begin
      t.ranks.(r) <- x :: t.ranks.(r);
      t.size <- t.size + 1;
      t.top <- max t.top r;
      while t.size > t.cap do
        while t.ranks.(t.top) = [] do
          t.top <- t.top - 1
        done;
        t.ranks.(t.top) <- List.tl t.ranks.(t.top);
        t.size <- t.size - 1
      done
    end

  let add t (changed, rest) =
    List.iteri (fun r x -> add_at t r x) changed;
    List.iteri (fun r x -> add_at t (t.cap + r) x) rest

  let to_list t = List.concat_map List.rev (Array.to_list t.ranks)
end

(* {1 Scenarios} *)

type fault = { pos : pos; name : string; text : string; alone : bool }
type state = Drop | Keep | Enum of int list | Seed of int | Shape of int list
type cut = { before : pos option; state : state }

type scenario = {
  faults : fault list;
  lasting : fault option;
  cut : cut option;
  rcut : cut option;
}

let none = { faults = []; lasting = None; cut = None; rcut = None }
let base = { none with cut = Some { before = None; state = Drop } }

let rec trim = function
  | [] -> []
  | l -> ( match List.rev l with 0 :: r -> trim (List.rev r) | _ -> l)

let state_key = function
  | Drop -> "drop"
  | Keep -> "keep"
  | Enum v -> "e" ^ String.concat "." (List.map string_of_int (trim v))
  | Seed s -> Fmt.str "s%d" s
  | Shape v -> "p" ^ String.concat "." (List.map string_of_int (trim v))

(* Only the key [state_key] makes names a state: no negative choice, no
   leading zero or sign, no trailing zero. *)
let state_of_key k =
  let body = if k = "" then "" else String.sub k 1 (String.length k - 1) in
  let choice s =
    match int_of_string_opt s with Some c when c >= 0 -> Some c | _ -> None
  in
  let choices mk =
    if body = "" then Some (mk [])
    else
      List.fold_right
        (fun s acc ->
          match (acc, choice s) with
          | Some l, Some c -> Some (c :: l)
          | _ -> None)
        (String.split_on_char '.' body)
        (Some [])
      |> Option.map mk
  in
  let st =
    match k with
    | "drop" -> Some Drop
    | "keep" -> Some Keep
    | _ when k = "" -> None
    | _ when k.[0] = 'e' -> choices (fun v -> Enum v)
    | _ when k.[0] = 'p' -> choices (fun v -> Shape v)
    | _ when k.[0] = 's' -> Option.map (fun s -> Seed s) (choice body)
    | _ -> None
  in
  match st with Some st when state_key st = k -> Some st | _ -> None

let faults_key ~single = function
  | [] -> "none"
  | fs ->
      String.concat "+"
        (List.map (fun f -> pos_key ~single f.pos ^ "." ^ f.name) fs)

let cut_key ~single c =
  let at =
    match c.before with
    | None -> "end"
    | Some p -> "before:" ^ pos_key ~single p
  in
  at ^ "|" ^ state_key c.state

let key ~single s =
  match (s.cut, s.lasting) with
  | None, Some l -> Fmt.str "from:%s.%s" (pos_key ~single l.pos) l.name
  | None, None -> faults_key ~single s.faults
  | Some c, _ ->
      let r =
        match s.rcut with None -> "" | Some r -> "|r|" ^ cut_key ~single r
      in
      Fmt.str "%s|%s%s" (faults_key ~single s.faults) (cut_key ~single c) r

let cut_last s =
  match String.rindex_opt s '|' with
  | Some i ->
      Some (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))
  | None -> None

(* The key before the last [|r|] in [s], if any. *)
let before_rcut s =
  let rec go i =
    if i < 0 then None
    else if String.sub s i 3 = "|r|" then Some (String.sub s 0 i)
    else go (i - 1)
  in
  go (String.length s - 3)

let well_formed k =
  let state s = Option.is_some (state_of_key s) in
  match cut_last k with
  | None -> false
  | Some (rest, st) -> (
      state st
      &&
      match before_rcut rest with
      | None -> String.contains rest '|'
      | Some main -> (
          match cut_last main with
          | Some (rest, st) -> state st && String.contains rest '|'
          | None -> false))

let is_number s =
  s <> ""
  && String.for_all (function '0' .. '9' -> true | _ -> false) s
  && (s = "0" || s.[0] <> '0')

let rank_of r =
  is_number r
  && match int_of_string_opt r with Some n -> n > 0 | None -> false

(* [kind:rank], after [device:] unless [single], before [@at] if [at] is
   not [0]: [rank] from [1]. *)
let pos_of_key ~single p =
  let p, at =
    match String.index_opt p '@' with
    | None -> (p, Some 0)
    | Some i -> (
        let at = String.sub p (i + 1) (String.length p - i - 1) in
        let at = if is_number at then int_of_string_opt at else None in
        (String.sub p 0 i, match at with Some 0 -> None | at -> at))
  in
  match (at, String.split_on_char ':' p) with
  | Some _, [ kind; rank ] when single -> kind <> "" && rank_of rank
  | Some _, [ device; kind; rank ] when not single ->
      device <> "" && kind <> "" && rank_of rank
  | _ -> false

let fault_of_key ~single f =
  match String.index_opt f '.' with
  | Some i ->
      let name = String.sub f (i + 1) (String.length f - i - 1) in
      pos_of_key ~single (String.sub f 0 i)
      && name <> ""
      && not (String.exists (function '|' | '/' -> true | _ -> false) name)
  | None -> false

let errors_key ~single k =
  k = "none"
  ||
  match String.starts_with ~prefix:"from:" k with
  | true -> fault_of_key ~single (String.sub k 5 (String.length k - 5))
  | false -> List.for_all (fault_of_key ~single) (String.split_on_char '+' k)

let split_state_key k =
  match String.split_on_char '|' k with
  | f :: c :: st :: rest ->
      Option.map
        (fun st -> (String.concat "|" (f :: c :: "drop" :: rest), st))
        (state_of_key st)
  | _ -> None

let ordinal n =
  let suffix =
    match (n mod 100, n mod 10) with
    | (11 | 12 | 13), _ -> "th"
    | _, 1 -> "st"
    | _, 2 -> "nd"
    | _, 3 -> "rd"
    | _ -> "th"
  in
  string_of_int n ^ suffix

let pos_text ~single ?(at = fun _ -> None) (p : pos) =
  match at p with
  | Some line -> line
  | None when single -> Fmt.str "the %s %s of the run" (ordinal p.rank) p.kind
  | None -> Fmt.str "the %s %s %s of the run" (ordinal p.rank) p.device p.kind

let faults_text ~single ?at = function
  | [] -> "none"
  | fs ->
      String.concat "; then "
        (List.map (fun f -> pos_text ~single ?at f.pos ^ " " ^ f.text) fs)

let text ~single ?at s =
  match s.lasting with
  | Some l -> Fmt.str "%s from %s on" l.text (pos_text ~single ?at l.pos)
  | None -> faults_text ~single ?at s.faults

let state_text = function
  | Drop -> "every unsynced change lost"
  | Keep -> "every change kept, as after a process kill"
  | Enum _ as st ->
      Fmt.str "state %s of those the cut allows, tried one by one"
        (state_key st)
  | (Seed _ | Shape _) as st ->
      Fmt.str
        "state %s, drawn at random: the cut allows more states than its share \
         of the tier"
        (state_key st)

let cut_text ~single ?at c =
  let where =
    match c.before with
    | None -> "at the end"
    | Some p -> "before " ^ pos_text ~single ?at p
  in
  Fmt.str "%s, %s" where (state_text c.state)

(* {1 Runs with no cut} *)

let after_text text = function
  | 0 -> text
  | 1 -> text ^ " after 1 byte"
  | k -> Fmt.str "%s after %d bytes" text k

let fault ?(alone = false) ?(at = 0) (s : step) (n : named) =
  {
    pos = { (pos s) with at };
    name = n.name;
    text = after_text (Lazy.force n.text) at;
    alone;
  }

let single f = { none with faults = [ f ] }

let singles ?alone get steps =
  List.concat_map
    (fun (s : step) -> List.map (fun n -> fault ?alone s n) (get (allowed s)))
    steps

(* The faults of [s] at its places [ks], each after that many bytes. *)
let placed (s : step) ks =
  let once = (allowed s).once in
  List.concat_map (fun at -> List.map (fun n -> fault ~at s n) once) ks

(* The places of [s] a fault may land at in the linear tier, as their source
   is one of [linear]'s operations, and the others, past its start. The
   start of an operation the tier tries already is no place of its own. *)
let places ~linear (s : step) =
  let lin = List.filter (fun (_, j) -> Hashtbl.mem linear j) s.places in
  let lin = List.sort_uniq Int.compare (List.map fst lin) in
  let lin =
    if Hashtbl.mem linear s.index then List.filter (( <> ) 0) lin else lin
  in
  let rest =
    List.filter
      (fun k -> k > 0 && not (List.mem k lin))
      (List.sort_uniq Int.compare (List.map fst s.places))
  in
  (lin, rest)

let selected ops =
  let h = Hashtbl.create 64 in
  List.iter (fun (s : step) -> Hashtbl.replace h s.index ()) ops;
  h

let of_steps steps =
  List.map single (singles (fun a -> a.once) steps)
  @ List.map
      (fun l -> { none with lasting = Some l })
      (singles (fun a -> a.lasting) steps)

let linear trace =
  let ops, _ = structural trace in
  let linear = selected ops in
  List.map single (singles (fun a -> a.once) ops)
  @ List.concat_map
      (fun s -> List.map single (placed s (fst (places ~linear s))))
      trace
  @ List.map
      (fun l -> { none with lasting = Some l })
      (singles (fun a -> a.lasting) ops)
  @ List.map single (singles ~alone:true (fun a -> a.alone) ops)

(* [n] of the [total] scenarios [at] builds from [ops], [count] of each,
   drawn as [sample] draws them, each built only once picked. *)
let spread ~count ~at ~total ops rng n =
  if total <= n then List.concat_map at ops
  else
    let next =
      Array.init n (fun i ->
          let lo = i * total / n and hi = (i + 1) * total / n in
          lo + Random.State.int rng (hi - lo))
    in
    let rec go i k acc = function
      | [] -> List.rev acc
      | s :: rest ->
          let m = count s in
          if i < n && next.(i) < k + m then begin
            let c = Array.of_list (at s) in
            let rec take i acc =
              if i < n && next.(i) < k + m then
                take (i + 1) (c.(next.(i) - k) :: acc)
              else (i, acc)
            in
            let i, acc = take i acc in
            go i (k + m) acc rest
          end
          else go i (k + m) acc rest
    in
    go 0 0 [] ops

(* The scenarios of the sampled tier at one step: its own faults, if it is
   not in the linear tier, and those at its other places. *)
type candidates = Start of step | Places of step * int list

(* Sampled as [sample] samples them, but the scenarios are counted first and
   built only once picked. *)
let others trace =
  let sel, rest = structural trace in
  let linear = selected sel in
  let ops =
    List.map (fun s -> Start s) rest
    @ List.filter_map
        (fun s ->
          match snd (places ~linear s) with
          | [] -> None
          | ks -> Some (Places (s, ks)))
        trace
  in
  let at = function
    | Start s -> of_steps [ s ]
    | Places (s, ks) -> List.map single (placed s ks)
  in
  let count = function
    | Start s -> (
        match s.faults with
        | No_faults -> 0
        | Faults f -> List.length f.once + List.length f.lasting)
    | Places (s, ks) -> (
        match s.faults with
        | No_faults -> 0
        | Faults f -> List.length f.once * List.length ks)
  in
  let total = List.fold_left (fun n s -> n + count s) 0 ops in
  (total, spread ~count ~at ~total ops)

let is_alone s = List.exists (fun f -> f.alone) s.faults

let changes (op : Op.t) =
  match Op.changes op with Never -> false | Maybe | Always -> true

(* The changes a run made, in order: what tells whether a fault changed its
   course. *)
module Changes = struct
  type t = step array

  let v trace = Array.of_list (List.filter (fun s -> changes s.op) trace)

  (* Whether the changes of [later], past [s1], differ from those of the
     first run past the same place. *)
  let diverge (t : t) later s1 =
    let rec start i =
      if i < Array.length t && t.(i).index <= s1.index then start (i + 1) else i
    in
    let rec go i = function
      | [] -> i < Array.length t
      | s :: rest ->
          i >= Array.length t
          || (not (Op.same s.op t.(i).op))
          || go (i + 1) rest
    in
    go (start 0) (List.filter (fun s -> changes s.op) later)
end

(* The operations of [after] past [s1], where the first fault landed: if the
   fault changed what the run changes, those that may change what a device
   keeps, then the others, each nearest the fault first; else none, and all
   of them. *)
let aimed ~first ~(after : trace) s1 =
  let later = List.filter (fun s -> s.index > s1.index) after in
  if Changes.diverge first later s1 then
    let changing, other = List.partition (fun s -> changes s.op) later in
    (changing @ other, [])
  else ([], later)

(* At most [limit] of the pairs [pairs] makes at [changed], in order, and of
   those at [rest], sampled, in an order each prefix of which is sampled too:
   each operation makes one pair or more, so [limit] operations of each are
   enough. *)
let aim ~rng limit pairs (changed, rest) =
  let first l = List.take limit l in
  ( first (pairs (first changed)),
    interleave rng (sample rng limit (pairs (sample rng limit rest))) )

let seconds ?(limit = max_int) ~rng one ~first ~(after : trace) =
  match List.find_opt (at one.pos) after with
  | None -> ([], [])
  | Some s1 ->
      let changed, rest = aimed ~first ~after s1 in
      let pairs steps =
        List.map
          (fun two -> { none with faults = [ one; two ] })
          (List.concat_map
             (fun (s : step) ->
               singles (fun a -> a.once @ a.mild) [ s ]
               @ placed s
                   (List.sort_uniq Int.compare
                      (List.filter_map
                         (fun (k, _) -> if k > 0 then Some k else None)
                         s.places)))
             steps)
      in
      aim ~rng limit pairs (changed, rest)

(* {1 Runs with a cut} *)

let both s =
  match s.cut with
  | Some c ->
      [
        { s with cut = Some { c with state = Drop } };
        { s with cut = Some { c with state = Keep } };
      ]
  | None -> []

let at_end = Some { before = None; state = Drop }

(* After each operation of [ops] that changes what a device keeps, before the
   one that follows it in [trace]; [None] is the end. *)
let cut_points ~changed (trace : trace) ops =
  let next = Hashtbl.create 16 in
  let rec link = function
    | a :: (b :: _ as rest) ->
        Hashtbl.replace next a.index b;
        link rest
    | _ -> ()
  in
  link trace;
  let after =
    List.filter_map
      (fun s ->
        let changes =
          match Op.changes s.op with
          | Always -> true
          | Never | Maybe -> changed s
        in
        if changes then
          Option.map
            (fun (b : step) -> Some (pos b))
            (Hashtbl.find_opt next s.index)
        else None)
      ops
  in
  None :: after

let cuts ~changed trace =
  List.concat_map
    (fun before -> both { none with cut = Some { before; state = Drop } })
    (cut_points ~changed trace (fst (structural trace)))

let durable_faults steps =
  List.map
    (fun f -> { none with faults = [ f ]; cut = at_end })
    (singles (fun a -> a.durable) steps)

let alone_faults steps =
  List.map
    (fun f -> { none with faults = [ f ]; cut = at_end })
    (singles ~alone:true (fun a -> a.alone) steps)

let cut_faults trace =
  let ops = fst (structural trace) in
  List.concat_map both (durable_faults ops @ alone_faults ops)

let cut_others ~changed trace =
  let sel, rest = structural trace in
  let lin = cut_points ~changed trace sel in
  List.concat_map
    (fun before -> both { none with cut = Some { before; state = Drop } })
    (List.filter
       (fun c -> not (List.mem c lin))
       (cut_points ~changed trace rest))
  @ List.concat_map both (durable_faults rest)

let rcuts ~changed s ~(restart : trace) =
  List.concat_map
    (fun before ->
      List.map
        (fun state -> { s with rcut = Some { before; state } })
        [ Drop; Keep ])
    (cut_points ~changed restart (fst (structural restart)))

let cut_seconds ?(limit = max_int) ~rng one ~first ~(after : trace) =
  match List.find_opt (at one.pos) after with
  | None -> ([], [])
  | Some s1 ->
      let changed, rest = aimed ~first ~after:(fst (structural after)) s1 in
      let pairs steps =
        List.concat_map
          (fun two -> both { none with faults = [ one; two ]; cut = at_end })
          (singles (fun a -> a.durable) steps)
      in
      aim ~rng limit pairs (changed, rest)

(* A cut is tried in both states. *)
let cut_floor ~sure ~changed ~durable (op : Op.t) =
  if not sure then 0
  else
    (match Op.changes op with
      | Always -> 2
      | Maybe | Never -> if changed then 2 else 0)
    + (2 * durable)

(* {1 States} *)

let shape ~widths i =
  let rng = Random.State.make [| i |] in
  let d = List.length widths in
  let last n = max 0 (n - 1) in
  match i mod 4 with
  | 0 -> Seed (i / 4)
  | 1 ->
      let t = Random.State.int rng (d + 1) in
      Shape (List.mapi (fun k n -> if k < t then last n else 0) widths)
  | 2 ->
      let j = Random.State.int rng (max 1 d) in
      Shape
        (List.mapi
           (fun k n ->
             if k = j && n > 1 then 1 + Random.State.int rng (n - 1) else 0)
           widths)
  | _ ->
      let j = Random.State.int rng (max 1 d) in
      Shape (List.mapi (fun k n -> if k = j then 0 else last n) widths)

let next_choices trail =
  let rec go = function
    | [] -> None
    | (c, n) :: rest when c + 1 < n -> Some (List.rev_map fst rest @ [ c + 1 ])
    | _ :: rest -> go rest
  in
  go trail

(* The tree of the draws the states met so far make: a node per draw, with
   the choices it had, and below it the choices taken. *)
module Draws = struct
  type t = { mutable width : int; below : (int, t) Hashtbl.t }

  let empty () = { width = 0; below = Hashtbl.create 2 }

  let add t trail =
    ignore
      (List.fold_left
         (fun node (c, n) ->
           node.width <- n;
           match Hashtbl.find_opt node.below c with
           | Some next -> next
           | None ->
               let next = empty () in
               Hashtbl.replace node.below c next;
               next)
         t (List.rev trail)
        : t)

  (* The states the tree allows, a choice not taken counted as the largest
     taken one at its draw. *)
  let rec estimate t =
    if t.width = 0 then 1
    else
      let sum, widest, taken =
        Hashtbl.fold
          (fun _ next (sum, widest, taken) ->
            let e = estimate next in
            (min max_int (sum + e), max widest e, taken + 1))
          t.below (0, 0, 0)
      in
      let rest = t.width - taken in
      if rest > 0 && widest > (max_int - sum) / rest then max_int
      else sum + (rest * widest)
end
