open! Hermetic

type entry = { index : int; op : Op.t; met : (string * string) option }

let parent path =
  match String.rindex_opt path '/' with
  | Some 0 -> "/"
  | Some i -> String.sub path 0 i
  | None -> "."

(* What a repeated operation keeps: its part, its kind, the directory it is
   in and its fault, so a retry under one name or under fresh ones repeats. *)
let shape e = (e.op.device, e.op.kind, parent e.op.target, Option.map fst e.met)

(* The lines of a trace: an operation, or a block of [period] operations
   repeated, of which the first and the last are shown. *)
type line =
  | Line of entry
  | Repeated of { period : int; times : int; first : int; last : int }

let max_period = 8
let min_times = 3

(* A trace shows at most [keep] lines at each end, and those around the
   first fault. *)
let keep = 100
let context = 10

type item = entry * (string * string * string * string option)

(* A block repeating at the end of the trace: its first instance, the
   instances so far, the last whole one and the start of the next. *)
type rep = {
  period : int;
  first_block : item array;
  second : int; (* the first operation of the second instance *)
  mutable count : int;
  mutable last_block : item array;
  mutable before_last : int; (* the operation before the last instance *)
  mutable partial : item list; (* the next instance, latest first *)
  mutable npartial : int;
}

let window = (2 * keep) + (2 * context) + 1
let blank = Repeated { period = 0; times = 0; first = 0; last = 0 }

type t = {
  mutable buf : item array; (* the operations no line holds yet *)
  mutable len : int;
  mutable rep : rep option;
  head : line array; (* the first [window] lines *)
  ring : line array; (* the last [window] lines *)
  mutable n : int;
  mutable fault : int option; (* the line of the first fault *)
  mutable around : (int * line) list;
}

let v () =
  {
    buf = [||];
    len = 0;
    rep = None;
    head = Array.make window blank;
    ring = Array.make window blank;
    n = 0;
    fault = None;
    around = [];
  }

let index ((e, _) : item) = e.index

let emit t l =
  let k = t.n in
  if k < window then t.head.(k) <- l;
  t.ring.(k mod window) <- l;
  (match (t.fault, l) with
  | None, Line { met = Some _; _ } ->
      t.fault <- Some k;
      for j = max 0 (k - context) to k do
        t.around <- (j, t.ring.(j mod window)) :: t.around
      done
  | Some f, _ when k <= f + context -> t.around <- (k, l) :: t.around
  | _ -> ());
  t.n <- k + 1

let emit_block t b = Array.iter (fun (e, _) -> emit t (Line e)) b

let append t e =
  if t.len = Array.length t.buf then begin
    let b = Array.make (max 16 (2 * t.len)) e in
    Array.blit t.buf 0 b 0 t.len;
    t.buf <- b
  end;
  t.buf.(t.len) <- e;
  t.len <- t.len + 1

let drop t k =
  Array.blit t.buf k t.buf 0 (t.len - k);
  t.len <- t.len - k

(* The whole instances of the block of [p] operations from the start of the
   buffer, and whether more may come. *)
let run t p =
  let rec go r =
    let j = r * p in
    let rec same k =
      k >= p
      || j + k >= t.len
      || (snd t.buf.(j + k) = snd t.buf.(j + k - p) && same (k + 1))
    in
    if not (same 0) then (r, false)
    else if j + p > t.len then (r, true)
    else go (r + 1)
  in
  go 1

let close t rep rest =
  t.rep <- None;
  emit_block t rep.first_block;
  emit t
    (Repeated
       {
         period = rep.period;
         times = rep.count - 2;
         first = rep.second;
         last = rep.before_last;
       });
  emit_block t rep.last_block;
  List.iter (append t) rest

(* A block of [p] operations repeated [r] times collapses if that hides two
   lines at least, as one that hides one saves none. *)
let worth p r = r >= min_times && (r - 2) * p > 1

(* The block that covers the most operations among the periods [only]
   keeps, worth collapsing: its period and instances. *)
let best runs ~only =
  let bp = ref 1 and br = ref 1 in
  Array.iteri
    (fun k (r, _) ->
      let p = k + 1 in
      if only k && worth p r && r * p > !bp * !br then begin
        bp := p;
        br := r
      end)
    runs;
  (!bp, !br)

(* The buffer's first lines: the block of [p] operations repeated [r] times,
   or its first operation. *)
let take t (p, r) =
  if worth p r then begin
    let b = Array.sub t.buf 0 (r * p) in
    emit_block t (Array.sub b 0 p);
    emit t
      (Repeated
         {
           period = p;
           times = r - 2;
           first = index b.(p);
           last = index b.(((r - 1) * p) - 1);
         });
    emit_block t (Array.sub b ((r - 1) * p) p);
    drop t (r * p)
  end
  else begin
    emit t (Line (fst t.buf.(0)));
    drop t 1
  end

(* The block of [q] operations repeated [r] times so far, which more may
   follow. *)
let repeating t q r =
  let rest = Array.to_list (Array.sub t.buf (r * q) (t.len - (r * q))) in
  t.rep <-
    Some
      {
        period = q;
        first_block = Array.sub t.buf 0 q;
        second = index t.buf.(q);
        count = r;
        last_block = Array.sub t.buf ((r - 1) * q) q;
        before_last = index t.buf.(((r - 1) * q) - 1);
        partial = List.rev rest;
        npartial = List.length rest;
      };
  t.len <- 0;
  t.buf <- [||]

(* Decide the lines the buffer starts with, as far as the operations so far
   allow, or all of them at the end. *)
let rec decide ~at_end t =
  if t.len > 0 then begin
    let runs = Array.init max_period (fun k -> run t (k + 1)) in
    let ongoing k = (not at_end) && snd runs.(k) in
    match List.find_opt ongoing (List.init max_period Fun.id) with
    | None ->
        take t (best runs ~only:(fun _ -> true));
        decide ~at_end t
    | Some k ->
        (* Past [2 * max_period] operations, every block still repeating is a
           multiple of the shortest one, which covers as much as they do:
           once it covers more than any that ended, it wins. *)
        let q = k + 1 and r = fst runs.(k) in
        let bp, br = best runs ~only:(fun k -> not (ongoing k)) in
        if t.len >= 2 * max_period && worth q r && r * q > bp * br then
          repeating t q r
  end

let add t e =
  let e = (e, shape e) in
  match t.rep with
  | None ->
      append t e;
      decide ~at_end:false t
  | Some rep ->
      let k = rep.npartial in
      if snd e = snd rep.last_block.(k) then begin
        rep.partial <- e :: rep.partial;
        rep.npartial <- k + 1;
        if k + 1 = rep.period then begin
          rep.before_last <- index rep.last_block.(rep.period - 1);
          rep.last_block <- Array.of_list (List.rev rep.partial);
          rep.partial <- [];
          rep.npartial <- 0;
          rep.count <- rep.count + 1
        end
      end
      else begin
        close t rep (List.rev (e :: rep.partial));
        decide ~at_end:false t
      end

let finish t =
  (match t.rep with
  | Some rep -> close t rep (List.rev rep.partial)
  | None -> ());
  decide ~at_end:true t

let line t i =
  if i < window then t.head.(i)
  else if i >= t.n - window then t.ring.(i mod window)
  else List.assoc i t.around

let plural k = if k = 1 then "" else "s"

(* The report's lines: the first and last [keep] and those around the first
   fault, or all of them when they are few. *)
let pp pp_entry ppf t =
  finish t;
  let n = t.n in
  let shown i =
    n <= window || i < keep
    || i >= n - keep
    || match t.fault with Some f -> abs (i - f) <= context | None -> false
  in
  let rec pp i =
    if i < n then (
      if shown i then begin
        (match line t i with
        | Line ({ met = None; _ } as e) -> Fmt.pf ppf "    %a@\n" pp_entry e
        | Line ({ met = Some (_, text); _ } as e) ->
            Fmt.pf ppf "    %a  <- %s@\n" pp_entry e text
        | Repeated { period; times; first; last } ->
            Fmt.pf ppf
              "    ... the %d operation%s above, %d more time%s, #%d to #%d@\n"
              period (plural period) times (plural times) first last);
        pp (i + 1)
      end
      else
        let rec skip j = if j < n && not (shown j) then skip (j + 1) else j in
        let j = skip i in
        Fmt.pf ppf "    ... %d more line%s@\n" (j - i) (plural (j - i));
        pp j)
  in
  pp 0
