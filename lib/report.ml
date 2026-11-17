open! Hermetic

(* The collapse of repeated blocks and the window of a long trace are
   adapted from eio-net's lib/chaos/eio_net_chaos.ml. *)

type ended = Returned | Raised of exn | Cut
type stage = Setup | Run | Lost of ended | Inspect of ended | Restart of ended
type failure = { stage : stage; error : string }

(* A report a nested check raised is text, its lines set in. *)
let exn_text = function
  | Failure m when String.contains m '\n' -> m
  | x -> Printexc.to_string x

let plural n = if n = 1 then "" else "s"

type line =
  | Step of Plan.step
  | Repeated of { period : int; times : int; first : int; last : int }

let max_period = 8
let min_times = 3

(* What a repeated operation keeps: its part, its kind and what it met. *)
let shape (s : Plan.step) = (s.op.device, s.op.kind, s.met)

(* The times the block of [p] operations at [i] repeats from [i]. *)
let times a i p =
  let n = Array.length a in
  let rec same j k =
    k = p || (shape a.(j + k) = shape a.(j + k - p) && same j (k + 1))
  in
  let rec go r =
    let j = i + (r * p) in
    if j + p <= n && same j 0 then go (r + 1) else r
  in
  go 1

(* Runs of a repeated block collapse to its first and last instances. A
   collapse hides two lines at least, as one that hides one saves none. *)
let collapse trace =
  let a = Array.of_list trace in
  let n = Array.length a in
  let rec go i acc =
    if i >= n then List.rev acc
    else
      let best =
        List.fold_left
          (fun (bp, br) p ->
            let r = times a i p in
            if r >= min_times && r * p > bp * br then (p, r) else (bp, br))
          (1, 1)
          (List.init max_period (fun k -> k + 1))
      in
      match best with
      | p, r when r >= min_times && (r - 2) * p > 1 ->
          let block j = List.init p (fun k -> Step a.(j + k)) in
          let last = i + ((r - 1) * p) in
          let repeated =
            Repeated
              {
                period = p;
                times = r - 2;
                first = a.(i + p).index;
                last = a.(last - 1).index;
              }
          in
          go
            (i + (r * p))
            (List.rev_append (block i @ (repeated :: block last)) acc)
      | _ -> go (i + 1) (Step a.(i) :: acc)
  in
  go 0 []

(* At most [keep] lines at each end, and those around the first failure. *)
let keep = 100
let context = 10

let pp_line ppf = function
  | Step s ->
      Fmt.pf ppf "    #%d %a" s.index Op.pp s.op;
      Option.iter (Fmt.pf ppf "  <- %s") s.met;
      Fmt.pf ppf "@\n"
  | Repeated { period; times; first; last } ->
      Fmt.pf ppf
        "    ... the %d operation%s above, %d more time%s, #%d to #%d@\n" period
        (plural period) times (plural times) first last

let pp_trace ppf trace =
  let lines = Array.of_list (collapse trace) in
  let n = Array.length lines in
  let rec find i =
    if i >= n then None
    else
      match lines.(i) with
      | Step { met = Some _; _ } -> Some i
      | _ -> find (i + 1)
  in
  let first_fault = find 0 in
  let shown i =
    n <= (2 * keep) + (2 * context) + 1
    || i < keep
    || i >= n - keep
    || match first_fault with Some f -> abs (i - f) <= context | None -> false
  in
  let rec pp i =
    if i < n then (
      if shown i then begin
        pp_line ppf lines.(i);
        pp (i + 1)
      end
      else
        let rec skip j = if j < n && not (shown j) then skip (j + 1) else j in
        let j = skip i in
        Fmt.pf ppf "    ... %d more line%s@\n" (j - i) (plural (j - i));
        pp j)
  in
  if n = 0 then Fmt.pf ppf "    (no operation)@\n" else pp 0

(* A field of a report, its later lines set in under its first. *)
let field ppf (k, v) =
  let rec chop v =
    let n = String.length v in
    if n > 0 && v.[n - 1] = '\n' then chop (String.sub v 0 (n - 1)) else v
  in
  match chop v with
  | "" -> ()
  | v ->
      Fmt.pf ppf "  %-9s %s@\n" (k ^ ":")
        (String.concat "\n            " (String.split_on_char '\n' v))

let fail fmt = Fmt.kstr (fun s -> failwith (String.trim s)) fmt

let first ~name ~printed trace error =
  fail "%s: the run with no fault failed: %s@\n%a  trace:@\n%a" name error field
    ("output", printed) pp_trace trace

let ended_text = function
  | Returned -> "the faulty run returned"
  | Raised x -> "the faulty run raised " ^ exn_text x
  | Cut -> "the power was cut"

let scenario ~name ~replay ?matched ~printed s trace { stage; error } =
  let title, ended =
    match stage with
    | Setup -> ("setup failed", None)
    | Run -> ("the program failed under a fault", None)
    | Lost e -> ("a part lost what it promised, before the restart", Some e)
    | Inspect e -> ("inspect rejected what the fault left", Some e)
    | Restart e ->
        ( "the restart failed: the program, run again with no fault on what \
           the faulty run left, did not return",
          Some e )
  in
  let fields =
    [
      ("faults", Plan.text s);
      ("ended", Option.fold ~none:"" ~some:ended_text ended);
      ("matched", Option.value ~default:"" matched);
      ("error", error);
      ("output", printed);
    ]
  in
  fail "%s: %s@\n%a  trace:@\n%a  replay:   %s" name title
    (Fmt.list ~sep:Fmt.nop field)
    fields pp_trace trace
    (replay (Plan.key s))
