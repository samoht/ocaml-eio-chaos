open! Hermetic
module Op = Op

(* {1 Guards} *)

(* A run that makes more operations spins, or is more than a check plans
   from: it stops there, so its operations stay bounded. *)
let max_ops = 100_000

(* The operations a program may make once the power is cut, before it is
   taken to catch [Eio.Cancel.Cancelled]. *)
let max_after_cut = 1000

(* The failures of one operation, while the clock stands still, that make a
   livelock: 10000 more than three times the first run's count on one
   target, or than 1000 times it for one kind in one directory, as a retry
   under fresh names makes. *)
let streak_limit tag first =
  10_000 + (first * match tag with `Target -> 3 | `Dir -> 1000)

type outcome =
  | Passed
  | Raised of exn
  | Deadlock of string (* the fibers the program waits on *)
  | Hung of string (* why a part saw the program spin *)
  | Time_limit
  | Busy
  | Too_long
  | Kept_going
  | Too_many
  | Livelock of [ `Target | `Dir ] * string * int
  | Over_budget of int (* the operation the first run stopped at *)
  | Restart of exn option * outcome
  (* what the faulty run raised, if anything, and how its restart failed *)
  | Lost of exn option * string
  (* what the faulty run raised, and the promise a part lost *)
  | Inspected of exn option * exn
  (* what the faulty run raised, and what inspect raised *)
  | Multiplied of outcome * string * int
(* a run stopped at a limit, and the condition that split [n] of its
   operations *)

(* A report a nested check raised is text, its lines set in. *)
let exn_text = function
  | Failure m when String.contains m '\n' ->
      String.concat "\n    " (String.split_on_char '\n' m)
  | x -> Printexc.to_string x

let rec outcome_text ~noun = function
  | Passed -> "none"
  | Raised x -> exn_text x
  | Deadlock waits ->
      "the run hangs: every fiber waits and no timer is due" ^ waits
  | Hung why -> "the run hangs: " ^ why
  | Time_limit ->
      "the run hangs: it is still running after an hour of its clock"
  | Busy ->
      "the run woke 10,000,000 times without waiting 1 ms or more: a busy loop"
  | Too_long ->
      "the run took 10,000,000 turns, more than a check can repeat: a busy \
       loop, or a program too large for a check"
  | Kept_going ->
      "the run hangs: it kept going for 1000 operations after the power cut, \
       so it catches Eio.Cancel.Cancelled"
  | Too_many ->
      Fmt.str
        "the run made 100,000 %ss, more than a check plans from: a busy loop, \
         or a program too large for a check"
        noun
  | Livelock (`Target, target, n) ->
      Fmt.str
        "the run hangs: the same operation on %s failed %d times while its \
         clock stood still"
        target n
  | Livelock (`Dir, dir, n) ->
      Fmt.str
        "the run hangs: one kind of operation in %s failed %d times while its \
         clock stood still"
        dir n
  | Over_budget at ->
      Fmt.str "the linear tier needs more runs than the budget by operation #%d"
        at
  | Restart (_, o) -> outcome_text ~noun o
  | Lost (_, s) -> s
  | Inspected (_, x) -> "inspect raised " ^ exn_text x
  | Multiplied (o, name, n) ->
      Fmt.str "%s; %s multiplied its operations, %d of them"
        (outcome_text ~noun o) name n

(* A cut fails the run's switch with it, which cancels every fiber of the
   run. *)
exception Cut

let () =
  Printexc.register_printer (function
    | Cut -> Some "the power was cut"
    | _ -> None)

(* {1 Records} *)

(* What a phase keeps of its operations: the report's lines, its first [cap]
   operations, which the plan of the next scenarios reads, those at [marked]
   places, which the report names, and those that changed what a part
   keeps. *)
(* Where a second run with no fault first differs from the first, as it
   goes: in the part or kind of an operation, or in its target alone. *)
type diff = {
  against : Plan.step array;
  mutable seen : int;
  mutable kinds : (Plan.step option * Plan.step option) option;
  mutable targets : (Plan.step option * Plan.step option) option;
}

type record = {
  lines : Recorder.t option;
  mutable kept : Plan.step list; (* latest first *)
  mutable nkept : int;
  cap : int;
  marked : Plan.step -> bool;
  mutable marks : Plan.step list;
  mutable last : int; (* the last operation's index *)
  ranks : (string * string, int) Hashtbl.t;
  changed : (int, unit) Hashtbl.t;
  diff : diff option;
}

let record ?(lines = true) ?(cap = max_int) ?(marked = fun _ -> false) ?against
    () =
  {
    lines = (if lines then Some (Recorder.v ()) else None);
    kept = [];
    nkept = 0;
    cap;
    marked;
    marks = [];
    last = 0;
    ranks = Hashtbl.create 16;
    changed = Hashtbl.create 16;
    diff =
      Option.map
        (fun against -> { against; seen = 0; kinds = None; targets = None })
        against;
  }

let compare_step d (step : Plan.step) =
  let i = d.seen in
  d.seen <- i + 1;
  if d.kinds = None then
    if i >= Array.length d.against then d.kinds <- Some (None, Some step)
    else
      let x = d.against.(i) in
      if
        not
          (String.equal (Op.device x.op) (Op.device step.op)
          && String.equal (Op.kind x.op) (Op.kind step.op))
      then d.kinds <- Some (Some x, Some step)
      else if
        d.targets = None
        && not (String.equal (Op.target x.op) (Op.target step.op))
      then d.targets <- Some (Some x, Some step)

(* The first difference in kinds, once the second run has ended. *)
let kinds_differ d =
  match d.kinds with
  | Some _ as k -> k
  | None when d.seen < Array.length d.against ->
      Some (Some d.against.(d.seen), None)
  | None -> None

(* The number and the rank of the next operation of [r]. *)
let number r (op : Op.t) =
  let k = (Op.device op, Op.kind op) in
  let rank = 1 + Option.value ~default:0 (Hashtbl.find_opt r.ranks k) in
  Hashtbl.replace r.ranks k rank;
  r.last <- r.last + 1;
  (r.last, rank)

let note r (step : Plan.step) met =
  Option.iter
    (fun l -> Recorder.add l { index = step.index; op = step.op; met })
    r.lines;
  if r.nkept < r.cap then begin
    r.kept <- step :: r.kept;
    r.nkept <- r.nkept + 1
  end;
  if r.marked step then r.marks <- step :: r.marks;
  Option.iter (fun d -> compare_step d step) r.diff

(* The operations kept, in order. *)
let kept r = List.rev r.kept

(* The operation at [p], where the report names it. *)
let found r p =
  match List.find_opt (Plan.at p) r.marks with
  | Some s -> Some s
  | None -> List.find_opt (Plan.at p) (kept r)

(* {1 Streaks} *)

(* An operation: its part and kind, and the target it names, or the
   directory that target is in. *)
type target = string * string * [ `Target | `Dir ] * string

let targets (op : Op.t) : target list =
  [
    (Op.device op, Op.kind op, `Target, Op.target op);
    (Op.device op, Op.kind op, `Dir, Recorder.parent (Op.target op));
  ]

(* How often the first run made each operation. *)
let made (trace : Plan.trace) =
  let t = Hashtbl.create 16 in
  List.iter
    (fun (s : Plan.step) ->
      List.iter
        (fun k ->
          Hashtbl.replace t k
            (1 + Option.value ~default:0 (Hashtbl.find_opt t k)))
        (targets s.op))
    trace;
  t

(* Injected failures of each operation while the run's clock stands still,
   since its last attempt that met no fault. The time operations took does
   not move it: a retry spins whatever operations it makes between two
   failures. *)
type streak = {
  counts : (target, int) Hashtbl.t;
  first : (target, int) Hashtbl.t;
  mutable at : int;
  mutable hit : ([ `Target | `Dir ] * string * int) option;
}

let streak first =
  { counts = Hashtbl.create 16; first; at = min_int; hit = None }

(* {1 The question} *)

type pass = Errors | Cuts
type phase = Setup | Running | Inspecting | Restarting | Again | Ended
type keep = Lose_unsynced | Keep_unsynced | Choose of (int -> int)

(* A part's state, found again by the identity of its type. *)
type any = Any : 'a Type.Id.t * 'a -> any

(* What a fiber does, for the report of a hang: the kind of operation it
   waits in, if any, and the last one it made. *)
type fiber = { mutable inside : string option; mutable last : Plan.step option }

(* What the phases of one scenario share. *)
type state = {
  pass : pass;
  scenario : Plan.scenario;
  noun : string;
  found_parts : any list;
  mutable cut_parts : unit -> (string * string) list;
  mutable phase : phase;
  trace : record;
  rtrace : record;
  mutable index : int; (* the operation the last question numbered *)
  mutable last_op : Op.t option; (* kept once for a run of equal ones *)
  mutable pending : Plan.fault list;
  mutable on : bool; (* the lasting condition holds *)
  streak : streak;
  halted : outcome option ref;
  mutable ops : int;
  mutable splits : int; (* the operations a condition split *)
  mutable restart_ops : int;
  budget : int option;
  mutable floor : int;
  sure : Plan.Sure.t;
  mutable prev : (Plan.step * bool * int) option;
  (* the power *)
  mutable cut : bool;
  mutable after : int;
  mutable kept_run : bool;
  mutable kept_restart : bool;
  mutable cut_at : Plan.step option; (* the operation the cut fell before *)
  mutable cut_lines : (string * string) list;
  mutable stop : Eio.Switch.t option;
  mutable phase_sw : Eio.Switch.t option;
  (* time *)
  mutable mono : Eio.Time.Mono.ty Eio.Resource.t option;
  mutable taken : int; (* the time operations took, in ns *)
  mutable lost : string list; (* latest first *)
  mutable ended : exn option option; (* how a run with a fault ended *)
  mutable recovering : bool; (* the restart after a cut began *)
  mutable place : int; (* the bytes the last fault decided lands after *)
  mutable faulted : bool; (* a fault met in the run *)
  fibers : (int, fiber) Hashtbl.t; (* by trace id *)
  mutable root : int option; (* the program's own fiber *)
  mutable charge : unit -> unit;
}

type t = { st : state option; quiet : bool }

let none = { st = None; quiet = true }

(* Park the fiber where no cancel reaches, so even a loop that catches
   everything stops; the loop then ends the run. *)
let park () = Eio.Cancel.protect Eio.Fiber.await_cancel

let halt st o =
  st.halted := Some o;
  park ()

let count_op st n = if n > max_ops then halt st Too_many
let now_ns mono = Int64.to_int (Mtime.to_uint64_ns (Eio.Time.Mono.now mono))

(* The operation the power cut fell before, and every one after, is
   cancelled; once every fiber waits, so is the run. One that catches the
   cancellation still reaches here, and is stopped. *)
let cancelled st =
  st.after <- st.after + 1;
  if st.after > max_after_cut then begin
    (match st.phase with
    | Restarting -> st.kept_restart <- true
    | _ -> st.kept_run <- true);
    park ()
  end;
  raise (Eio.Cancel.Cancelled Cut)

let failed st (op : Op.t) =
  let s = st.streak in
  let now = match st.mono with Some m -> now_ns m - st.taken | None -> 0 in
  if now <> s.at then begin
    s.at <- now;
    Hashtbl.reset s.counts
  end;
  List.iter
    (fun ((_, _, tag, at) as key) ->
      let n = 1 + Option.value ~default:0 (Hashtbl.find_opt s.counts key) in
      Hashtbl.replace s.counts key n;
      let limit =
        streak_limit tag
          (Option.value ~default:0 (Hashtbl.find_opt s.first key))
      in
      if n >= limit then begin
        s.hit <- Some (tag, at, limit);
        if n >= 2 * limit then park ();
        failwith
          ("Eio_chaos: "
          ^ outcome_text ~noun:st.noun (Livelock (tag, at, limit)))
      end)
    (targets op)

let met_no_fault st (op : Op.t) =
  if Hashtbl.length st.streak.counts > 0 then
    List.iter (Hashtbl.remove st.streak.counts) (targets op)

(* The first run stops once the linear tier needs more runs than the
   budget: past that, the check cannot pass, and the run's operations need
   not grow. *)
let add_floor st ~at n =
  match st.budget with
  | None -> ()
  | Some budget ->
      st.floor <- st.floor + n;
      if st.floor > budget then halt st (Over_budget at)

let find name l n = List.find_opt (fun f -> String.equal (name f) n) l

(* The next pending fault placed at [step], taken off the list. *)
let pending st step =
  match
    List.find_opt (fun (f : Plan.fault) -> Plan.at f.pos step) st.pending
  with
  | Some f ->
      st.pending <- List.filter (fun x -> x != f) st.pending;
      Some f
  | None -> None

(* The fault the scenario puts on [step] of a run with no cut, and the bytes
   into it where it lands: its lasting condition from its place on, or the
   next pending one placed there. *)
let scheduled st (step : Plan.step) ~name ~once ~lasting ~alone ~mild =
  match st.scenario.lasting with
  | Some l ->
      if Plan.at l.pos step then st.on <- true;
      if st.on && String.equal l.pos.device (Op.device step.op) then
        Option.map (fun f -> (f, 0)) (find name lasting l.name)
      else None
  | None ->
      Option.bind (pending st step) (fun (f : Plan.fault) ->
          let l = if f.alone then alone else once @ mild in
          Option.map (fun x -> (x, f.pos.at)) (find name l f.name))

let pending_cut st step ~name ~durable ~alone =
  Option.bind (pending st step) (fun (f : Plan.fault) ->
      find name (if f.alone then alone else durable) f.name)

let met_text ~name ~pp =
  Option.map (fun (f, at) ->
      (name f, Plan.after_text (Fmt.to_to_string pp f) at))

let tid () =
  (Eio.Private.Fiber_context.tid
     (Effect.perform Eio.Private.Effects.Get_context)
    :> int)

let fiber st =
  let k = tid () in
  match Hashtbl.find_opt st.fibers k with
  | Some f -> f
  | None ->
      let f = { inside = None; last = None } in
      Hashtbl.replace st.fibers k f;
      f

let cut_matches (c : Plan.cut option) step =
  match c with Some { before = Some p; _ } -> Plan.at p step | _ -> false

(* A loop makes the same operation many times: its trace keeps one. *)
let shared st (op : Op.t) =
  match st.last_op with
  | Some o
    when Op.same o op
         && Op.structure o = Op.structure op
         && Op.changes o = Op.changes op ->
      o
  | _ ->
      st.last_op <- Some op;
      op

(* The floor of the linear tier of cuts, once the operation before [step] is
   known to have changed what a part keeps or not. *)
let cut_floor st (step : Plan.step) ~durable =
  Option.iter
    (fun ((p : Plan.step), sure, durable) ->
      add_floor st ~at:step.index
        (Plan.cut_floor ~sure
           ~changed:(Hashtbl.mem st.trace.changed p.index)
           ~durable p.op))
    st.prev;
  st.prev <- Some (step, Plan.Sure.add st.sure step.op, durable)

let decide_running st t (op : Op.t) ~name ~pp ~lasting ~durable ~alone ~mild
    ~split once =
  if split then st.splits <- st.splits + 1
  else begin
    st.ops <- st.ops + 1;
    count_op st st.ops
  end;
  let r = st.trace in
  let index, rank = number r op in
  st.index <- index;
  st.place <- 0;
  let q l = if t.quiet then [] else l in
  let once = q once and lasting = q lasting and durable = q durable in
  let alone = q alone and mild = q mild in
  let faults : Plan.faults =
    match (once, lasting, durable, alone, mild) with
    | [], [], [], [], [] -> No_faults
    | _ -> Faults { name; pp; once; lasting; durable; alone; mild }
  in
  let op = shared st op in
  let step = { Plan.index; op; rank; faults; places = [] } in
  (fiber st).last <- Some step;
  match st.pass with
  | Errors -> (
      if Option.is_some st.budget then begin
        let sure = Plan.Sure.add st.sure op in
        add_floor st ~at:index
          (if sure then
             List.length once + List.length lasting + List.length alone
           else 0)
      end;
      let fault = scheduled st step ~name ~once ~lasting ~alone ~mild in
      note r step (met_text ~name ~pp fault);
      match fault with
      | Some (f, at) ->
          st.place <- at;
          st.faulted <- true;
          failed st op;
          Some f
      | None ->
          met_no_fault st op;
          None)
  | Cuts ->
      if Option.is_some st.budget then
        cut_floor st step ~durable:(List.length durable + List.length alone);
      if (not st.cut) && cut_matches st.scenario.cut step then begin
        st.cut_at <- Some step;
        st.cut_lines <- st.cut_parts ();
        st.cut <- true;
        st.stop <- st.phase_sw
      end;
      if st.cut then cancelled st;
      let fault = pending_cut st step ~name ~durable ~alone in
      note r step (met_text ~name ~pp (Option.map (fun f -> (f, 0)) fault));
      if Option.is_some fault then st.faulted <- true;
      fault

let decide_restarting st (op : Op.t) =
  match st.pass with
  | Errors ->
      st.restart_ops <- st.restart_ops + 1;
      count_op st st.restart_ops;
      (fiber st).last <-
        Some { Plan.index = 0; op; rank = 0; faults = No_faults; places = [] };
      None
  | Cuts ->
      st.ops <- st.ops + 1;
      count_op st st.ops;
      let r = st.rtrace in
      let index, rank = number r op in
      st.index <- index;
      let step = { Plan.index; op; rank; faults = No_faults; places = [] } in
      (fiber st).last <- Some step;
      if (not st.cut) && cut_matches st.scenario.rcut step then begin
        ignore (st.cut_parts () : (string * string) list);
        st.cut <- true;
        st.stop <- st.phase_sw
      end;
      if st.cut then cancelled st;
      note r step None;
      None

let decide t op ~name ?(pp = fun ppf f -> Fmt.string ppf (name f))
    ?(lasting = []) ?(durable = []) ?(alone = []) ?(mild = []) ?(split = false)
    once =
  match t.st with
  | None -> None
  | Some st -> (
      match st.phase with
      | Setup | Inspecting | Again | Ended -> None
      | Running ->
          decide_running st t op ~name ~pp ~lasting ~durable ~alone ~mild ~split
            once
      | Restarting -> decide_restarting st op)

let index t = match t.st with Some st -> st.index | None -> 0

let place t =
  match t.st with Some { phase = Running; place; _ } -> place | _ -> 0

let places t i l =
  let add (r : record) =
    let rec go = function
      | (s : Plan.step) :: rest ->
          if s.index = i then s.places <- l @ s.places
          else if s.index > i then go rest
      | [] -> ()
    in
    go r.kept
  in
  match t.st with
  | Some ({ phase = Running; _ } as st) when l <> [] -> add st.trace
  | Some _ | None -> ()

let faulted t =
  match t.st with Some { phase = Running; faulted; _ } -> faulted | _ -> false

let turn t =
  match t.st with
  | Some ({ phase = Running | Restarting | Again; _ } as st) ->
      st.charge ();
      st.taken <- st.taken + 1000
  | Some _ | None -> ()

let inside t kind f =
  match t.st with
  | Some ({ phase = Running | Restarting | Again; _ } as st) ->
      let fb = fiber st in
      let before = fb.inside in
      fb.inside <- Some kind;
      Fun.protect ~finally:(fun () -> fb.inside <- before) f
  | Some _ | None -> f ()

let hangs t why =
  match t.st with
  | Some ({ phase = Running | Restarting | Again; _ } as st) ->
      halt st (Hung why)
  | Some _ | None -> failwith why

let changed t i =
  match t.st with
  | Some ({ phase = Running; _ } as st) -> Hashtbl.replace st.trace.changed i ()
  | Some ({ phase = Restarting; _ } as st) ->
      Hashtbl.replace st.rtrace.changed i ()
  | Some _ | None -> ()

let spend t span =
  match t.st with
  | Some ({ phase = Running | Restarting | Again; _ } as st) ->
      Option.iter
        (fun m ->
          let t0 = now_ns m in
          Eio.Time.Mono.sleep_span m span;
          st.taken <- st.taken + (now_ns m - t0))
        st.mono;
      if st.cut then cancelled st
  | Some _ | None -> ()

let lost t what =
  match t.st with
  | Some st -> st.lost <- what :: st.lost
  | None -> invalid_arg "Eio_chaos.lost: none is no run"

(* {1 Parts} *)

type os = [ `Linux | `Macos | `Windows ]

type capability =
  | Fs of Eio.Fs.dir_ty Eio.Path.t * Eio.Fs.dir_ty Eio.Path.t
  | Net of [ `Generic ] Eio.Net.ty Eio.Resource.t

module type PART = sig
  type chaos
  type t

  val name : string
  val noun : string

  val start :
    t ->
    os:os ->
    sw:Eio.Switch.t ->
    clock:float Eio.Time.clock_ty Eio.Resource.t ->
    mono_clock:Eio.Time.Mono.ty Eio.Resource.t ->
    chaos ->
    capability

  val run : t -> cut:keep option -> faults:string list -> capability
  val cut : t -> (string * string) list
  val restart : t -> power:bool -> cut:keep option -> capability
  val pp_op : Op.t Fmt.t
  val indeterminate : exn -> bool
end
with type chaos := t

type part =
  | Part : {
      m : (module PART with type t = 'a);
      state : 'a;
      quiet : bool;
      id : 'a Type.Id.t;
    }
      -> part

let live st = st.phase <> Ended

module Part (P : PART) = struct
  let id : P.t Type.Id.t = Type.Id.make ()
  let v state = Part { m = (module P); state; quiet = false; id }

  let state : any -> P.t option =
   fun (Any (i, s)) ->
    match Type.Id.provably_equal i id with Some Equal -> Some s | None -> None

  let find t =
    match t.st with
    | Some st when live st -> List.find_map state st.found_parts
    | Some _ | None -> None
end

let quiet (Part p) = Part { p with quiet = true }
let part_name (Part { m = (module P); _ }) = P.name

(* {1 The env} *)

type env =
  < stdin : Eio.Flow.source_ty Eio.Resource.t
  ; stdout : Eio.Flow.sink_ty Eio.Resource.t
  ; stderr : Eio.Flow.sink_ty Eio.Resource.t
  ; net : [ `Generic ] Eio.Net.ty Eio.Resource.t
  ; domain_mgr : Eio.Domain_manager.ty Eio.Resource.t
  ; process_mgr : [ `Generic ] Eio.Process.mgr_ty Eio.Resource.t
  ; clock : float Eio.Time.clock_ty Eio.Resource.t
  ; mono_clock : Eio.Time.Mono.ty Eio.Resource.t
  ; fs : Eio.Fs.dir_ty Eio.Path.t
  ; cwd : Eio.Fs.dir_ty Eio.Path.t
  ; secure_random : Eio.Flow.source_ty Eio.Resource.t
  ; debug : Eio.Debug.t
  ; backend_id : string
  ; chaos : t >

(* What the envs of one scenario share. *)
type world = {
  mutable envs : int;
  out : Output.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  mono : Eio.Time.Mono.ty Eio.Resource.t;
  chaos : t;
}

(* The seed of the [n]th env of a scenario: of the default [Random] state,
   and of [secure_random]. *)
let seed n = [| 0x5eed; n |]

(* The next env of [w], on the capabilities [caps]. Its [secure_random] and
   the default [Random] state, which it resets, are seeded by its place among
   the envs of the scenario, so the [n]th env of each scenario draws the same
   bytes. *)
let env w caps : env =
  let n = w.envs in
  w.envs <- n + 1;
  Random.full_init (seed n);
  let entropy =
    Stdenv.entropy (Random.State.make (Array.append (seed n) [| 1 |]))
  in
  let fs, cwd =
    List.find_map (function Fs (f, c) -> Some (f, c) | Net _ -> None) caps
    |> Option.value ~default:(Stdenv.dir "env#fs", Stdenv.dir "env#cwd")
  in
  let net =
    List.find_map (function Net n -> Some n | Fs _ -> None) caps
    |> Option.value ~default:Stdenv.net
  in
  let output = Stdenv.sink w.out in
  object
    method stdin = Stdenv.stdin
    method stdout = output
    method stderr = output
    method net = net
    method domain_mgr = Stdenv.domain_mgr
    method process_mgr = Stdenv.process_mgr
    method clock = w.clock
    method mono_clock = w.mono
    method fs = fs
    method cwd = cwd
    method secure_random = entropy
    method debug = Eio.Private.Debug.v
    method backend_id = "chaos"
    method chaos = w.chaos
  end

let check_capabilities name caps =
  let count p = List.length (List.filter p caps) in
  if count (function Fs _ -> true | Net _ -> false) > 1 then
    Fmt.invalid_arg "%s: two parts give env#fs" name;
  if count (function Net _ -> true | Fs _ -> false) > 1 then
    Fmt.invalid_arg "%s: two parts give env#net" name

(* {1 Runs} *)

type config = {
  name : string;
  args : string;
  os : os;
  seed : int;
  setup : env -> unit;
  inspect : env -> unit;
  app : env -> unit;
  parts : part list;
  single : bool; (* one part: keys and lines name no device *)
  noun : string;
}

(* A faulty run may end with [Eio.Io], or with what a failure of one of its
   parts makes a program raise, from one fiber or several. *)
let rec is_io parts = function
  | Eio.Io _ -> true
  | Eio.Exn.Multiple l ->
      l <> [] && List.for_all (fun (x, _) -> is_io parts x) l
  | x ->
      List.exists (fun (Part { m = (module P); _ }) -> P.indeterminate x) parts

(* A run ends once [halted] holds why. A hang says which fibers wait, as
   [waits] tells of their trace ids. *)
let in_loop ?idle ?(waits = fun _ -> "") ~halted main =
  match
    Quiet_mock.run ?idle
      ~effects:{ handle = Systhread.handle }
      ~stop:(fun () -> Option.is_some !halted)
      ~limit:3600. main
  with
  | v -> Ok v
  | exception Quiet_mock.Stopped -> Error (Option.get !halted)
  | exception Quiet_mock.Deadlock_detected tids -> Error (Deadlock (waits tids))
  | exception Quiet_mock.Time_limit -> Error Time_limit
  | exception Quiet_mock.Busy -> Error Busy
  | exception Quiet_mock.Too_long -> Error Too_long

(* What a run with a cut keeps, its draws going to [trail]. *)
let keep_of trail : Plan.state -> keep = function
  | Drop -> Lose_unsynced
  | Keep -> Keep_unsynced
  | Enum v | Shape v ->
      let v = Array.of_list v and pos = ref 0 in
      Choose
        (fun n ->
          let c = if !pos < Array.length v then v.(!pos) else 0 in
          incr pos;
          trail := (c, n) :: !trail;
          if 0 <= c && c < n then c else 0)
  | Seed s ->
      let rng = Random.State.make [| s |] in
      Choose
        (fun n ->
          let c = Random.State.int rng n in
          trail := (c, n) :: !trail;
          c)

(* Whether the cut allows [st], given the draws it made: no choice out of
   range, and no choice for a draw it did not make. *)
let allows (st : Plan.state) trail =
  match st with
  | Enum v | Shape v ->
      List.for_all (fun (c, n) -> 0 <= c && c < n) trail
      && List.length v <= List.length trail
  | Drop | Keep | Seed _ -> true

let names_of_device (s : Plan.scenario) device =
  List.filter_map
    (fun (f : Plan.fault) ->
      if String.equal f.pos.device device then Some f.name else None)
    (s.faults @ Option.to_list s.lasting)

(* The program in a switch a cut fails once every fiber waits. *)
let in_phase st f =
  match
    Eio.Switch.run (fun sw ->
        st.phase_sw <- Some sw;
        f ())
  with
  | () ->
      st.stop <- None;
      Passed
  | exception _ when st.cut ->
      st.stop <- None;
      Passed
  | exception x -> Raised x

(* The error of a run with a cut. *)
type error =
  | Program of outcome
  | Settle of outcome (* a promise lost, or inspect raised *)
  | Recover of outcome
  | Again_failed of exn

type run = {
  trace : record;
  rtrace : record;
  outcome : outcome; (* with no cut *)
  error : error option; (* with a cut *)
  cut_at : Plan.step option;
  cut_lines : (string * string) list;
  trail : (int * int) list;
  allowed : bool;
}

let scenario_state cfg ~pass ?cap ?budget ?against ~first (s : Plan.scenario) =
  let marked step =
    List.exists (fun (f : Plan.fault) -> Plan.at f.pos step) s.faults
    || match s.lasting with Some l -> Plan.at l.pos step | None -> false
  in
  {
    pass;
    scenario = s;
    noun = cfg.noun;
    found_parts =
      List.map (fun (Part { state; id; _ }) -> Any (id, state)) cfg.parts;
    cut_parts = (fun () -> []);
    phase = Setup;
    trace = record ?cap ~marked ?against ();
    rtrace = record ?cap ~lines:false ();
    index = 0;
    last_op = None;
    pending = s.faults;
    on = false;
    streak = streak first;
    halted = ref None;
    ops = 0;
    restart_ops = 0;
    budget;
    floor = 0;
    sure = Plan.Sure.v ();
    prev = None;
    cut = false;
    after = 0;
    kept_run = false;
    kept_restart = false;
    cut_at = None;
    cut_lines = [];
    stop = None;
    phase_sw = None;
    mono = None;
    taken = 0;
    lost = [];
    ended = None;
    recovering = false;
    splits = 0;
    place = 0;
    faulted = false;
    fibers = Hashtbl.create 8;
    root = None;
    charge = ignore;
  }

(* What the phases of one scenario's loop share. *)
type ctx = { cfg : config; st : state; w : world; rkeep : keep option }

(* What each part says of a cut, labelled with its name once a run has two
   parts. *)
let cut_parts cfg () =
  List.concat_map
    (fun (Part { m = (module P); state; _ }) ->
      let l = P.cut state in
      if cfg.single then l else List.map (fun (k, v) -> (P.name ^ " " ^ k, v)) l)
    cfg.parts

let restart_parts cfg ~power ~cut =
  List.map
    (fun (Part { m = (module P); state; _ }) -> P.restart state ~power ~cut)
    cfg.parts

let first_lost st = match List.rev st.lost with [] -> None | l :: _ -> Some l
let run_app c caps = c.cfg.app (env c.w caps)

(* After a run with a fault and no cut: each part restarted, [inspect], then
   the program again. *)
let restart_errors c e =
  c.st.phase <- Inspecting;
  let caps = restart_parts c.cfg ~power:false ~cut:None in
  match first_lost c.st with
  | Some l -> Lost (e, l)
  | None -> (
      match c.cfg.inspect (env c.w caps) with
      | exception x -> Inspected (e, x)
      | () -> (
          c.st.ended <- Some e;
          Output.mark c.w.out "-- restart --";
          c.st.phase <- Restarting;
          match run_app c caps with () -> Passed | exception x -> Raised x))

let run_errors c caps =
  let s = c.st.scenario in
  let faulty = s.faults <> [] || s.lasting <> None in
  match run_app c caps with
  | () when faulty -> restart_errors c None
  | () -> Passed
  | exception x when faulty && is_io c.cfg.parts x -> restart_errors c (Some x)
  | exception x -> Raised x

(* What a cut left: a promise a part lost, or what [inspect] says of it. *)
let settle c caps =
  match first_lost c.st with
  | Some l -> Some (Settle (Lost (None, l)))
  | None -> (
      c.st.phase <- Inspecting;
      match c.cfg.inspect (env c.w caps) with
      | () -> None
      | exception x -> Some (Settle (Inspected (None, x))))

(* After a cut during the restart: the program once more. *)
let again c =
  c.st.phase <- Inspecting;
  let caps = restart_parts c.cfg ~power:true ~cut:None in
  match settle c caps with
  | Some e -> Some e
  | None -> (
      Output.mark c.w.out "-- restart --";
      c.st.cut <- false;
      c.st.phase <- Again;
      match run_app c caps with
      | () -> None
      | exception x -> Some (Again_failed x))

(* The restart after a cut, in which a second cut may fall. *)
let recover c =
  let st = c.st in
  st.phase <- Inspecting;
  let caps = restart_parts c.cfg ~power:true ~cut:c.rkeep in
  match settle c caps with
  | Some e -> Some e
  | None -> (
      st.recovering <- true;
      st.cut <- false;
      st.after <- 0;
      Output.mark c.w.out "-- restart --";
      st.phase <- Restarting;
      let r = in_phase st (fun () -> run_app c caps) in
      match ((if st.kept_restart then Kept_going else r), st.scenario.rcut) with
      | Passed, None -> None
      | Passed, Some _ ->
          if not st.cut then
            ignore (cut_parts c.cfg () : (string * string) list);
          again c
      | o, _ -> Some (Recover o))

let run_cuts c caps =
  let st = c.st in
  let o = in_phase st (fun () -> run_app c caps) in
  if not st.cut then begin
    st.cut_lines <- cut_parts c.cfg ();
    st.cut <- true
  end;
  match if st.kept_run then Kept_going else o with
  | Passed -> recover c
  | Raised x when st.scenario.faults <> [] && is_io c.cfg.parts x -> recover c
  | o -> Some (Program o)

(* The program's run in its loop, after [setup] on new parts. *)
let in_scenario cfg (st : state) ~trail ~rtrail out clock mono charge =
  st.mono <- Some mono;
  st.charge <- charge;
  st.root <- Some (tid ());
  Eio.Fiber.with_binding Eio.Private.Debug.v#traceln (Stdenv.traceln out)
  @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let chaos quiet = { st = Some st; quiet } in
  let w = { envs = 0; out; clock; mono; chaos = chaos false } in
  let caps =
    List.map
      (fun (Part { m = (module P); state; quiet; _ }) ->
        P.start state ~os:cfg.os ~sw ~clock ~mono_clock:mono (chaos quiet))
      cfg.parts
  in
  check_capabilities cfg.name caps;
  cfg.setup (env w caps);
  st.cut_parts <- cut_parts cfg;
  let s = st.scenario in
  let keep trail c =
    Option.map (fun (c : Plan.cut) -> keep_of trail c.state) c
  in
  let caps =
    List.map
      (fun (Part { m = (module P); state; _ }) ->
        P.run state ~cut:(keep trail s.cut) ~faults:(names_of_device s P.name))
      cfg.parts
  in
  let c = { cfg; st; w; rkeep = keep rtrail s.rcut } in
  st.phase <- Running;
  match st.pass with
  | Errors -> (run_errors c caps, None)
  | Cuts -> (Passed, run_cuts c caps)

(* How a scenario ended, from how its loop did. *)
let verdict st = function
  | Ok (o, e) -> (o, e)
  | Error o -> (
      match st.pass with
      | Errors -> (o, None)
      | Cuts when st.kept_run -> (Passed, Some (Program Kept_going))
      | Cuts when st.kept_restart -> (Passed, Some (Recover Kept_going))
      | Cuts when st.recovering -> (Passed, Some (Recover o))
      | Cuts -> (Passed, Some (Program o)))

(* A run with no cut fails as its loop halted, as a livelock, or as its
   restart failed. *)
let errors_outcome st outcome =
  let multiplied o =
    match (o, st.scenario.lasting) with
    | (Busy | Too_long | Too_many), Some l when st.splits > 0 ->
        Multiplied (o, l.name, st.splits)
    | _ -> o
  in
  let restarted o =
    let o = multiplied o in
    match st.ended with Some e when o <> Passed -> Restart (e, o) | _ -> o
  in
  match (!(st.halted), st.streak.hit) with
  | Some o, _ -> restarted o
  | None, Some (t, p, n) -> Livelock (t, p, n)
  | None, None -> restarted outcome

let pp_op cfg ppf (op : Op.t) =
  let pp =
    List.find_map
      (fun (Part { m = (module P); _ }) ->
        if String.equal P.name (Op.device op) then Some P.pp_op else None)
      cfg.parts
    |> Option.value ~default:Op.pp
  in
  if cfg.single then pp ppf op else Fmt.pf ppf "%s %a" (Op.device op) pp op

let pp_step cfg ppf (e : Recorder.entry) =
  Fmt.pf ppf "#%d %a" e.index (pp_op cfg) e.op

let step_text cfg (s : Plan.step) =
  Fmt.to_to_string (pp_step cfg)
    { Recorder.index = s.index; op = s.op; met = None }

(* What a hang adds when the program's own fiber waits in no operation on
   other fibers that still run, as a switch whose body returned waits on the
   fibers it forked: those fibers, by what they do. *)
let waits cfg st tids =
  let tids = List.map (fun (t : Eio.Private.Trace.id) -> (t :> int)) tids in
  let is_root t = st.root = Some t in
  let others = List.filter (fun t -> not (is_root t)) tids in
  let busy t =
    match Hashtbl.find_opt st.fibers t with
    | Some { inside = Some _; _ } -> true
    | _ -> false
  in
  let named = 5 in
  match others with
  | [] -> ""
  | _ when not (List.exists is_root tids) -> ""
  | _ when busy (Option.get st.root) -> ""
  | _ ->
      let article k =
        match k.[0] with
        | 'a' | 'e' | 'i' | 'o' | 'u' -> "an " ^ k
        | _ | (exception Invalid_argument _) -> "a " ^ k
      in
      (* The restart after a fault numbers no operation. *)
      let last (s : Plan.step) =
        if s.index = 0 then Fmt.to_to_string (pp_op cfg) s.op
        else step_text cfg s
      in
      let fiber t =
        match Hashtbl.find_opt st.fibers t with
        | Some { inside = Some k; last = Some s } ->
            Fmt.str "a fiber in %s, last %s" (article k) (last s)
        | Some { inside = Some k; last = None } ->
            Fmt.str "a fiber in %s" (article k)
        | Some { inside = None; last = Some s } ->
            Fmt.str "a fiber in no %s, last %s" cfg.noun (last s)
        | Some { inside = None; last = None } | None ->
            Fmt.str "a fiber that made no %s" cfg.noun
      in
      let n = List.length others in
      Fmt.str
        ". The program's own fiber waits on %d fiber%s still running, as a \
         switch does once its body returned: %s%s"
        n
        (if n = 1 then "" else "s")
        (String.concat "; "
           (List.map fiber (List.filteri (fun i _ -> i < named) others)))
        (if n > named then Fmt.str "; and %d more" (n - named) else "")

let exec cfg ~pass ?cap ?budget ?against ~first (s : Plan.scenario) =
  let st = scenario_state cfg ~pass ?cap ?budget ?against ~first s in
  let out = Output.v () and trail = ref [] and rtrail = ref [] in
  let idle () =
    match st.stop with
    | Some sw ->
        st.stop <- None;
        Eio.Switch.fail sw Cut;
        true
    | None -> false
  in
  let result =
    in_loop ~idle ~waits:(waits cfg st) ~halted:st.halted
      (in_scenario cfg st ~trail ~rtrail out)
  in
  st.phase <- Ended;
  let outcome, error = verdict st result in
  let outcome =
    match pass with Cuts -> outcome | Errors -> errors_outcome st outcome
  in
  let allows_cut c trail =
    Option.fold ~none:true ~some:(fun (c : Plan.cut) -> allows c.state trail) c
  in
  ( {
      trace = st.trace;
      rtrace = st.rtrace;
      outcome;
      error;
      cut_at = st.cut_at;
      cut_lines = st.cut_lines;
      trail = !trail;
      allowed = allows_cut s.cut !trail && allows_cut s.rcut !rtrail;
    },
    out )

(* {1 Reports} *)

let fail ~name s = Report.fail (Report.title name s)
let failf cfg fmt = Fmt.kstr (fail ~name:cfg.name) fmt

(* What the program printed, a line of a report, its lines set in. *)
let output out =
  let s = String.trim (Output.printed out) in
  ("output", String.concat "\n            " (String.split_on_char '\n' s))

(* What a replay string spells the kernel as, after [NAMES], the argument
   that passes it and how a report names it. *)
let os_key = function
  | `Linux -> ""
  | `Macos -> "/macos"
  | `Windows -> "/windows"

let os_arg = function
  | `Linux -> ""
  | `Macos -> " ~os:`Macos"
  | `Windows -> " ~os:`Windows"

let os_name = function
  | `Linux -> "Linux"
  | `Macos -> "macOS"
  | `Windows -> "Windows"

(* [NAMES/HASH/KEY], with [/macos] or [/windows] after [NAMES] for a check
   under macOS or Windows. *)
let replay_string cfg hash key =
  let names = String.concat "+" (List.map part_name cfg.parts) in
  Fmt.str "%s%s/%s/%s" names (os_key cfg.os) hash key

(* The trace of a run, if it has an operation to show. *)
let steps cfg (t : record) =
  match t.lines with
  | Some l -> Recorder.steps (Fmt.to_to_string (pp_step cfg)) l
  | None -> []

(* The report of a failing run: the scenario's lines, the trace, and the
   replay that runs it alone. *)
let report cfg ~title ~hash ~key ~matched ~diverged ~(trace : record) ~out
    ~error lines =
  let matched =
    Option.map
      (fun old ->
        Fmt.str "the replay named first run %s, and the program's is now %s" old
          hash)
      matched
  in
  let fields =
    lines
    @ Option.fold ~none:[] ~some:(fun m -> [ ("matched", m) ]) matched
    @ Option.fold ~none:[] ~some:(fun d -> [ ("diverged", d) ]) diverged
    @ [ ("error", error); output out ]
  in
  let replay =
    Fmt.str "%s%s ~replay:%S %s" cfg.name (os_arg cfg.os)
      (replay_string cfg hash key)
      cfg.args
  in
  Report.fail
    {
      name = cfg.name;
      title;
      fields;
      trace = steps cfg trace;
      replay = Some replay;
    }

(* The first run failed, or stopped at the budget: its trace says where. *)
let refuse_first cfg ~(trace : record) ~out o ~budget ~what =
  let title =
    match o with
    | Over_budget at ->
        Fmt.str
          "the linear tier needs more than the budget of %d runs by operation \
           #%d of the first run, which stopped there, and a check never passes \
           on a partial one. Shrink the program, or raise ~budget."
          budget at
    | o ->
        Fmt.str "the run with no %s failed: %s" what
          (outcome_text ~noun:cfg.noun o)
  in
  Report.fail
    {
      name = cfg.name;
      title;
      fields = [ output out ];
      trace = steps cfg trace;
      replay = None;
    }

let no_longer cfg ~hash (h, k) =
  if h = hash then
    failf cfg "the replay names %s, a scenario this check no longer has" k
  else
    failf cfg
      "the replay names %s, a scenario this check no longer has: its first run \
       was %s and is now %s. Run the check without ~replay."
      k h hash

let digest cfg (trace : Plan.trace) =
  let s = String.concat ";" (List.map (step_text cfg) trace) in
  String.sub (Digest.to_hex (Digest.string s)) 0 8

(* Two runs with no fault, [a] and [b], must make the same kinds of operation
   in the same order, for a fault to find its operation again: else the
   check fails. Where only their targets differ, a report says so. *)
let diverge cfg (r : record) =
  let op = function Some s -> step_text cfg s | None -> "nothing more" in
  let d = Option.get r.diff in
  match kinds_differ d with
  | Some (x, y) ->
      failf cfg
        "two runs with no fault made different operations: %s in the first, %s \
         in the second. A check aims each fault at an operation by its kind \
         and its place in the run, so it cannot aim them here. The program \
         reached something the check does not simulate: Random.self_init, a \
         process id, the wall clock read through Unix, a second domain, or \
         state an earlier run left in memory. Built with hermetic (open! \
         Hermetic), the first four do not compile."
        (op x) (op y)
  | None ->
      Option.map
        (fun (x, y) ->
          Fmt.str
            "two runs with no fault named different targets: %s in the first, \
             %s in the second. Faults are aimed by kind and place, so the \
             check went on."
            (op x) (op y))
        d.targets

let at cfg r p = Option.map (step_text cfg) (found r p)

(* {1 Runs with no cut} *)

(* How a restart failed, for a report's title: it raised, or it hung. *)
let restart_title ~left o =
  let how =
    match o with
    | Raised x -> (
        match exn_text x with
        | t when String.contains t '\n' -> "raised an exception"
        | t -> "raised " ^ t)
    | _ -> "hung"
  in
  Fmt.str
    "the restart failed: the program, run again with no fault on what %s left, \
     %s"
    left how

(* The report of a scenario whose run failed, or whose restart did. *)
let failed_check cfg ~hash ~matched ~diverged ~(run : run) ~out
    (s : Plan.scenario) =
  let faults =
    ("faults", Plan.text ~single:cfg.single ~at:(at cfg run.trace) s)
  in
  let report =
    report cfg ~hash
      ~key:(Plan.key ~single:cfg.single s)
      ~matched ~diverged ~trace:run.trace ~out
  in
  let ended e =
    ( "ended",
      match e with
      | None -> "the faulty run returned"
      | Some x -> "the faulty run raised " ^ exn_text x )
  in
  let text = outcome_text ~noun:cfg.noun in
  match run.outcome with
  | Restart (e, o) ->
      report
        ~title:(restart_title ~left:"the faulty run" o)
        ~error:(text o)
        [ faults; ended e ]
  | Lost (e, l) ->
      report ~title:("a fault lost " ^ l) ~error:"" [ faults; ended e ]
  | Inspected (e, _) as o ->
      report ~title:"inspect rejected what the fault left" ~error:(text o)
        [ faults; ended e ]
  | o ->
      report ~title:"the program failed under a fault" ~error:(text o)
        [ faults ]

(* The scenario a replay's key [k] names: a linear one, another single
   fault, or a second fault a linear one's run allows. *)
let replayed cfg ~exec ~rng ~changes ~first linear k =
  let find l =
    List.find_opt (fun s -> String.equal (Plan.key ~single:cfg.single s) k) l
  in
  match find linear with
  | Some _ as s -> s
  | None -> (
      let total, pick = Plan.others first in
      match find (pick rng total) with
      | Some _ as s -> s
      | None ->
          List.find_map
            (fun (s : Plan.scenario) ->
              match s.faults with
              | [ one ]
                when s.lasting = None
                     && String.starts_with
                          ~prefix:(Plan.key ~single:cfg.single s ^ "+")
                          k ->
                  let r = exec s in
                  let changed, rest =
                    Plan.seconds ~rng one ~first:changes ~after:(kept r.trace)
                  in
                  find (changed @ rest)
              | _ -> None)
            linear)

(* The linear tier whole, then the sampled one: other single faults and the
   best second faults the linear runs allowed, each class with its share of
   the budget. *)
let run_tiers cfg ~budget ~rng ~changes ~first ~attempt linear =
  let n = List.length linear in
  (* Faults tried alone are linear runs the sampled tier does not grow
     with. *)
  let n_share =
    List.length (List.filter (fun s -> not (Plan.is_alone s)) linear)
  in
  if n > budget then
    failf cfg
      "the linear tier needs %d runs, more than the budget of %d, and a check \
       never passes on a partial one. Shrink the program, or raise ~budget."
      n budget;
  let others, pick_others = Plan.others first in
  let share classes = Plan.sampled_share ~budget ~linear:n_share ~classes in
  (* The second faults each run allows, drawn as it ends, so no run's
     operations outlive it: the best of each, round by round. *)
  let sh = share (if others = 0 then 1 else 2) in
  let best = Plan.Pairs.v sh in
  List.iter
    (fun (s : Plan.scenario) ->
      let r = attempt ~keep:true s in
      match s.faults with
      | [ one ] when (not one.alone) && s.lasting = None ->
          Plan.Pairs.add best
            (Plan.seconds ~limit:sh ~rng one ~first:changes
               ~after:(kept r.trace))
      | _ -> ())
    linear;
  let pairs = Plan.Pairs.to_list best in
  let classes =
    List.filter
      (fun (n, _) -> n > 0)
      [
        (others, pick_others rng);
        (List.length pairs, fun n -> List.take n pairs);
      ]
  in
  let share = share (List.length classes) in
  List.iter
    (fun (_, pick) ->
      List.iter (fun s -> ignore (attempt ~keep:false s : run)) (pick share))
    classes

let candidates (trace : Plan.trace) =
  List.exists
    (fun (s : Plan.step) ->
      match s.faults with No_faults -> false | Faults _ -> true)
    trace

(* The runs with a fault and no cut, from the first run's operations. *)
let errors cfg ~budget ~replay first =
  let rng = Random.State.make [| cfg.seed |] in
  (* A run keeps no more operations than four times the first run made:
     past that, the plan of the next scenarios reads its first ones. *)
  let n = List.length first in
  let counts = made first in
  let full ?(cap = (4 * n) + 10_000) ?against s =
    exec cfg ~pass:Errors ~cap ?against ~first:counts s
  in
  let diverged =
    diverge cfg
      (fst (full ~cap:0 ~against:(Array.of_list first) Plan.none)).trace
  in
  let hash = digest cfg first in
  let changes = Plan.Changes.v first in
  let linear = Plan.linear first in
  (* Only a run whose operations lead to other scenarios keeps them. *)
  let attempt ?matched ~keep s =
    let r, out = full ?cap:(if keep then None else Some 0) s in
    if r.outcome <> Passed then
      failed_check cfg ~hash ~matched ~diverged ~run:r ~out s;
    r
  in
  match replay with
  | Some ((h, k) as r) -> (
      (* After a fix the first run may differ: the scenario is found by the
         operations it names, which its key carries. *)
      let exec s = fst (full s) in
      match replayed cfg ~exec ~rng ~changes ~first linear k with
      | Some s ->
          let matched = if h = hash then None else Some h in
          ignore (attempt ?matched ~keep:false s : run)
      | None -> no_longer cfg ~hash r)
  | None ->
      run_tiers cfg ~budget ~rng ~changes ~first
        ~attempt:(fun ~keep s -> attempt ~keep s)
        linear

(* The pass with no cut. It is [true] iff an operation a part may fail may
   also change what a part keeps, so the pass with a cut has work to do. *)
let check_errors cfg ~budget ~replay =
  (* A replay runs one scenario, whatever the budget. *)
  let first, out =
    exec cfg ~pass:Errors
      ?budget:(if replay = None then Some budget else None)
      ~first:(Hashtbl.create 1) Plan.none
  in
  if first.outcome <> Passed then
    refuse_first cfg ~trace:first.trace ~out first.outcome ~budget ~what:"fault";
  let first = kept first.trace in
  (* With every part quiet, or none, the check is the first run alone. *)
  let loud = List.exists (fun (Part p) -> not p.quiet) cfg.parts in
  if first = [] && loud then
    failf cfg "the program makes no %s, so it tests nothing" cfg.noun;
  let some = candidates first in
  if some || replay <> None then errors cfg ~budget ~replay first;
  some
  && List.exists
       (fun (s : Plan.step) ->
         match Op.changes s.op with Never -> false | Maybe | Always -> true)
       first

(* {1 Runs with a cut} *)

let state_of (s : Plan.scenario) =
  match s.cut with Some c -> c.state | None -> Drop

let with_state (s : Plan.scenario) state =
  { s with cut = Option.map (fun (c : Plan.cut) -> { c with state }) s.cut }

let cut_lines cfg (s : Plan.scenario) (r : run) =
  let single = cfg.single in
  let at p =
    match r.cut_at with
    | Some step when Plan.at p step -> Some (step_text cfg step)
    | _ -> at cfg r.trace p
  in
  let cut =
    match s.cut with Some c -> Plan.cut_text ~single ~at c | None -> ""
  in
  [
    ("cut", cut);
    ("faults", Plan.faults_text ~single ~at s.faults);
    ( "recovery",
      match s.rcut with
      | None -> ""
      | Some c ->
          Fmt.str "cut in the restart %s, then restart again"
            (Plan.cut_text ~single c) );
  ]
  @ r.cut_lines

(* The title of a failed run's report, and its error. *)
let error_text cfg = function
  | Program o -> ("the program failed", outcome_text ~noun:cfg.noun o)
  | Settle (Lost (_, s)) -> ("a power cut lost " ^ s, "")
  | Settle o ->
      ("inspect rejected what the cut left", outcome_text ~noun:cfg.noun o)
  | Recover o -> (restart_title ~left:"the cut" o, outcome_text ~noun:cfg.noun o)
  | Again_failed x ->
      ( restart_title ~left:"a cut during the previous restart" (Raised x),
        exn_text x )

(* A state a cut allows, named by key [k] on top of a cut of the plan. *)
let state_of_cut cfg cuts k =
  match Plan.split_state_key k with
  | None -> None
  | Some (k', st) ->
      Option.map
        (fun s -> with_state s st)
        (List.find_opt
           (fun s -> String.equal (Plan.key ~single:cfg.single s) k')
           cuts)

(* A state drawn at random is one of a cut that allows more than two states,
   which the check may not try one by one. *)
let drawn ~exec (s : Plan.scenario) =
  let more st = Plan.next_choices (exec (with_state s st)).trail in
  match state_of s with
  | Seed _ | Shape _ -> (
      match more (Enum []) with
      | None -> false
      | Some v -> Option.is_some (more (Enum v)))
  | Drop | Keep | Enum _ -> true

(* A shape is named by the choices its run made, which a replay makes
   again. *)
let renamed (s : Plan.scenario) (r : run) =
  match state_of s with
  | Shape _ ->
      with_state s
        (Shape (List.rev_map (fun (c, n) -> if c < n then c else 0) r.trail))
  | _ -> s

(* What the runs of the pass with a cut share, once its first run is known. *)
type cuts = {
  ccfg : config;
  rng : Random.State.t;
  budget : int;
  hash : string;
  first_ops : Plan.trace;
  changes : Plan.Changes.t;
  changed : Plan.step -> bool;
  cuts : Plan.scenario list;
  faults : Plan.scenario list;
  exec : Plan.scenario -> run;
  attempt : ?matched:string -> ?keep:bool -> Plan.scenario -> run;
  runs : int ref;
}

let rchanged (r : run) (s : Plan.step) = Hashtbl.mem r.rtrace.changed s.index

let rcuts (s : Plan.scenario) r =
  Plan.rcuts ~changed:(rchanged r) s ~restart:(kept r.rtrace)

(* The second faults a run of one [durable] fault allows. *)
let cut_seconds ?limit p (s : Plan.scenario) r =
  match s.faults with
  | [ one ] when state_of s = Drop ->
      Plan.cut_seconds ?limit ~rng:p.rng one ~first:p.changes
        ~after:(kept r.trace)
  | _ -> ([], [])

let replay_cut p ((h, k) as r) =
  let cfg = p.ccfg in
  let matched = if h = p.hash then None else Some h in
  let find l =
    List.find_opt (fun s -> String.equal (Plan.key ~single:cfg.single s) k) l
  in
  (* The scenarios each run of [l] leads to, one run at a time. *)
  let find_after l f = List.find_map (fun s -> find (f s (p.exec s))) l in
  let seconds s r =
    let c, rest = cut_seconds p s r in
    c @ rest
  in
  let candidates =
    [
      (fun () -> find (p.cuts @ p.faults));
      (fun () -> find_after p.cuts rcuts);
      (fun () -> find (Plan.cut_others ~changed:p.changed p.first_ops));
      (fun () -> find_after p.faults seconds);
      (fun () -> find_after p.faults rcuts);
      (fun () -> state_of_cut cfg p.cuts k);
    ]
  in
  (* A state drawn at random is drawn by a seed below a fourth of the
     budget. *)
  match List.find_map (fun f -> f ()) candidates with
  | Some s when not (drawn ~exec:p.exec s && (p.exec s).allowed) ->
      no_longer cfg ~hash:p.hash r
  | Some { cut = Some { state = Seed n; _ }; _ } when n >= p.budget / 4 ->
      let need = 4 * (n + 1) in
      failf cfg
        "the replay names %s, a state drawn at random under a budget of %d or \
         more: pass ~budget:%d."
        k need need
  | Some s -> ignore (p.attempt ?matched s : run)
  | None -> no_longer cfg ~hash:p.hash r

(* The states of cut [s] one by one from [next], while the draws met so far
   allow no more than [allowance] runs: [None] once they are all tried. *)
let enumerate p s tree spent allowance next =
  let rec go = function
    | None -> None
    | Some v when Plan.Draws.estimate tree > allowance || !spent >= allowance ->
        Some v
    | Some v ->
        incr spent;
        let r = p.attempt (with_state s (Enum v)) in
        Plan.Draws.add tree r.trail;
        go (Plan.next_choices r.trail)
  in
  go next

(* The states of cut [s] one by one, up to [allowance] runs: [None] once they
   are all tried, else the draws' estimate. *)
let enumerate_from_first p s spent allowance =
  let tree = Plan.Draws.empty () in
  incr spent;
  let r = p.attempt (with_state s (Enum [])) in
  Plan.Draws.add tree r.trail;
  match enumerate p s tree spent allowance (Plan.next_choices r.trail) with
  | None -> None
  | Some _ -> Some (Plan.Draws.estimate tree)

(* [share] states of cut [s] drawn at random. The first draw is a seed, which
   needs no widths: its draws give them to the others. *)
let draw_states p s spent share =
  incr spent;
  let r = p.attempt (with_state s (Plan.shape ~widths:[] 0)) in
  let widths = List.rev_map snd r.trail in
  for i = 1 to share - 1 do
    incr spent;
    ignore (p.attempt (with_state s (Plan.shape ~widths i)) : run)
  done

(* Every state each cut allows, enumerated while the draws met so far allow
   no more than the cut's share, and drawn at random past that. Each cut gets
   an even share of what is left: first every cut whose states fit in it,
   then, with what those left, the others, smallest first. Of a cut past its
   share only its size is kept, so memory stays that of one cut: one that
   fits in its new share is enumerated again from its first state. *)
let states p left =
  let drops = List.filter (fun s -> state_of s = Drop) p.cuts in
  let rec first left acc = function
    | [] -> (left, List.rev acc)
    | s :: rest as cuts ->
        let allowance = max 1 (left / List.length cuts) in
        let spent = ref 0 in
        let acc =
          match enumerate_from_first p s spent allowance with
          | None -> acc
          | Some estimate -> (s, estimate, !spent) :: acc
        in
        first (left - !spent) acc rest
  in
  let left, large = first left [] drops in
  let large =
    List.stable_sort (fun (_, a, _) (_, b, _) -> Int.compare a b) large
  in
  let rec share_out left = function
    | [] -> ()
    | (s, estimate, before) :: rest as cuts ->
        let share = max 0 (left / List.length cuts) in
        let spent = ref 0 in
        if share > 0 then
          if estimate <= before + share then
            ignore
              (enumerate_from_first p s spent (before + share) : int option)
          else draw_states p s spent share;
        share_out (left - !spent) rest
  in
  share_out left large

(* The sampled tier of cuts: half of [total] for the other operations, the
   pairs and the cuts during the restart, the rest for the states. *)
let sampled_cuts p ~total ~pairs ~recovery =
  let rng = p.rng in
  let thunks = List.map (fun s () -> ignore (p.attempt s : run)) in
  let classes =
    List.filter
      (fun (c, _) -> c <> [])
      [
        ( thunks (Plan.cut_others ~changed:p.changed p.first_ops),
          Plan.sample rng );
        (thunks pairs, List.take);
        (thunks recovery, Plan.sample rng);
      ]
  in
  let before = !(p.runs) in
  let share = total / 2 / max 1 (List.length classes) in
  List.iter
    (fun (c, pick) -> List.iter (fun run -> run ()) (pick share c))
    classes;
  states p (total - (!(p.runs) - before))

let cut_tiers p =
  let cfg = p.ccfg and rng = p.rng in
  let linear = List.length p.cuts + List.length p.faults in
  if linear > p.budget then
    failf cfg
      "the linear tier needs %d runs, more than the budget of %d, and a check \
       never passes on a partial one. Shrink the program, or raise ~budget."
      linear p.budget;
  (* Faults tried alone are linear runs the sampled tier does not grow with,
     and no second fault or cut in the restart follows them. *)
  let alone, faults = List.partition Plan.is_alone p.faults in
  (* The second faults and the cuts during the restart each run of the
     linear tier allows, drawn as it ends, so no run's operations outlive
     it: an even share of what the sampled tier can spend. *)
  let total =
    Plan.sampled_share ~budget:p.budget
      ~linear:(linear - List.length alone)
      ~classes:1
  in
  let quota =
    let k = List.length p.cuts + List.length faults in
    (total + k - 1) / max 1 k
  in
  let best = Plan.Pairs.v total in
  let recovery =
    List.concat_map
      (fun s ->
        let r = p.attempt ~keep:true s in
        Plan.Pairs.add best (cut_seconds ~limit:total p s r);
        Plan.sample rng quota (rcuts s r))
      (p.cuts @ faults)
  in
  List.iter (fun s -> ignore (p.attempt s : run)) alone;
  sampled_cuts p ~total ~pairs:(Plan.Pairs.to_list best) ~recovery

let cut_run cfg ?cap ?budget ?against s =
  exec cfg ~pass:Cuts ?cap ?budget ?against ~first:(Hashtbl.create 1) s

(* The runs of the pass with a cut, from its first run; and whether a
   restart made an operation in some run. *)
let cuts_plan cfg ~budget (first : run) first_ops =
  let full = cut_run cfg in
  let n = List.length first_ops in
  let diverged =
    diverge cfg
      (fst (full ~cap:0 ~against:(Array.of_list first_ops) Plan.base)).trace
  in
  (* A run keeps no more operations than four times the first run made:
     past that, the plan of the next scenarios reads its first ones. *)
  let cap = (4 * (n + first.rtrace.nkept)) + 10_000 in
  let hash = digest cfg first_ops in
  (* Whether the restart made an operation in some run. *)
  let reads = ref (first.rtrace.last > 0) and runs = ref 0 in
  (* A run's result is dropped once the scenarios it leads to are drawn from
     it, so memory stays that of one run. Only a run whose operations lead to
     other scenarios keeps them. *)
  let attempt ?matched ?(keep = false) s =
    incr runs;
    let r, out = full ~cap:(if keep then cap else 0) s in
    let s = renamed s r in
    if r.rtrace.last > 0 then reads := true;
    Option.iter
      (fun e ->
        let title, error = error_text cfg e in
        report cfg ~title ~hash
          ~key:(Plan.key ~single:cfg.single s)
          ~matched ~diverged ~trace:r.trace ~out ~error (cut_lines cfg s r))
      r.error;
    r
  in
  let changed (s : Plan.step) = Hashtbl.mem first.trace.changed s.index in
  ( {
      ccfg = cfg;
      rng = Random.State.make [| cfg.seed |];
      budget;
      hash;
      first_ops;
      changes = Plan.Changes.v first_ops;
      changed;
      cuts = Plan.cuts ~changed first_ops;
      faults = Plan.cut_faults first_ops;
      exec = (fun s -> fst (full ~cap s));
      attempt;
      runs;
    },
    reads )

let check_cuts cfg ~budget ~replay =
  (* A replay runs one scenario, whatever the budget. *)
  let first, out =
    cut_run cfg ?budget:(if replay = None then Some budget else None) Plan.base
  in
  (match first.error with
  | Some (Program o) ->
      refuse_first cfg ~trace:first.trace ~out o ~budget ~what:"cut"
  | _ -> ());
  let first_ops = kept first.trace in
  if first_ops = [] then
    failf cfg "the program makes no %s, so it tests nothing" cfg.noun;
  let p, reads = cuts_plan cfg ~budget first first_ops in
  match replay with
  | Some r -> replay_cut p r
  | None ->
      cut_tiers p;
      if not !reads then
        failf cfg
          "the restart makes no %s in any run, so it checks nothing a cut \
           leaves"
          cfg.noun

(* {1 The check} *)

let check_parts name parts =
  let rec dup = function
    | [] -> ()
    | n :: ns ->
        if List.mem n ns then
          Fmt.invalid_arg "%s: two parts are named %S" name n;
        dup ns
  in
  dup (List.map part_name parts)

(* [NAMES/HASH/KEY], [NAMES] the names of the parts joined by [+] and
   [HASH] eight hexadecimal digits, in either case; [NAMES/macos/HASH/KEY]
   under macOS and [NAMES/windows/HASH/KEY] under Windows. *)
let parse_replay ~name ~names ~os ~single ~passes r =
  let bad () =
    Fmt.invalid_arg "%s: replay is %S, not a replay string of this check" name r
  in
  let hex = function
    | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
    | _ -> false
  in
  let printed, prefix =
    match
      List.find_opt
        (fun o -> String.starts_with ~prefix:(names ^ os_key o ^ "/") r)
        [ `Macos; `Windows ]
    with
    | Some o -> (o, names ^ os_key o ^ "/")
    | None -> (`Linux, names ^ "/")
  in
  if not (String.starts_with ~prefix r) then bad ()
  else if printed <> os then
    match printed with
    | `Macos | `Windows ->
        Fmt.invalid_arg "%s: replay is %S, printed by a check under %s: pass%s"
          name r (os_name printed) (os_arg printed)
    | `Linux ->
        Fmt.invalid_arg
          "%s: replay is %S, printed by a check under Linux: drop%s" name r
          (os_arg os)
  else
    let n = String.length prefix in
    let r' = String.sub r n (String.length r - n) in
    match String.index_opt r' '/' with
    | Some 8 when String.length r' > 9 && String.for_all hex (String.sub r' 0 8)
      ->
        let key = String.sub r' 9 (String.length r' - 9) in
        let cut = Plan.well_formed key in
        if passes = [ Cuts ] && not cut then bad ()
        else if (not cut) && not (Plan.errors_key ~single key) then bad ()
        else (String.lowercase_ascii (String.sub r' 0 8), key)
    | _ -> bad ()

let run_named ~name ~args ?(passes = [ Errors; Cuts ]) ?os ?budget ?(seed = 0)
    ?replay ?(setup = ignore) ?(inspect = ignore) parts app =
  let os = match os with Some (#os as os) -> os | None -> `Linux in
  let budget =
    match budget with
    | None -> 20_000
    | Some b when b < 1 ->
        Fmt.invalid_arg "%s: budget is %d, must be positive" name b
    | Some b -> b
  in
  check_parts name parts;
  let names = String.concat "+" (List.map part_name parts) in
  let single, noun =
    match parts with
    | [ Part { m = (module P); _ } ] -> (true, P.noun)
    | _ -> (false, "operation")
  in
  let replay =
    Option.map (parse_replay ~name ~names ~os ~single ~passes) replay
  in
  let cfg =
    { name; args; os; seed; setup; inspect; app; parts; single; noun }
  in
  let saved = Random.get_state () in
  Fun.protect ~finally:(fun () -> Random.set_state saved) @@ fun () ->
  let errors = List.mem Errors passes and cuts = List.mem Cuts passes in
  match replay with
  | None ->
      let changes = errors && check_errors cfg ~budget ~replay:None in
      if cuts && ((not errors) || changes) then
        check_cuts cfg ~budget ~replay:None
  | Some ((_, k) as r) ->
      if cuts && Plan.well_formed k then check_cuts cfg ~budget ~replay:(Some r)
      else ignore (check_errors cfg ~budget ~replay:(Some r) : bool)

let run ?name ?os ?budget ?seed ?replay ?setup ?inspect parts app =
  let name, args =
    match name with
    | None -> ("Eio_chaos.run", "parts app")
    | Some n -> (n, "app")
  in
  run_named ~name ~args ?os ?budget ?seed ?replay ?setup ?inspect parts app

module Private = struct
  type nonrec pass = pass = Errors | Cuts

  let run ~name ?(args = "app") ?passes ?os ?budget ?seed ?replay ?setup
      ?inspect parts app =
    run_named ~name ~args ?passes ?os ?budget ?seed ?replay ?setup ?inspect
      parts app

  let name = part_name

  let restarted (env : env) =
    match env#chaos.st with
    | Some st -> (
        match st.phase with Restarting | Again -> true | _ -> false)
    | None -> false

  let within ?os parts f =
    let os = match os with Some (#os as os) -> os | None -> `Linux in
    let halted = ref None in
    match
      in_loop ~halted @@ fun clock mono _ ->
      let out = Output.v () in
      Eio.Fiber.with_binding Eio.Private.Debug.v#traceln (Stdenv.traceln out)
      @@ fun () ->
      Eio.Switch.run @@ fun sw ->
      let caps =
        List.map
          (fun (Part { m = (module P); state; _ }) ->
            P.start state ~os ~sw ~clock ~mono_clock:mono none)
          parts
      in
      let w = { envs = 0; out; clock; mono; chaos = none } in
      f (env w caps)
    with
    | Ok v -> Ok v
    | Error o -> Error (outcome_text ~noun:"operation" o)
    | exception (Eio.Io _ as x) -> Error ("raised " ^ exn_text x)
end
