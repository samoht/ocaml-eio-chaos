open! Hermetic
module Op = Op

(* {1 The question} *)

(* A run that makes more operations spins, or is more than a check plans
   from: it stops there. *)
let max_ops = 100_000

(* The operations a program may make once the power is cut, before it is
   taken to catch [Eio.Cancel.Cancelled]. *)
let max_after_cut = 1000

type phase = Setup | Running | After

(* What the runs of one scenario share. *)
type state = {
  mutable phase : phase;
  mutable pending : Plan.fault list;
  mutable steps : Plan.step list; (* latest first *)
  mutable n : int;
  mutable ops : int; (* in this phase *)
  mutable cut : bool;
  mutable after_cut : int;
  mutable scope : Eio.Cancel.t option; (* the program's, a cut cancels it *)
  mutable lost : string list; (* latest first *)
  mutable halted : string option; (* why the loop must stop *)
}

type t = { state : state option; quiet : bool }

let none = { state = None; quiet = true }

(* Park the fiber where no cancel reaches, so even a loop that catches
   everything stops; the loop's [stop] then ends the run. *)
let halt st why =
  st.halted <- Some why;
  Eio.Cancel.protect Eio.Fiber.await_cancel

let too_many =
  Fmt.str
    "the run made %d operations, more than a check plans from: a busy loop, or \
     a program too large for a check"
    max_ops

let kept_going =
  Fmt.str
    "the run hangs: it kept going for %d operations after the power cut, so it \
     catches Eio.Cancel.Cancelled"
    max_after_cut

let met st (op : Op.t) names =
  let pos = Plan.pos op in
  match
    List.find_opt
      (fun (f : Plan.fault) -> f.pos = pos && List.mem f.name names)
      st.pending
  with
  | None -> None
  | Some f ->
      st.pending <- List.filter (( != ) f) st.pending;
      Some f.name

let decide t (op : Op.t) ~name allowed =
  match t.state with
  | None -> None
  | Some st -> (
      st.ops <- st.ops + 1;
      if st.ops > max_ops then halt st too_many;
      if st.cut then begin
        st.after_cut <- st.after_cut + 1;
        if st.after_cut > max_after_cut then halt st kept_going;
        None
      end
      else
        match st.phase with
        | Setup | After -> None
        | Running ->
            let names = if t.quiet then [] else List.map name allowed in
            let met = met st op names in
            st.n <- st.n + 1;
            st.steps <-
              { Plan.index = st.n; op; allowed = names; met } :: st.steps;
            Option.bind met (fun m ->
                List.find_opt (fun f -> String.equal (name f) m) allowed))

exception Power_cut

let () =
  Printexc.register_printer (function
    | Power_cut -> Some "the power was cut"
    | _ -> None)

let power_cut t =
  match t.state with
  | Some ({ phase = Running; scope = Some cc; _ } as st) ->
      st.cut <- true;
      Eio.Cancel.cancel cc Power_cut;
      Eio.Fiber.check ();
      raise (Eio.Cancel.Cancelled Power_cut)
  | _ -> invalid_arg "Eio_chaos.power_cut: the program is not running a fault"

let lost t what =
  match t.state with
  | Some st -> st.lost <- what :: st.lost
  | None -> invalid_arg "Eio_chaos.lost: none is no run"

(* {1 Parts} *)

type os = [ `Linux | `Macos ]

type capability =
  | Fs of Eio.Fs.dir_ty Eio.Path.t * Eio.Fs.dir_ty Eio.Path.t
  | Net of [ `Generic ] Eio.Net.ty Eio.Resource.t

module type PART = sig
  type chaos
  type t

  val name : string

  val start :
    t ->
    os:os ->
    sw:Eio.Switch.t ->
    clock:float Eio.Time.clock_ty Eio.Resource.t ->
    mono_clock:Eio.Time.Mono.ty Eio.Resource.t ->
    chaos ->
    capability

  val restart : t -> power:bool -> capability
end
with type chaos := t

type part = Part : (module PART with type t = 'a) * 'a * bool -> part

module Part (P : PART) = struct
  let v s = Part ((module P), s, false)
end

let quiet (Part (m, s, _)) = Part (m, s, true)
let part_name (Part ((module P), _, _)) = P.name

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
  seed : int;
  mutable envs : int;
  out : Output.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  mono : Eio.Time.Mono.ty Eio.Resource.t;
  chaos : t;
}

(* The next env of [w], on the capabilities [caps]. Its [secure_random] and
   the default [Random] state, which it resets, are seeded by its place among
   the envs of the scenario, so the [n]th env of each scenario draws the same
   bytes. *)
let env w caps : env =
  let n = w.envs in
  w.envs <- n + 1;
  Random.full_init [| w.seed; n |];
  let entropy = Stdenv.entropy (Random.State.make [| w.seed; n; 1 |]) in
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

let check_capabilities caps =
  let count p = List.length (List.filter p caps) in
  if count (function Fs _ -> true | Net _ -> false) > 1 then
    invalid_arg "Eio_chaos.run: two parts give env#fs";
  if count (function Net _ -> true | Fs _ -> false) > 1 then
    invalid_arg "Eio_chaos.run: two parts give env#net"

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
}

(* An error a part gave, which a run with a fault may let escape. *)
let rec indeterminate = function
  | Eio.Io _ | End_of_file -> true
  | Eio.Exn.Multiple l ->
      l <> [] && List.for_all (fun (x, _) -> indeterminate x) l
  | _ -> false

(* A promise a part lost, which fails the run with its text alone. *)
exception Broken of string

let in_loop st main =
  match
    Quiet_mock.run
      ~effects:{ handle = Systhread.handle }
      ~stop:(fun () -> Option.is_some st.halted)
      ~limit:3600.
      (fun clock mono _charge -> main clock mono)
  with
  | () -> None
  | exception Quiet_mock.Stopped -> st.halted
  | exception Quiet_mock.Deadlock_detected _ ->
      Some "the run hangs: every fiber waits and no timer is due"
  | exception Quiet_mock.Time_limit ->
      Some "the run hangs: it is still running after an hour of its clock"
  | exception Quiet_mock.Busy ->
      Some
        "the run woke 10,000,000 times without waiting 1 ms or more: a busy \
         loop"
  | exception Quiet_mock.Too_long ->
      Some
        "the run took 10,000,000 turns, more than a check can repeat: a busy \
         loop, or a program too large for a check"
  | exception Broken why -> Some why
  | exception x -> Some (Report.exn_text x)

(* The program in a scope a power cut cancels whole. *)
let run_app st app env : Report.ended =
  match
    Eio.Cancel.sub (fun cc ->
        st.scope <- Some cc;
        app env)
  with
  | () -> if st.cut then Cut else Returned
  | exception x -> if st.cut then Cut else Raised x

let state s =
  {
    phase = Setup;
    pending = s;
    steps = [];
    n = 0;
    ops = 0;
    cut = false;
    after_cut = 0;
    scope = None;
    lost = [];
    halted = None;
  }

let enter st phase =
  st.phase <- phase;
  st.ops <- 0

(* After a run with a fault: each part restarted, after a power cut or not,
   then [inspect] and the program again, with no fault. *)
let restart cfg st w ~stage ended =
  let power = st.cut in
  st.cut <- false;
  stage := Report.Restart ended;
  Output.mark w.out "-- restart --";
  let caps =
    List.map (fun (Part ((module P), p, _)) -> P.restart p ~power) cfg.parts
  in
  if st.lost <> [] then begin
    stage := Lost ended;
    raise (Broken (String.concat "; " (List.rev st.lost)))
  end;
  stage := Inspect ended;
  cfg.inspect (env w caps);
  stage := Restart ended;
  cfg.app (env w caps)

(* A run of [s] on new parts that [setup] prepared, then, if [s] has a
   fault, its restart: its operations, what it printed, and why it failed if
   it did. *)
let exec cfg (s : Plan.scenario) =
  let st = state s and out = Output.v () and stage = ref Report.Setup in
  let failed =
    in_loop st @@ fun clock mono ->
    Eio.Fiber.with_binding Eio.Private.Debug.v#traceln (Stdenv.traceln out)
    @@ fun () ->
    Eio.Switch.run @@ fun sw ->
    let chaos quiet = { state = Some st; quiet } in
    let w =
      { seed = cfg.seed; envs = 0; out; clock; mono; chaos = chaos false }
    in
    let caps =
      List.map
        (fun (Part ((module P), p, quiet)) ->
          P.start p ~os:cfg.os ~sw ~clock ~mono_clock:mono (chaos quiet))
        cfg.parts
    in
    check_capabilities caps;
    cfg.setup (env w caps);
    stage := Run;
    enter st Running;
    let ended = run_app st cfg.app (env w caps) in
    enter st After;
    match ended with
    | Returned when s = [] -> ()
    | Raised x when s = [] || not (indeterminate x) -> raise x
    | ended -> restart cfg st w ~stage ended
  in
  let failed =
    Option.map (fun error -> { Report.stage = !stage; error }) failed
  in
  (List.rev st.steps, Output.printed out, failed)

(* {1 The check} *)

let fail cfg fmt = Fmt.failwith ("%s: " ^^ fmt) cfg.name

(* Where a run with no fault differs from the first, if it does. *)
let diverged cfg first =
  let again, _, _ = exec cfg Plan.none in
  Option.iter
    (fail cfg
       "two runs with no fault made different operations: %s. A check aims \
        each fault at an operation by its place in the run, so it cannot aim \
        them here. The program reached something the check does not simulate: \
        Random.self_init, a process id, the wall clock read through Unix, a \
        second domain, or state an earlier run left in memory. Built with \
        hermetic (open! Hermetic), the first four do not compile.")
    (Plan.diverge first again)

let replay_line cfg hash key =
  Fmt.str "%s ~replay:%S %s" cfg.name (hash ^ "/" ^ key) cfg.args

(* A run of [s], failing the check with its report if it failed. *)
let attempt cfg ~hash ?matched s =
  let trace, printed, failed = exec cfg s in
  Option.iter
    (Report.scenario ~name:cfg.name ~replay:(replay_line cfg hash) ?matched
       ~printed s trace)
    failed;
  trace

(* The scenario a replay's key [k] names: one failure, or a second one that
   the run of a first allows. *)
let replayed cfg ~first k =
  let singles = Plan.linear first @ Plan.others first in
  let find l = List.find_opt (fun s -> Plan.key s = k) l in
  match find singles with
  | Some _ as s -> s
  | None ->
      List.find_map
        (fun (s : Plan.scenario) ->
          match s with
          | [ f ] when String.starts_with ~prefix:(Plan.key s ^ "+") k ->
              let after, _, _ = exec cfg s in
              let changed, same = Plan.seconds f ~first ~after in
              find (changed @ same)
          | _ -> None)
        singles

let replay_one cfg ~first ~hash (h, k) =
  (* After a fix the first run may differ: the scenario is found by the
     places its key names. *)
  let matched =
    if h = hash then None
    else
      Fmt.kstr Option.some
        "the replay named first run %s, and the program's is now %s" h hash
  in
  match replayed cfg ~first k with
  | Some s -> ignore (attempt cfg ~hash ?matched s : Plan.trace)
  | None when h = hash ->
      fail cfg "the replay names %s, a scenario this check no longer has" k
  | None ->
      fail cfg
        "the replay names %s, a scenario this check no longer has: its first \
         run was %s and is now %s. Run the check without ~replay."
        k h hash

(* The linear tier, then the sampled one, within [budget] runs. *)
let explore cfg ~budget ~first ~hash =
  let linear = Plan.linear first and others = Plan.others first in
  let n = List.length linear in
  if n > budget then
    fail cfg
      "the linear tier needs %d runs, more than the budget of %d, and a check \
       never passes on a partial one. Shrink the program, or raise ~budget."
      n budget;
  let rng = Random.State.make [| cfg.seed |] in
  let share classes = Plan.sampled_share ~budget ~linear:n ~classes in
  (* The second faults each run allows, drawn as it ends, so no run's
     operations outlive it: first those on the path the first fault opened. *)
  let quota =
    let sh = share (if others = [] then 1 else 2) in
    (sh + n - 1) / max 1 n
  in
  let pairs =
    List.concat_map
      (fun s ->
        let after = attempt cfg ~hash s in
        match s with
        | [ f ] ->
            let changed, same = Plan.seconds f ~first ~after in
            let changed = List.filteri (fun i _ -> i < quota) changed in
            changed @ Plan.sample rng (quota - List.length changed) same
        | _ -> [])
      linear
  in
  let classes = List.filter (( <> ) []) [ others; pairs ] in
  let share = share (List.length classes) in
  List.iter
    (fun c ->
      List.iter
        (fun s -> ignore (attempt cfg ~hash s : Plan.trace))
        (Plan.sample rng share c))
    classes

let check cfg ~budget ~replay =
  let first, printed, failed = exec cfg Plan.none in
  Option.iter
    (fun (f : Report.failure) ->
      Report.first ~name:cfg.name ~printed first f.error)
    failed;
  let candidates = List.exists (fun (s : Plan.step) -> s.allowed <> []) first in
  if candidates || Option.is_some replay then begin
    diverged cfg first;
    let hash = Plan.digest first in
    match replay with
    | Some r -> replay_one cfg ~first ~hash r
    | None -> explore cfg ~budget ~first ~hash
  end

let check_parts name parts =
  let rec dup = function
    | [] -> ()
    | n :: ns ->
        if List.mem n ns then
          Fmt.invalid_arg "%s: two parts are named %S" name n;
        dup ns
  in
  dup (List.map part_name parts)

let run_named ~name ~args ?os ?(budget = 20_000) ?(seed = 0) ?replay
    ?(setup = ignore) ?(inspect = ignore) parts app =
  let os =
    match os with Some `Macos -> `Macos | Some `Linux | None -> `Linux
  in
  if budget < 1 then
    Fmt.invalid_arg "%s: budget is %d, it must be positive" name budget;
  let replay =
    Option.map
      (fun r ->
        match Plan.parse_replay r with
        | Some r -> r
        | None ->
            Fmt.invalid_arg
              "%s: replay is %S, not a replay string: one is HASH/KEY, HASH 8 \
               hexadecimal digits and KEY as a failure names it"
              name r)
      replay
  in
  check_parts name parts;
  let cfg = { name; args; os; seed; setup; inspect; app; parts } in
  let saved = Random.get_state () in
  Fun.protect ~finally:(fun () -> Random.set_state saved) @@ fun () ->
  check cfg ~budget ~replay

let run ?os ?budget ?seed ?replay ?setup ?inspect parts app =
  run_named ~name:"Eio_chaos.run" ~args:"parts app" ?os ?budget ?seed ?replay
    ?setup ?inspect parts app

module Private = struct
  let run ~name ?os ?budget ?seed ?replay ?setup ?inspect parts app =
    run_named ~name ~args:"app" ?os ?budget ?seed ?replay ?setup ?inspect parts
      app
end
