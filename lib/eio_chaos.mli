(** Chaos tests of Eio programs on several simulated devices at once.

    A test assembles the environment of a program from {e parts}, as a test
    written with [Eio_mock] assembles it from mocks. A part is a simulated
    device that keeps its state across a restart, such as a disk or a network.
    {!run} runs the program many times on those parts, each time with a failure
    a part allows, and restarts it on what the failure left.

    {[
    let check ~disk ~net main = Eio_chaos.run [ disk; net ] main
    ]}

    The parts share two things only, both in this module:

    - one question, {!decide}: at each operation a part asks whether it fails,
      and with which of the failures the kernel allows there;
    - one notification, [restart] of {!PART}: the program is restarted, after a
      power cut or not, and the part keeps what survives.

    The event loop is {!run}'s own, and private: parts receive a switch and
    clocks. *)

(** {1:question The question} *)

type t
(** The type for what a part asks: the runner of the current run, seen from a
    part, as a capability. A part receives it from [start] of {!PART}, and a
    device built inside the program takes it from [env#chaos]. *)

val none : t
(** [none] never fails anything: {!decide} answers [None]. It is what a device
    passes in production. *)

(** The operations a part asks about. *)
module Op : sig
  (** The type for which operations of one kind on one target the check tries
      its faults at first, in its linear tier. The others are drawn in its
      sampled tier. *)
  type structure =
    | Every  (** Each of them, as a part's opens or directory changes. *)
    | Thin
        (** The first three, the last, and the first of each length class, a
            length rounded down to a power of two, as a log's writes. *)
    | After of string
        (** Those that follow one the check tries first among the operations of
            that kind on the same target, or follow none, as the syncs that
            follow a file's writes. *)

  (** The type for whether an operation changes what a part keeps across a power
      cut. A power cut falls after each operation that does, and a second fault
      aims first at those of the path a first fault opened. *)
  type changes =
    | Never
    | Maybe  (** Only where the part says it did, with {!Eio_chaos.changed}. *)
    | Always

  type t = {
    device : string;  (** The part's name: ["fs"], ["net"]. *)
    kind : string;  (** ["write"], ["sync"], ["connect"], ["read"]. *)
    target : string;
        (** What the operation is on: a path, or a connection and its side, as
            ["1c"]. *)
    instance : string;
        (** What {!field-target} is a part of among others alike, as the
            connection ["1"] of its ends ["1c"] and ["1s"], else [""]. Of the
            instances of a part, the linear tier tries the first three, the last
            and the first of each shape, and leaves the others to the sampled
            tier. The shape of an instance is the kinds, details and length
            classes of its operations, on each of its targets named past the
            instance's own name. *)
    offset : int;  (** Where a read or a write starts, else [0]. *)
    length : int;  (** The bytes a read asks for or a write offers, else [0]. *)
    detail : string;
        (** What else tells two changes apart, as a rename's destination, else
            [""]. *)
    structure : structure;
    changes : changes;
  }
  (** The type for operations. A check aims a fault at an operation by its part,
      its kind and its rank among the operations of that kind of that part, so
      the targets a program names may differ from run to run. *)

  val pp : t Fmt.t
  (** [pp] formats an operation as a trace line shows it by default:
      [write /f@0+5]. *)
end

val decide :
  t ->
  Op.t ->
  name:('fault -> string) ->
  ?pp:'fault Fmt.t ->
  ?lasting:'fault list ->
  ?durable:'fault list ->
  ?alone:'fault list ->
  ?mild:'fault list ->
  ?split:bool ->
  'fault list ->
  'fault option
(** [decide t op ~name ~pp ~lasting ~durable ~alone ~mild ~split once] is the
    failure [op] meets in this run, if any, among those the kernel can produce
    at [op], in the part's own type. The part then acts the failure out:
    [decide] only chooses. [name f] is one word for [f], distinct among the
    failures listed at [op], which keys show; [pp] (defaults to [name]) says
    what [f] does, as ["fails with EIO"], which reports show. The failures come
    in four classes:

    - [once] fail [op] alone, and pair with a second one later in the run;
    - [lasting] (defaults to none) are conditions: one that starts at [op] holds
      for every later operation of the part that lists it, as a disk that is
      full. Its [pp] says what holds: ["the filesystem is full"];
    - [durable] (defaults to none) are tried with a power cut after the run, in
      place of [once]: the faults whose effect a cut shows, as a torn write;
    - [alone] (defaults to none) are tried alone, at the operations of the
      linear tier only, with a cut and without;
    - [mild] (defaults to none) are tried only as the second failure of a pair:
      too slight to test alone, as a read that returns one byte.

    A [once] failure may also land inside [op], at a place {!places} gave it:
    {!place} then says after how many bytes. [split] (defaults to [false]) says
    that [op] is one of those a condition of the run split another operation
    into, as one-byte reads: it does not count towards the operations a run may
    make, and a report of a run stopped at a limit says how many there were.

    It is [None] for {!none}, for a {!quiet} part, outside a run of the program
    and its restart, and during the restart. Once the power is cut, it raises
    [Eio.Cancel.Cancelled]. It does not suspend the calling fiber, and may be
    called from any fiber of the run. *)

val index : t -> int
(** [index t] is the number {!decide} gave to the operation it was last asked
    about, from [1] in each run and in each restart. *)

val places : t -> int -> (int * int) list -> unit
(** [places t i l] records that the failures of the operation numbered [i] may
    also land inside it, at each place [(k, j)] of [l]: after its first [k]
    bytes, a place the operation numbered [j] made, as the end of the write that
    sent the bytes a read returned. A part calls it once [i] has moved its
    bytes. The linear tier tries a place whose [j] it tries, and the sampled
    tier the others. *)

val place : t -> int
(** [place t] is how many bytes of its operation pass before the failure
    {!decide} last answered: [0] if it falls at the operation's start. The part
    then moves those bytes, and the failure falls at the next operation of the
    same kind on the same target. *)

val faulted : t -> bool
(** [faulted t] is [true] iff an operation of the current run met a failure so
    far: [false] in a run with no failure, in [setup], [inspect] and the
    restart. A device built over parts tells by it whether an error it gives may
    come from below. *)

val turn : t -> unit
(** [turn t] is work that does not wait, such as an operation that returns at
    once: it takes a turn of the run, 1 us of its clock, without letting other
    fibers run, so a loop of them still moves the clock and meets the run's
    limits. The time it takes does not count as time a retry waited. It does
    nothing outside a run of the program and its restart. *)

val inside : t -> string -> (unit -> 'a) -> 'a
(** [inside t kind f] is [f ()], an operation of kind [kind] that may wait. A
    report of a hang names the fibers the program's own fiber waits on, by the
    operation each waits in and the last one {!decide} was asked about. *)

val hangs : t -> string -> 'a
(** [hangs t why] stops the run, which fails as a hang: its report says
    ["the run hangs: "] followed by [why]. A part calls it when it sees the
    program spin, as on an operation that keeps failing while the clock stands
    still. Outside a run of the program and its restart, it raises [Failure]
    with [why]. *)

val changed : t -> int -> unit
(** [changed t i] records that the operation numbered [i] changed what the part
    keeps across a power cut: a power cut is tried after it. *)

val spend : t -> Mtime.span -> unit
(** [spend t d] is an operation that takes [d] of the run's clock, while other
    fibers run. A part calls it for each operation that meets no fault, before
    the operation has its effect, or within it for a sync. The time it takes
    does not count as time a retry waited. It raises [Eio.Cancel.Cancelled] if
    the power was cut meanwhile.

    It does nothing outside a run of the program and its restart. *)

val lost : t -> string -> unit
(** [lost t what] records that the program was promised [what] and that the part
    no longer holds it, as ["an acknowledged file: /f held \"x\""]. A part calls
    it from [restart]. The run fails with a report titled ["a power cut lost "]
    or ["a fault lost "] followed by [what].

    @raise Invalid_argument if [t] is {!none}. *)

(** {1:parts Parts} *)

type os = [ `Linux | `Macos ]
(** The type for the kernels a part follows. *)

(** The type for what a part gives the environment. *)
type capability =
  | Fs of Eio.Fs.dir_ty Eio.Path.t * Eio.Fs.dir_ty Eio.Path.t
      (** [env#fs], and [env#cwd] confined to itself. *)
  | Net of [ `Generic ] Eio.Net.ty Eio.Resource.t  (** [env#net]. *)

(** The type for what a power cut keeps of the changes no sync made durable. *)
type keep =
  | Lose_unsynced  (** None of them. *)
  | Keep_unsynced  (** All of them, as after a process kill. *)
  | Choose of (int -> int)
      (** Those the draws choose: [f n] is a choice in [\[0, n)]. The check
          tries each sequence of choices while they are few, and draws them at
          random past that. *)

(** What a package provides to plug a simulated device in. *)
module type PART = sig
  type chaos

  type t
  (** The type for the part's state. It is made outside the program, so it
      outlives a run. *)

  val name : string
  (** [name] names the part in operations, reports and replay keys: ["fs"]. It
      holds no [':'], ['+'], ['|'] or ['/']. *)

  val noun : string
  (** [noun] is one of the part's operations, as a report names them:
      ["filesystem operation"]. *)

  val start :
    t ->
    os:os ->
    sw:Eio.Switch.t ->
    clock:float Eio.Time.clock_ty Eio.Resource.t ->
    mono_clock:Eio.Time.Mono.ty Eio.Resource.t ->
    chaos ->
    capability
  (** [start t ~os ~sw ~clock ~mono_clock chaos] makes [t] new and empty, as
      [os] has it, for a run on those clocks, and is the capability that [setup]
      gets. [sw] ends with the run and its restart. *)

  val run : t -> cut:keep option -> faults:string list -> capability
  (** [run t ~cut ~faults] is the capability of [t] for the program, on what
      [setup] left, which is durable. Each operation through it asks {!decide}
      on the [chaos] of [start]. [cut] is what a power cut would keep, if the
      run may meet one; [faults] the names of the faults the run will meet on
      [t]. *)

  val cut : t -> (string * string) list
  (** [cut t] cuts the power now: [t] keeps what [cut] of {!run} or {!restart}
      says. It is lines for a report: a name and a text, as
      [("lost", "#2 write /f@0+4")]. *)

  val restart : t -> power:bool -> cut:keep option -> capability
  (** [restart t ~power ~cut] is the capability of [t] for the program started
      again. [t] keeps what a restart keeps. With [power] false a process was
      restarted: a disk keeps everything, a network keeps the ports held in
      TIME_WAIT. With [power] true the power was cut: a disk keeps what its
      crash rules allow, a network keeps nothing. [cut] is what a power cut
      during the restart would keep, if it may meet one. A part reports a
      promise it broke with {!lost}. *)

  val pp_op : Op.t Fmt.t
  (** [pp_op] formats one of the part's operations for a trace line. *)

  val indeterminate : exn -> bool
  (** [indeterminate x] is [true] iff [x] is what a program may raise when an
      operation of the part fails, other than [Eio.Io], as [End_of_file] when a
      connection ends. *)
end
with type chaos := t

type part
(** The type for parts. *)

module Part (P : PART) : sig
  val v : P.t -> part
  (** [v p] is the part with state [p]. *)

  val find : t -> P.t option
  (** [find t] is the state of the part of [P] in the run of [t], while it runs:
      how a part's own functions reach it from [env#chaos]. *)
end

val quiet : part -> part
(** [quiet p] is [p] simulated but never failed: {!decide} answers its
    operations [None]. It is still restarted, and still keeps what a restart
    keeps. *)

(** {1:running Running a program} *)

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
(** The type for what a program runs with: the methods of [Eio.Stdenv], each
    simulated, so a program written against the [Eio.Stdenv] accessors takes it
    unchanged.

    - [fs] and [cwd], [net]: those of the parts given to {!run}. A capability no
      part gives is refused: each of its operations raises [Eio.Io] with
      [Not_available];
    - [clock] and [mono_clock] start at [0.] and jump to the next timer when
      every fiber waits;
    - [secure_random] draws bytes from a fixed seed, and the default [Random]
      state is seeded before each run: the first run and each run with a fault
      draw the same, and a restart draws others, as a new process does;
    - [stdin] is empty, and [stdout], [stderr] and [Eio.traceln] are kept in
      memory: a failure report shows what the program printed, its last lines;
    - [domain_mgr] runs its work in the run's own event loop;
    - [process_mgr] starts nothing: [spawn] and [pipe] raise [Eio.Io] with
      [Not_available];
    - [debug] is Eio's, and [backend_id] is ["chaos"];
    - [chaos] is the run's {!t}, for a device the program builds itself. *)

val run :
  ?os:[< os ] ->
  ?budget:int ->
  ?seed:int ->
  ?replay:string ->
  ?setup:(env -> unit) ->
  ?inspect:(env -> unit) ->
  part list ->
  (env -> unit) ->
  unit
(** [run ~os ~budget ~seed ~replay ~setup ~inspect parts app] runs [app] once
    with no failure on [parts], which answer as [os] (defaults to [`Linux]) does
    and hold what [setup] (defaults to doing nothing) left there. It then runs
    [app] again many times, each time from that state, with failures of the
    parts. After each of those runs it restarts every part and runs [app] again
    with no failure, as a machine restarts a service.

    {b How the failures are picked.} [run] learns the operations of [app] from
    its first run, those of every part in one sequence, and what each part
    allows at each. Runs come in two passes:

    - with no power cut: each [once] failure at each operation and at each of
      its {!places}, each [lasting] condition, and each [alone] failure, then
      pairs of failures, a [mild] one among the seconds;
    - with a power cut, if an operation of the first run may change what a part
      keeps: a cut after each such operation and at the end, each [durable] and
      [alone] failure followed by a cut at the end, then pairs of those, cuts
      during the restart, and the other states a cut may leave.

    Each pass has two tiers. The linear tier runs whole, at the operations
    {!Op.type-structure} picks, of the instances {!Op.t.instance} picks, and at
    the places those operations made: the runs of two parts add up, they do not
    multiply. The sampled tier spends a fortieth of [budget] (defaults to
    [20_000]), or half as much as the linear tier if that is more, on runs drawn
    from [seed] (defaults to [0]): the other operations, and second failures,
    first those on the path the first failure opened, at the operations that may
    change what a part keeps. A check never passes on a partial linear tier: the
    first run stops once the linear tier needs more runs than [budget].

    {b When it passes.} [run] passes iff:

    - the first run returns, and a second run with no failure makes the same
      kinds of operation in the same order. Where only their targets differ, the
      check goes on, and a failure report says where they differed;
    - no run hangs or raises an exception other than [Eio.Io], an assertion of
      [app] included. A run hangs when every fiber waits and no timer is due,
      when it still runs after an hour of its clock, when it wakes 10 million
      times without waiting 1 ms, when it makes 100,000 operations, those a
      condition split aside, when one failed operation is retried 10,000 times
      more than its first run made it while its clock stands still, when a part
      says it spins ({!hangs}), or when it keeps going for 1000 operations after
      a power cut. A report of a hang where every fiber waits names those the
      program's own fiber waits on;
    - after each failure no part reports a promise {!lost}, and [inspect]
      (defaults to doing nothing), called as a fresh process, returns;
    - each restart returns.

    An [Eio.Io] that escapes a run with a failure neither passes nor fails it:
    the restart decides. So does any exception a part of [parts] says a failure
    of its own may make a program raise ({!PART.indeterminate}), whichever part
    failed, as a failure of one part may reach a program through another, such
    as a server that a disk failure stopped and whose clients then read end of
    file.

    The default [Random] state is as it was before, once [run] returns.

    @raise Failure
      with a report of the first failing run: the failure, the operations of the
      run across the parts, what the program printed, the error, and the line to
      paste that runs it alone, as
      [Eio_chaos.run ~replay:"fs+net/85d85f49/fs:write:1.eio" parts app].
    @raise Invalid_argument
      if [budget] is not positive, [replay] is not a replay string of these
      parts, or two of [parts] have the same name or give the same capability.
*)

(**/**)

module Private : sig
  type pass = Errors | Cuts

  val run :
    name:string ->
    ?passes:pass list ->
    ?os:[< os ] ->
    ?budget:int ->
    ?seed:int ->
    ?replay:string ->
    ?setup:(env -> unit) ->
    ?inspect:(env -> unit) ->
    part list ->
    (env -> unit) ->
    unit
  (** [run ~name ~passes parts app] is {!val-run} [parts app] with the passes
      [passes] alone (defaults to both), for a one-call shortcut of another
      package: its report names [name], and replays as [name ~replay:"..." app].
  *)

  val restarted : env -> bool
  (** [restarted env] is [true] iff [env] is that of a restart. *)

  val within : ?os:[< os ] -> part list -> (env -> 'a) -> ('a, string) result
  (** [within parts f] is [f env] in an event loop of its own, with an
      {!type-env} on [parts] and no fault; [Error] says why [f] hung or the
      [Eio.Io] it raised. *)
end

(**/**)
