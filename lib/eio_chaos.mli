(** Chaos tests of Eio programs on several simulated devices at once.

    A test assembles the environment of a program from simulated {e devices}, as
    a test written with [Eio_mock] assembles it from mocks. A device keeps its
    state across a restart, as a disk or a network does. {!run} runs the program
    many times on those devices, each time with a fault a device allows, and
    restarts it on what the fault left.

    {[
    let check ~disk ~net main = Eio_chaos.run [ disk; net ] main
    ]}

    The devices share two things only, both in this module:

    - one question, {!decide}: at each operation a device asks whether it fails,
      and with which of the faults the kernel allows there;
    - one notification, [restart] of {!DEVICE}: the program is restarted, after
      a power cut or not, and the device keeps what survives.

    The event loop is {!run}'s own, and private: devices receive a switch and
    clocks. *)

(** {1:running Running a program} *)

type device
(** The type for devices. Packages provide them, as [Eio_fs_chaos.disk ()] and
    [Eio_net_chaos.network ()]; {!Device} makes one of a module of type
    {!DEVICE}. *)

val quiet : device -> device
(** [quiet d] is [d] simulated but never failed: {!decide} answers its
    operations [None]. It is still restarted, and still keeps what a restart
    keeps. *)

type os = [ `Linux | `Macos | `Windows ]
(** The type for the kernels a device follows. *)

type t
(** The type for the runner of the current run, seen from a device, as a
    capability: what a device asks {!decide} of. A device given to {!run}
    receives it from [start] of {!DEVICE}, and a device built over others takes
    it from [env#chaos]. *)

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

    - [fs] and [cwd], [net]: those of the devices given to {!run}. A capability
      no device gives is refused: each of its operations raises [Eio.Io] with
      [Not_available];
    - [clock] and [mono_clock] start at [0.] and jump to the next timer when
      every fiber waits;
    - [secure_random] draws bytes from a fixed seed, and the default [Random]
      state is seeded before each run: the first run and each run with a fault
      draw the same, and a restart draws others, as a new process does;
    - [stdin] is empty, and [stdout], [stderr] and [Eio.traceln] are kept in
      memory: a report shows what the program printed, its last lines;
    - [domain_mgr] runs its work in the run's own event loop;
    - [process_mgr] starts nothing: [spawn] and [pipe] raise [Eio.Io] with
      [Not_available];
    - [debug] is Eio's, and [backend_id] is ["chaos"];
    - [chaos] is the run's {!t}, for a device the program builds over others. *)

val run :
  ?name:string ->
  ?os:[< os ] ->
  ?budget:int ->
  ?seed:int ->
  ?replay:string ->
  ?setup:(env -> unit) ->
  ?inspect:(env -> unit) ->
  device list ->
  (env -> unit) ->
  unit
(** [run ~name ~os ~budget ~seed ~replay ~setup ~inspect devices app] runs [app]
    once with no fault on [devices], which answer as [os] (defaults to [`Linux])
    does and hold what [setup] (defaults to doing nothing) left there. It then
    runs [app] again many times, each time from that state, with faults of the
    devices. After each of those runs it restarts every device and runs [app]
    again with no fault, as a machine restarts a service.

    {b How the faults are picked.} [run] learns the operations of [app] from its
    first run, those of every device in one sequence, and what each device
    allows at each. Runs come in two passes:

    - with no power cut: each [once] fault at each operation and at each of its
      {!places}, each [lasting] condition, and each [alone] fault, then pairs of
      faults, a [mild] one among the seconds;
    - with a power cut, if an operation of the first run may change what a
      device keeps: a cut after each such operation and at the end, each
      [durable] and [alone] fault followed by a cut at the end, then pairs of
      those, cuts during the restart, and the other states a cut may leave.

    Each pass has two tiers. The linear tier runs whole, at the operations
    {!Op.type-structure} picks, of the instances {!Op.instance} picks, and at
    the places those operations made: the runs of two devices add up, they do
    not multiply. The sampled tier spends a fortieth of [budget] (defaults to
    [20_000]), or half as much as the linear tier if that is more, on runs drawn
    from [seed] (defaults to [0]): the other operations, and second faults,
    first those on the path the first fault opened, at the operations that may
    change what a device keeps. The check fails if the linear tier needs more
    runs than [budget], rather than pass on part of it: the first run stops
    there.

    {b When it passes.} [run] passes iff:

    - the first run returns, makes an operation of a device that is not
      {!quiet}, and a second run with no fault makes the same kinds of operation
      in the same order. Where only their targets differ, the check goes on, and
      a report says where they differed;
    - no run hangs or raises an exception other than [Eio.Io], an assertion of
      [app] included. A run hangs when every fiber waits and no timer is due,
      when it still runs after an hour of its clock, when it wakes 10,000,000
      times without waiting 1 ms, when it makes 100,000 operations, those a
      condition split aside, when one failed operation is retried 10,000 times
      more than its first run made it while its clock stands still, when a
      device says it spins ({!hangs}), or when it keeps going for 1000
      operations after a power cut. A report of a hang where every fiber waits
      names those the program's own fiber waits on;
    - after each fault no device reports a promise {!lost}, and [inspect]
      (defaults to doing nothing), called as a fresh process, returns;
    - each restart returns, and a restart after a power cut makes an operation
      in some run.

    An [Eio.Io] that escapes a run with a fault neither passes nor fails it: the
    restart decides. So does any exception a device of [devices] says a fault of
    its own may make a program raise ({!DEVICE.indeterminate}), whichever device
    failed, as a fault of one device may reach a program through another, such
    as a server that a disk fault stopped and whose clients then read end of
    file.

    The default [Random] state is as it was before, once [run] returns.

    [name] (defaults to ["Eio_chaos.run"]) names the check in its reports, for a
    one-call shortcut of another package, as [Eio_fs_chaos.run]: a report then
    starts with [name], and replays as [name ~replay:"..." app].

    @raise Failure
      with a report of the first failing run: the fault, the operations of the
      run across the devices, what the program printed, the error, and the line
      to paste that runs it alone, as
      [Eio_chaos.run ~replay:"fs+net/85d85f49/fs:write:1.eio" devices app]. The
      line passes [~os:`Macos] for a run under macOS, or [~os:`Windows] under
      Windows, whose replay string names the kernel. A CRLF or a lone CR in the
      report, as in an error's message on Windows, is a line feed. Left
      uncaught, the report prints in the colours of console's theme where the
      program gave [Fmt.stderr] colour, as [Console_tty.setup ()] does on a
      terminal, and as its text elsewhere. Caught, [Printexc.to_string] of it is
      its text.
    @raise Invalid_argument
      if [budget] is not positive, [replay] is not a replay string of these
      devices under [os], or two of [devices] have the same name or give the
      same capability. *)

(** {2:reports Reports of other checks}

    A check of another package that is not a {!run}, as
    [Eio_fs_chaos.conformance], raises its report as a run does. *)

type difference
(** The type for what a check found different from what it expected. *)

val difference :
  ?operations:string -> expected:string -> got:string -> string -> difference
(** [difference ~operations ~expected ~got case] is the case [case], as
    ["case lookup"], which ran [operations] (defaults to none, as
    ["write a x; stat A"]) and got [got] where [expected] was expected. A value
    may hold line feeds. A cell of [operations] breaks after a ["; "] before it
    breaks inside an operation. *)

val fail :
  name:string -> ?differences:difference list -> ?footer:string -> string -> 'a
(** [fail ~name ~differences ~footer title] raises [Failure] with the report of
    the check [name]: [name ^ ": " ^ title] on its first line, then a table of
    [differences] (defaults to none), a row each, with the columns [scenario],
    [operations] if any has some, [expected] and [got], and then [footer]
    (defaults to none), a summary. The cases named ["family: member"], where
    [family] has no colon, are grouped under [family] when two or more are. The
    report is laid out in 80 columns: a long cell or footer goes on over several
    lines, and no column is wider than its cells need, counted in the cells a
    terminal draws them in.

    The text the [Failure] carries is that layout plain, its rules and guides in
    ASCII, and a CRLF or a lone CR in it a line feed. Left uncaught, it prints
    as a report of {!run} does: its title, the table's header and what each case
    got in the colours of console's theme where the program gave [Fmt.stderr]
    colour, as [Console_tty.setup ()] does on a terminal, and as its text
    elsewhere. Caught, [Printexc.to_string] of it is its text. *)

(** {1:devices Writing a device}

    A device is a module of type {!DEVICE}, given to {!Device}. At each
    operation it asks {!decide} whether the operation fails, with the faults the
    kernel allows there, and acts out the answer. A device that moves bytes
    gives with {!places} the places inside an operation where a fault may land,
    and acts out one that lands there after the bytes {!place} says. Its
    operations name an {!Op.instance}, as the connection of an end, so that the
    linear tier tries a few of many instances alike rather than each. A
    condition of the whole run, such as an outage, it proposes at its first
    operation and acts out from the start of the run, which [run ~faults] of
    {!DEVICE} names. *)

(** {2:question The question} *)

val none : t
(** [none] never fails anything: {!decide} answers [None]. It is what a device
    passes in production. *)

(** The operations a device asks about. *)
module Op : sig
  (** The type for which operations of one kind on one target the check tries
      its faults at first, in its linear tier. The others are drawn in its
      sampled tier. *)
  type structure =
    | Every  (** Each of them, as a device's opens or directory changes. *)
    | Thin
        (** The first three, the last, and the first of each length class, a
            length rounded down to a power of two, as a log's writes. *)
    | After of string
        (** Those that follow one the check tries first among the operations of
            that kind on the same target, or follow none, as the syncs that
            follow a file's writes. *)

  (** The type for whether an operation changes what a device keeps across a
      power cut. A power cut falls after each operation that does, and a second
      fault aims first at those of the path a first fault opened. *)
  type changes =
    | Never  (** Never, as a read. No power cut falls after it. *)
    | Maybe
        (** Only where the device says it did, with {!Eio_chaos.changed}. *)
    | Always  (** Each time, as a write. *)

  type t
  (** The type for operations. A check aims a fault at an operation by its
      device, its kind and its rank among the operations of that kind of that
      device, so the targets a program names may differ from run to run. *)

  val v :
    ?instance:string ->
    ?offset:int ->
    ?length:int ->
    ?detail:string ->
    ?structure:structure ->
    device:string ->
    kind:string ->
    changes:changes ->
    string ->
    t
  (** [v ~instance ~offset ~length ~detail ~structure ~device ~kind ~changes
       target] is the operation of kind [kind] of the device named [device] on
      [target], with:

      - [instance] (defaults to [""]): what [target] is a part of among others
        alike, as the connection ["1"] of its ends ["1c"] and ["1s"]. Of the
        instances of a device, the linear tier tries the first three, the last
        and the first of each shape, and leaves the others to the sampled tier.
        The shape of an instance is the kinds, details and length classes of its
        operations, on each of its targets named past the instance's own name;
      - [offset] (defaults to [0]): where a read or a write starts;
      - [length] (defaults to [0]): the bytes a read asks for or a write offers;
      - [detail] (defaults to [""]): what else tells two changes apart, as a
        rename's destination;
      - [structure] (defaults to {!Every}): whether the linear tier tries the
        operation, by its kind and its target;
      - [device]: the device's name, {!DEVICE.name}: ["fs"], ["net"];
      - [kind]: ["write"], ["sync"], ["connect"], ["read"];
      - [changes]: whether a power cut is tried after it;
      - [target]: what the operation is on, a path, or a connection and its
        side, as ["1c"]. *)

  val device : t -> string
  (** [device op] is the name of the device of [op]. *)

  val kind : t -> string
  (** [kind op] is the kind of [op]. *)

  val target : t -> string
  (** [target op] is what [op] is on. *)

  val instance : t -> string
  (** [instance op] is what the target of [op] is a part of, or [""]. *)

  val offset : t -> int
  (** [offset op] is where [op] starts, or [0]. *)

  val length : t -> int
  (** [length op] is the bytes [op] moves, or [0]. *)

  val detail : t -> string
  (** [detail op] is what else tells [op] apart, or [""]. *)

  val structure : t -> structure
  (** [structure op] is whether the linear tier tries [op]. *)

  val changes : t -> changes
  (** [changes op] is whether [op] changes what its device keeps. *)

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
    fault [op] meets in this run, if any, among those the kernel can produce at
    [op], in the device's own type. The device then acts the fault out: [decide]
    only chooses. [name f] is one word for [f], distinct among the faults listed
    at [op], which keys show; [pp] (defaults to [name]) says what [f] does, as
    ["fails with EIO"], which reports show. The faults come in five classes:

    - [once] fail [op] alone, and pair with a second one later in the run;
    - [lasting] (defaults to none) are conditions: one that starts at [op] holds
      for every later operation of the device that lists it, as a disk that is
      full. Its [pp] says what holds: ["the filesystem is full"];
    - [durable] (defaults to none) are tried with a power cut after the run, in
      place of [once]: the faults whose effect a cut shows, as a torn write;
    - [alone] (defaults to none) are tried alone, at the operations of the
      linear tier only, with a cut and without;
    - [mild] (defaults to none) are tried only as the second fault of a pair:
      too slight to test alone, as a read that returns one byte.

    A [once] fault may also land inside [op], at a place {!places} gave it:
    {!place} then says after how many bytes. [split] (defaults to [false]) says
    that [op] is one of those a condition of the run split another operation
    into, as one-byte reads: it does not count towards the operations a run may
    make, and a report of a run stopped at a limit says how many there were.

    It is [None] for {!none}, for a {!quiet} device, outside a run of the
    program and its restart, and during the restart. Once the power is cut, it
    raises [Eio.Cancel.Cancelled]. Once the run stops at one of its limits, it
    raises an exception of its own, which a device lets through: the run ends
    when the exception leaves the program, or when the program next waits. It
    does not suspend the calling fiber, so a device may call it holding a
    [Mutex.t], and may be called from any fiber of the run. *)

val index : t -> int
(** [index t] is the number {!decide} gave to the operation it was last asked
    about, from [1] in each run and in each restart. *)

val places : t -> int -> (int * int) list -> unit
(** [places t i l] records that the faults of the operation numbered [i] may
    also land inside it, at each place [(k, j)] of [l]: after its first [k]
    bytes, a place the operation numbered [j] made, as the end of the write that
    sent the bytes a read returned. A device calls it once [i] has moved its
    bytes. The linear tier tries a place whose [j] it tries, and the sampled
    tier the others. *)

val place : t -> int
(** [place t] is how many bytes of its operation pass before the fault {!decide}
    last answered: [0] if it falls at the operation's start. The device then
    moves those bytes, and the fault falls at the next operation of the same
    kind on the same target. *)

val faulted : t -> bool
(** [faulted t] is [true] iff an operation of the current run met a fault so
    far: [false] in a run with no fault, in [setup], [inspect] and the restart.
    A device built over others tells by it whether an error it gives may come
    from below. *)

val turn : t -> unit
(** [turn t] is work that does not wait, such as an operation that returns at
    once: it takes a turn of the run, 1 us of its clock, without letting other
    fibers run, so a loop of them still moves the clock and meets the run's
    limits. The time it takes does not count as time a retry waited. In a run
    stopped at one of its limits it does not return, so a device calls it
    holding no [Mutex.t]. It does nothing outside a run of the program and its
    restart. *)

val inside : t -> string -> (unit -> 'a) -> 'a
(** [inside t kind f] is [f ()], an operation of kind [kind] that may wait. A
    report of a hang names the fibers the program's own fiber waits on, by the
    operation each waits in and the last one {!decide} was asked about. *)

val hangs : t -> string -> 'a
(** [hangs t why] stops the run, which fails as a hang: its report says
    ["the run hangs: "] followed by [why]. A device calls it when it sees the
    program spin, as on an operation that keeps failing while the clock stands
    still. Outside a run of the program and its restart, it raises [Failure]
    with [why]. *)

val changed : t -> int -> unit
(** [changed t i] records that the operation numbered [i] changed what the
    device keeps across a power cut: a power cut is tried after it. *)

val spend : t -> Mtime.span -> unit
(** [spend t d] is an operation that takes [d] of the run's clock, while other
    fibers run. A device calls it for each operation that meets no fault, before
    the operation has its effect, or within it for a sync. The time it takes
    does not count as time a retry waited. It raises [Eio.Cancel.Cancelled] if
    the power was cut meanwhile.

    It does nothing outside a run of the program and its restart. *)

val lost : t -> string -> unit
(** [lost t what] records that the program was promised [what] and that the
    device no longer holds it, as ["an acknowledged file: /f held \"x\""]. A
    device calls it from [restart]. The run fails with a report titled
    ["a power cut lost "] or ["a fault lost "] followed by [what].

    @raise Invalid_argument if [t] is {!none}. *)

(** {2:device Devices} *)

(** The type for what a device gives the environment. *)
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

(** What a package provides to plug a simulated device in. A device gives one
    capability: [env#fs] and [env#cwd], or [env#net]. *)
module type DEVICE = sig
  type chaos

  type t
  (** The type for the device's state. It is made outside the program, so it
      outlives a run. *)

  val name : string
  (** [name] names the device in operations, reports and replay keys: ["fs"]. It
      holds no [':'], ['+'], ['|'] or ['/']. *)

  val noun : string
  (** [noun] is one of the device's operations, as a report on this device alone
      names them: ["filesystem operation"]. *)

  val start :
    t ->
    os:os ->
    sw:Eio.Switch.t ->
    clock:float Eio.Time.clock_ty Eio.Resource.t ->
    mono_clock:Eio.Time.Mono.ty Eio.Resource.t ->
    chaos ->
    capability
  (** [start t ~os ~sw ~clock ~mono_clock chaos] makes [t] new and empty, as
      [os] has it, for a run on those clocks: it forgets what an earlier run
      left, and asks [chaos] from now on. It is the capability that [setup]
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
      [("lost", "#2 write /f@0+4")]. A report of a run on two devices or more
      puts the device's name before each name. *)

  val restart : t -> power:bool -> cut:keep option -> capability
  (** [restart t ~power ~cut] is the capability of [t] for the program started
      again. [t] keeps what a restart keeps. With [power] false a process was
      restarted: a disk keeps everything, a network keeps the ports held in
      TIME_WAIT. With [power] true the power was cut: a disk keeps what its
      crash rules allow, a network keeps nothing. [cut] is what a power cut
      during the restart would keep, if it may meet one. A device reports a
      promise it broke with {!lost}. *)

  val pp_op : Op.t Fmt.t
  (** [pp_op] formats one of the device's operations for a trace line, as
      {!Op.pp} does by default. *)

  val indeterminate : exn -> bool
  (** [indeterminate x] is [true] iff [x] is what a program may raise when an
      operation of the device fails, other than [Eio.Io], as [End_of_file] when
      a connection ends. *)
end
with type chaos := t

module Device (P : DEVICE) : sig
  val v : P.t -> device
  (** [v p] is the device with state [p]. *)

  val find : t -> P.t option
  (** [find t] is the state of the device of [P] in the run of [t], while it
      runs: how a device's own functions reach it from [env#chaos]. *)
end

(**/**)

module Private : sig
  type pass = Errors | Cuts

  val run :
    name:string ->
    ?args:string ->
    ?passes:pass list ->
    ?os:[< os ] ->
    ?budget:int ->
    ?seed:int ->
    ?replay:string ->
    ?setup:(env -> unit) ->
    ?inspect:(env -> unit) ->
    device list ->
    (env -> unit) ->
    unit
  (** [run ~name ~args ~passes devices app] is {!val-run} [~name devices app]
      with the passes [passes] alone (defaults to both), replayed as
      [name ~replay:"..." args], [args] defaulting to ["app"]. *)

  val name : device -> string
  (** [name d] is the name of [d], as {!DEVICE.name} gives it. *)

  val restarted : env -> bool
  (** [restarted env] is [true] iff [env] is that of a restart. *)

  val within : ?os:[< os ] -> device list -> (env -> 'a) -> ('a, string) result
  (** [within devices f] is [f env] in an event loop of its own, with an
      {!type-env} on [devices] and no fault; [Error] says why [f] hung or the
      [Eio.Io] it raised. *)
end

(**/**)
