(** Chaos tests of Eio programs on several simulated devices at once.

    A test assembles the environment of a program from {e parts}, as a test
    written with [Eio_mock] assembles it from mocks. A part is a simulated
    device that keeps its state across a restart, such as a disk or a network.
    {!run} runs the program many times on those parts, each time with one
    failure a part allows, and restarts it on what the failure left.

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
  type t = {
    device : string;  (** The part's name: ["fs"], ["net"]. *)
    kind : string;  (** ["write"], ["sync"], ["connect"], ["read"]. *)
    target : string;
        (** What the operation is on, stable from one run to the next: a path,
            or a connection and its side, as ["1c"]. A part that knows a name is
            temporary gives it a stable form. *)
    at : int;
        (** The operation's place among those of its kind on its target: a rank
            from [1], or a byte offset. With {!field-kind} and {!field-target}
            it finds the operation again in another run. *)
    length : int;  (** The bytes a read asks for or a write offers, else [0]. *)
  }
  (** The type for operations. *)

  val pp : t Fmt.t
  (** [pp] formats an operation as a trace line shows it: [fs write /f @1+5]. *)
end

val decide :
  t -> Op.t -> name:('fault -> string) -> 'fault list -> 'fault option
(** [decide t op ~name allowed] is the failure [op] meets in this run, if any,
    among [allowed]: the failures the kernel can produce at [op], in the part's
    own type. [name f] is one word for [f], distinct among [allowed], which
    reports and replay keys show. The part then acts the failure out: [decide]
    only chooses.

    [allowed] may hold more than errors. A condition that lasts from [op] on (a
    disk now full, an outage), a power cut, and each state a cut may leave are
    choices of the same kind. A part asks about them at an operation of its own,
    such as [{ kind = "cut"; _ }].

    It is [None] for {!none}, for a {!quiet} part, during [setup], [inspect] and
    the restart, and after a power cut. It does not suspend the calling fiber,
    and may be called from any fiber of the run. *)

val power_cut : t -> 'a
(** [power_cut t] cuts the power of the machine: it cancels every fiber of the
    program, and the runner then restarts every part with [~power:true]. A disk
    calls it when {!decide} answers its power cut. It raises
    [Eio.Cancel.Cancelled] in the calling fiber.

    @raise Invalid_argument
      if [t] is {!none}, or if the program is not running with a failure. *)

val lost : t -> string -> unit
(** [lost t what] records that the program was promised [what] and that the part
    no longer holds it, as an acknowledged file that a power cut emptied. The
    run fails with [what]. A part calls it from [restart].

    @raise Invalid_argument if [t] is {!none}. *)

(** {1:parts Parts} *)

type os = [ `Linux | `Macos ]
(** The type for the kernels a part follows. *)

(** The type for what a part gives the environment. *)
type capability =
  | Fs of Eio.Fs.dir_ty Eio.Path.t * Eio.Fs.dir_ty Eio.Path.t
      (** [env#fs], and [env#cwd] confined to itself. *)
  | Net of [ `Generic ] Eio.Net.ty Eio.Resource.t  (** [env#net]. *)

(** What a package provides to plug a simulated device in. *)
module type PART = sig
  type chaos

  type t
  (** The type for the part's state. It is made outside the program, so it
      outlives a run. *)

  val name : string
  (** [name] names the part in operations, reports and replay keys: ["fs"]. It
      holds no [':'] or ['+']. *)

  val start :
    t ->
    os:os ->
    sw:Eio.Switch.t ->
    clock:float Eio.Time.clock_ty Eio.Resource.t ->
    mono_clock:Eio.Time.Mono.ty Eio.Resource.t ->
    chaos ->
    capability
  (** [start t ~os ~sw ~clock ~mono_clock chaos] makes [t] new and empty, as
      [os] has it, for a run on those clocks, and is its capability. Each
      operation through it calls {!decide} on [chaos] and acts out the answer.
      [sw] ends with the run and its restart. *)

  val restart : t -> power:bool -> capability
  (** [restart t ~power] is the capability of [t] for the program started again.
      [t] keeps what a restart keeps. With [power] false a process was
      restarted: a disk keeps everything, a network keeps the ports held in
      TIME_WAIT. With [power] true the power was cut: a disk keeps what its
      crash rules allow, a network keeps nothing. Capabilities given before no
      longer work after a power cut. *)
end
with type chaos := t

type part
(** The type for parts. *)

module Part (P : PART) : sig
  val v : P.t -> part
  (** [v p] is the part with state [p]. *)
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
      part gives is refused: each of its operations raises [Eio.Io];
    - [clock] and [mono_clock] start at [0.] and jump to the next timer when
      every fiber waits;
    - [secure_random] is drawn from the run's seed, and OCaml's default [Random]
      is reset to it each time an env is made;
    - [stdin] is empty, and [stdout], [stderr] and [Eio.traceln] are kept in
      memory: a failure report shows what the program printed, bounded;
    - [domain_mgr] runs its work in the run's own event loop;
    - [process_mgr] refuses each process with [Eio.Io], and has an empty
      environment;
    - [backend_id] is ["chaos"];
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
    [app] again many times, each time from that state, with one failure of one
    part. After each of those runs it restarts every part and runs [app] again
    with no failure, as a machine restarts a service.

    {b How the failures are picked.} [run] learns the operations of [app] from
    its first run, those of every part in one sequence. It tries every failure a
    part allowed at every operation, but for the many operations of one kind on
    one target, of which it takes the first three, the last and one of each
    length class. So the runs of two parts add up: they do not multiply. Then it
    spends half as many runs again, at least 200 and within [budget] (defaults
    to [20_000]), on runs drawn at random from [seed] (defaults to [0]): the
    other operations, and two failures in a run, on one part or on two, the
    second on the path the first one opened first. With no part that allows a
    failure, it is the first run alone.

    {b When it passes.} [run] passes iff:

    - the first run returns, and a second run with no failure makes the same
      operations;
    - no run hangs or raises an exception other than [Eio.Io] and [End_of_file],
      an assertion of [app] included;
    - after each failure no part reports a promise {!lost}, and [inspect]
      (defaults to doing nothing), called as a fresh process, returns;
    - each restart returns.

    An [Eio.Io] or [End_of_file] that escapes a run with a failure neither
    passes nor fails it: the restart decides.

    The default [Random] state is as it was before, once [run] returns.

    @raise Failure
      with a report of the first failing run: the failure, the operations of the
      run across the parts, what the program printed, the error, and the line to
      paste that runs it alone, as
      [Eio_chaos.run ~replay:"85d85f49/net:connect:1c@0=timeout" parts app]. A
      key names one failure as [device:kind:target@at=name], and two joined by
      [+], whatever the parts they are on.
    @raise Invalid_argument
      if [budget] is not positive, [replay] is not a replay string, two of
      [parts] have the same name or give the same capability. *)

(**/**)

module Private : sig
  val run :
    name:string ->
    ?os:[< os ] ->
    ?budget:int ->
    ?seed:int ->
    ?replay:string ->
    ?setup:(env -> unit) ->
    ?inspect:(env -> unit) ->
    part list ->
    (env -> unit) ->
    unit
  (** [run ~name parts app] is {!val-run} [parts app], for a one-call shortcut
      of another package: its report names [name], and replays as
      [name ~replay:"..." app]. *)
end

(**/**)
