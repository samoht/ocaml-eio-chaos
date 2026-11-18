(** The scenarios of a check, as pure functions of a recorded run. *)

type pos = { device : string; kind : string; rank : int; at : int }
(** The type for the place of an operation in a run: its part, its kind and its
    rank among the operations of that kind of that part, on any target, and the
    bytes into it a fault lands after, [0] at its start. Each run of a check
    makes the same operations up to its first fault, whatever names it gives its
    targets, so a position is found again in another run. *)

val pos_key : single:bool -> pos -> string
(** [pos_key ~single p] names [p] in a key: [kind:rank], after [device:] unless
    [single], and before [@at] if [at] is not [0]. *)

(** {1 Traces} *)

type named = { name : string; text : string Lazy.t }
(** The type for a fault a part allows: its name in keys, and what it does in
    reports. *)

type allowed = {
  once : named list;
  lasting : named list;
  durable : named list;
  alone : named list;
  mild : named list;
}
(** The type for the faults a part allows at an operation, by class. *)

(** The type for the faults a part allows at an operation, in its own type. *)
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
(** The type for an operation a run made: its place in the run, from [1], its
    rank among those of its part and kind, the faults allowed there, and the
    places inside it its {!field-once} faults may land at too: [(k, j)] is after
    [k] bytes, a place operation [j] made. *)

type trace = step list
(** The type for a run's operations, in order. *)

val pos : step -> pos
(** [pos s] is the place of [s]. *)

val at : pos -> step -> bool
(** [at p s] is [true] iff [s] is the operation of [p], whatever its [at]. *)

val allowed : step -> allowed
(** [allowed s] is the faults allowed at [s], named. *)

val structural : trace -> trace * trace
(** [structural t] is the operations of [t] a check tries first, as each one's
    {!Op.structure} says, of an instance it tries first, and the others, each in
    order. Of the instances of each part, a check tries first the first three,
    the last, and the first of each shape: the kinds, details and length classes
    of its operations, on each of its targets named past the instance. *)

(** The operations sure to be structural as a run goes. *)
module Sure : sig
  type t
  (** The type for what a run made so far. *)

  val v : unit -> t
  (** [v ()] is a run that made nothing yet. *)

  val add : t -> Op.t -> bool
  (** [add t op] adds [op] to [t], and is [true] iff {!structural} keeps [op] in
      every trace that starts as [t] does. *)
end

(** {1 Sampling} *)

val sample : Random.State.t -> int -> 'a list -> 'a list
(** [sample rng n l] is [l] if it has [n] or fewer elements. Otherwise it splits
    [l] into [n] runs of consecutive elements whose lengths differ by one at
    most, and is one element of each run, drawn with [rng], in the order of [l].
    Unlike every [k]-th element, it meets each element of [l] for some state of
    [rng], whatever the period of [l]. *)

val interleave : Random.State.t -> 'a list -> 'a list
(** [interleave rng l] is [l] in an order each prefix of which is spread over
    it: when the length of [l] is a power of two, its first [m] elements, for
    each power of two [m], hold one element of each of [m] runs of consecutive
    elements of equal length. [rng] draws which, so any element may come first.
*)

val sampled_share : budget:int -> linear:int -> classes:int -> int
(** [sampled_share ~budget ~linear ~classes] is the runs of the sampled tier for
    each of its [classes]: in all at least a fortieth of [budget], and up to
    half of [linear], the runs the linear tier took, within [budget]. *)

(** The pairs of faults the sampled tier tries: those of each first fault, best
    first, round by round. *)
module Pairs : sig
  type 'a t
  (** The type for pairs kept. *)

  val v : int -> 'a t
  (** [v n] keeps at most [n] pairs. *)

  val add : 'a t -> 'a list * 'a list -> unit
  (** [add t (changed, others)] adds the pairs of one first fault, best first:
      those on an operation it changed, and the others. Past [n], it drops one
      of the others of the largest rank, else one of the changed of the largest
      rank, the latest added first. *)

  val to_list : 'a t -> 'a list
  (** [to_list t] is the pairs kept: on a changed operation by rank, then the
      others by rank, each rank in the order added. *)
end

(** {1 Scenarios} *)

type fault = { pos : pos; name : string; text : string; alone : bool }
(** The type for one fault at one place, and whether it is one of [alone]. *)

(** The type for the state a power cut leaves. *)
type state =
  | Drop  (** Every unsynced change lost. *)
  | Keep  (** Every unsynced change kept. *)
  | Enum of int list
      (** One of the states the cut allows, by the draws that build it. *)
  | Seed of int  (** One drawn at random from a seed. *)
  | Shape of int list
      (** One drawn with a shape, given by the draws that build it. *)

type cut = { before : pos option; state : state }
(** The type for a power cut: before that operation, or at the end. *)

type scenario = {
  faults : fault list;  (** Each met once, in order. *)
  lasting : fault option;  (** From its operation on. *)
  cut : cut option;
  rcut : cut option;  (** During the restart. *)
}
(** The type for the scenarios of a check. Without [cut] it runs with no power
    cut; with one, [faults] are those {!allowed} gives as {!field-durable} or
    {!field-alone}. *)

val none : scenario
(** [none] is the scenario with no fault and no cut. *)

val base : scenario
(** [base] is the cut at the end that loses every unsynced change. *)

val key : single:bool -> scenario -> string
(** [key ~single s] names [s] in a replay. *)

val well_formed : string -> bool
(** [well_formed k] is [true] iff [k] has the shape of the key of a scenario
    with a cut, its states as {!key} names them. *)

val errors_key : single:bool -> string -> bool
(** [errors_key ~single k] is [true] iff [k] has the shape of the key of a
    scenario with no cut, as {!key} names them: ["none"], a lasting condition
    from its place, or faults at their places. *)

val split_state_key : string -> (string * state) option
(** [split_state_key k] is the key of the scenario with a cut [k] names, with
    its state replaced by {!constructor-Drop}, and that state. *)

val state_key : state -> string
(** [state_key st] names [st] in a key. *)

val after_text : string -> int -> string
(** [after_text text k] is [text], what a fault does, as it does it after [k]
    bytes of its operation: [text] itself if [k] is [0]. *)

val pos_text : single:bool -> ?at:(pos -> string option) -> pos -> string
(** [pos_text ~single ~at p] says where [p] is, for a report: as [at p] shows
    the operation there, else by its kind and rank. *)

val faults_text :
  single:bool -> ?at:(pos -> string option) -> fault list -> string
(** [faults_text ~single ~at fs] says what [fs] do, for a report. *)

val text : single:bool -> ?at:(pos -> string option) -> scenario -> string
(** [text ~single ~at s] says what the faults of [s] do, for a report. *)

val cut_text : single:bool -> ?at:(pos -> string option) -> cut -> string
(** [cut_text ~single ~at c] says where [c] falls and what it keeps, for a
    report. *)

(** {1 Runs with no cut} *)

val linear : trace -> scenario list
(** [linear first] is each fault of {!field-once} and each lasting condition
    alone at each structural operation of [first], each fault of {!field-once}
    at each place a structural operation made; then each fault of {!field-alone}
    at each structural operation. *)

val others : trace -> int * (Random.State.t -> int -> scenario list)
(** [others first] is the number of scenarios of {!field-once} and
    {!field-lasting} faults at the other operations of [first], and at its other
    places, and a function that is [n] of them drawn with [rng], as {!sample}
    draws them, built only as they are drawn. *)

val is_alone : scenario -> bool
(** [is_alone s] is [true] iff [s] holds a fault of {!field-alone}: the linear
    tier alone tries it, and no second fault or cut during the restart follows
    it. *)

(** The changes a run made, in order: what tells whether a fault changed its
    course. *)
module Changes : sig
  type t
  (** The type for the operations of a run that may change what a part keeps. *)

  val v : trace -> t
  (** [v first] is those of [first]. *)
end

val seconds :
  ?limit:int ->
  rng:Random.State.t ->
  fault ->
  first:Changes.t ->
  after:trace ->
  scenario list * scenario list
(** [seconds ~limit ~rng f ~first ~after] is the pairs of faults whose first is
    [f] and whose second, of {!field-once} or {!field-mild} or at a place, lands
    after it in [after], the run [f] alone made, best first, at most [limit]
    (defaults to all). If [f] changed what the run changes, past [f], from
    [first], the run with no fault, they are the first list, which aims at the
    path [f] sent the run down: the operations that may change what a part
    keeps, then the others, each nearest [f] first. Else they are the second
    list, drawn with [rng] as {!sample} draws them, in the order {!interleave}
    gives. *)

(** {1 Runs with a cut} *)

val cuts : changed:(step -> bool) -> trace -> scenario list
(** [cuts ~changed first] is a cut after each structural operation of [first]
    that changes what a part keeps, and at its end, in both simplest states. An
    {!Op.Always} operation changes it, another one if [changed] holds for it. *)

val cut_faults : trace -> scenario list
(** [cut_faults first] is each {!field-durable} fault at each structural
    operation of [first], alone, with a cut at the end, in both simplest states;
    then each {!field-alone} fault there. *)

val cut_others : changed:(step -> bool) -> trace -> scenario list
(** [cut_others ~changed first] is the same at the other operations. *)

val rcuts : changed:(step -> bool) -> scenario -> restart:trace -> scenario list
(** [rcuts ~changed s ~restart] is [s] with a cut after each structural change
    of [restart], the trace of its restart, and at its end. *)

val cut_seconds :
  ?limit:int ->
  rng:Random.State.t ->
  fault ->
  first:Changes.t ->
  after:trace ->
  scenario list * scenario list
(** [cut_seconds] is {!seconds} for {!field-durable} faults, with a cut at the
    end. *)

val cut_floor : sure:bool -> changed:bool -> durable:int -> Op.t -> int
(** [cut_floor ~sure ~changed ~durable op] is the runs the linear tier of cuts
    has for [op], given whether it is sure to be structural, whether it changed
    what a part keeps, and how many {!field-durable} and {!field-alone} faults
    it has: none unless [sure]. *)

(** {1 States} *)

val shape : widths:int list -> int -> state
(** [shape ~widths i] is the [i]th state drawn at random for a cut whose draws,
    in the state where each takes its first choice, have [widths] choices: a
    fourth of them from a seed, and the others with a shape, as writeback leaves
    one: a prefix of the draws taking their last choice and the rest their
    first, every draw its first but one, or every draw its last but one. *)

val next_choices : (int * int) list -> int list option
(** [next_choices trail] is the draws of the next state a cut allows, after the
    one whose draws, last first, are [trail], each with the number of choices it
    had; [None] after the last. *)

(** The draws the states of a cut make. *)
module Draws : sig
  type t
  (** The type for the tree of the draws met so far. *)

  val empty : unit -> t
  (** [empty ()] is a tree with no draw. *)

  val add : t -> (int * int) list -> unit
  (** [add t trail] adds the draws, last first, of one state. *)

  val estimate : t -> int
  (** [estimate t] is the number of states the draws met so far allow, a choice
      not yet taken counted as the largest one taken at its draw. *)
end
