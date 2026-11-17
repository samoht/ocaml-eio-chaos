(** The runs of a check, as pure functions of a recorded run. *)

type pos = { device : string; kind : string; target : string; at : int }
(** The type for the place of an operation, found again in another run. *)

type fault = { pos : pos; name : string }
(** The type for one failure at one place. *)

type scenario = fault list
(** The type for the failures of a run, each met once, at most two. *)

type step = {
  index : int;  (** From [1], in the run. *)
  op : Op.t;
  allowed : string list;
      (** The names of the failures the operation allowed. *)
  met : string option;  (** The failure the operation met. *)
}
(** The type for an operation a run made. *)

type trace = step list
(** The type for a run's operations, in order. *)

val pos : Op.t -> pos
(** [pos op] is the place of [op]. *)

val none : scenario
(** [none] is the scenario with no failure. *)

val key : scenario -> string
(** [key s] names [s] in a replay: [device:kind:target@at=name] for each
    failure, joined by [+]. *)

val text : scenario -> string
(** [text s] says what [s] does, for a report. *)

val is_key : string -> bool
(** [is_key k] is [true] iff [k] has the form of the {!key} of one failure or
    two. *)

val parse_replay : string -> (string * string) option
(** [parse_replay r] is [Some (hash, key)] iff [r] is [HASH/KEY], [HASH] the 8
    hexadecimal digits of a {!digest} and [KEY] as {!is_key} accepts it. *)

val linear : trace -> scenario list
(** [linear first] is each failure alone at each structural operation of
    [first]. Of the operations of one kind on one target, the structural ones
    are the first three, the last and the first of each length class, a length
    rounded down to a power of two. *)

val others : trace -> scenario list
(** [others first] is each failure alone at the other operations of [first]. *)

val seconds :
  fault -> first:trace -> after:trace -> scenario list * scenario list
(** [seconds f ~first ~after] is the pairs of failures whose first is [f] and
    whose second is at an operation of [after], the run [f] alone made, that
    follows it. The first list holds those at places [first] does not have, the
    path [f] opened; the second the others. Each is in the order of [after]. *)

val sample : Random.State.t -> int -> 'a list -> 'a list
(** [sample rng n l] is [l] if it has [n] or fewer elements. Otherwise it splits
    [l] into [n] runs of consecutive elements whose lengths differ by one at
    most, and is one element of each run, drawn with [rng], in the order of [l].
*)

val sampled_share : budget:int -> linear:int -> classes:int -> int
(** [sampled_share ~budget ~linear ~classes] is the runs of the sampled tier for
    each of its [classes]: in all half of [linear], the runs the linear tier
    took, at least 200, and within [budget]. *)

val digest : trace -> string
(** [digest t] is 8 hexadecimal digits that name the places of [t]. *)

val diverge : trace -> trace -> string option
(** [diverge a b] says where the places of [a] and [b] first differ, if they do.
*)
