(** The operations a part asks about. *)

(** The type for which operations of one kind on one target a check tries its
    faults at first. *)
type structure =
  | Every  (** Each of them. *)
  | Thin
      (** The first three, the last, and the first of each length class, a
          length rounded down to a power of two. *)
  | After of string
      (** Those that follow one the check tries first among the operations of
          that kind on the same target, or follow none. *)

(** The type for whether an operation changes what a part keeps. *)
type changes = Never | Maybe  (** Only where the part says it did. *) | Always

type t
(** The type for operations. *)

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
     target] is the operation of kind [kind] of [device] on [target]: [instance]
    and [detail] default to [""], [offset] and [length] to [0], [structure] to
    {!Every}. {!Eio_chaos.Op.v} documents them. *)

val device : t -> string
(** [device op] is the part [op] is of. *)

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
(** [changes op] is whether [op] changes what its part keeps. *)

val pp : t Fmt.t
(** [pp] formats an operation: its kind, its target, and its offset and length
    if it has a length. *)

val same : t -> t -> bool
(** [same a b] is [true] iff [a] and [b] make the same change: their device,
    kind, target, offset, length and detail are equal. *)
