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

type t = {
  device : string;
  kind : string;
  target : string;
  instance : string;
  offset : int;
  length : int;
  detail : string;
  structure : structure;
  changes : changes;
}
(** The type for operations. *)

val pp : t Fmt.t
(** [pp] formats an operation: its kind, its target, and its offset and length
    if it has a length. *)

val same : t -> t -> bool
(** [same a b] is [true] iff [a] and [b] make the same change: their device,
    kind, target, offset, length and detail are equal. *)
