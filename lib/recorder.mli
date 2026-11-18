(** A run's trace as its report shows it, kept as the run goes.

    At each place, the block of up to eight operations repeated three times or
    more that covers the most operations, the shortest on a tie, collapses to
    its first and last instances, as a retry makes them, if that hides two lines
    or more. Of the lines that leaves, only the first and last hundred and those
    around the first fault are kept, so a run that spins costs no more than one
    that does not. *)

type entry = { index : int; op : Op.t; met : (string * string) option }
(** The type for an operation of a run: its number, and the name and the text of
    the fault it met, if any. *)

type t
(** The type for recorders. *)

val v : unit -> t
(** [v ()] is a recorder with no operation. *)

val add : t -> entry -> unit
(** [add t e] adds [e] to [t]. Two operations repeat each other when their
    device, kind, the parent of their target and the fault they met are equal.
*)

val parent : string -> string
(** [parent target] is the directory [target] is in: ["/"] for ["/f"], ["."] for
    a target with no ['/']. *)

val steps : (entry -> string) -> t -> Report.step list
(** [steps text t] is the lines of the trace, each operation as [text] gives it,
    with the fault it met; a repeated block and lines left out are said in
    words. *)
