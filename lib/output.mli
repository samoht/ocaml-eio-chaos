(** What a program printed in the runs of one scenario, bounded. *)

type t
(** The type for printed output. *)

val v : unit -> t
(** [v ()] holds nothing yet. *)

val add : t -> string -> unit
(** [add t s] appends [s] to [t]. [t] keeps its last 2048 bytes or so, and
    counts the others. *)

val mark : t -> string -> unit
(** [mark t line] appends [line] as a separator, if anything was printed. *)

val printed : t -> string
(** [printed t] is what [t] keeps, from the start of a line, after a line that
    says how many bytes came before; [""] if nothing was printed. *)
