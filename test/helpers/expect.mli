(** What a test expects of a check. *)

val passes : (unit -> unit) -> unit -> unit
(** [passes f ()] fails the test with the report if [f ()] fails. *)

val report : (unit -> unit) -> string
(** [report f] is the report [f ()] fails with. It fails the test if [f ()]
    passes. *)

val title : string -> string
(** [title r] is the first line of the report [r]. *)

val field : string -> string -> string
(** [field name r] is the line of [r] that starts with [name:], without it. *)

val replay : string -> string
(** [replay r] is the replay string of the report [r]. *)

val contains : string -> string -> bool
(** [contains s sub] is [true] iff [sub] occurs in [s]. *)
