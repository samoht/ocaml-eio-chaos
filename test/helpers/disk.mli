(** A toy disk part: names that each hold a value, written as symbolic links
    through [env#fs]. A write goes to a cache, and a sync of the root makes the
    cache durable. A power cut keeps what was durable. *)

type t
(** The type for disks. *)

val v : unit -> t
(** [v ()] is a new disk. *)

val part : t -> Eio_chaos.part
(** [part t] is [t] as a part named ["fs"]. Its operations and the failures each
    allows: [write] ([eio], [full]), [read] ([eio]), [sync] of ["/"] ([eio],
    [cut]) and [list] of ["/"] (none). *)

val ack : t -> string -> unit
(** [ack t name] promises that [name] holds what it holds now. *)

val runs : t -> string list list
(** [runs t] is the failures [t] acted out in each run, by key, the first run
    first. *)

val restarts : t -> bool list
(** [restarts t] is the [~power] of each restart of [t], the first first. *)
