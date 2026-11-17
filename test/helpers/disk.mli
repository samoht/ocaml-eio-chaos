(** A toy disk part: names that each hold a value, written as symbolic links
    through [env#fs]. A write goes to a cache, and a sync of the root makes the
    cache durable. A power cut keeps what was durable, all of the cache, or what
    its draws choose, name by name. *)

type t
(** The type for disks. *)

val v : unit -> t
(** [v ()] is a new disk. *)

val part : t -> Eio_chaos.part
(** [part t] is [t] as a part named ["fs"]. Its operations and the failures each
    allows:
    - [write] of a name, thinned: [eio] and [full], [full-from] as a lasting
      condition, and [eio] before a power cut;
    - [read] of a name, thinned: [eio];
    - [sync] of ["/"], each one: [eio], and [evict] alone. It changes what the
      disk keeps if the cache differs from what is durable;
    - [list] of ["/"]: none. *)

val ack : Eio_chaos.env -> string -> unit
(** [ack env name] promises that [name] holds what it holds now. *)

val runs : t -> string list list
(** [runs t] is the failures [t] acted out in each run, each as
    [kind:name=fault], the first run first. *)

val restarts : t -> bool list
(** [restarts t] is the [~power] of each restart of [t], the first first. *)
