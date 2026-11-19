(** A toy network device: a resolver, reached through [env#net]. Each name
    resolves to the loopback address. *)

type t
(** The type for resolvers. *)

val v : unit -> t
(** [v ()] is a new resolver. *)

val device : t -> Eio_chaos.device
(** [device t] is [t] as a device named ["net"]. Its one operation, [resolve] of
    a name, allows [timeout] and [refused]. *)

val runs : t -> string list list
(** [runs t] is the failures [t] acted out in each run, by key, the first run
    first. *)

val restarts : t -> bool list
(** [restarts t] is the [~power] of each restart of [t], the first first. *)
