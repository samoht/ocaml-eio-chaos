(** The effect of [Eio_unix.run_in_systhread], answered at once. *)

val handle : 'a Effect.t -> (unit -> 'a) option
(** [handle e] runs the function of [Eio_unix.run_in_systhread] in the calling
    thread, so a device on the host runs in the run's own event loop, and is
    [None] for any other effect. *)
