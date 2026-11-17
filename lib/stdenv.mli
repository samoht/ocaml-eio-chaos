(** The simulated capabilities of a run's env that no part gives. *)

val refused : string -> exn
(** [refused what] is the [Eio.Io] error of an operation on [what], a capability
    no part simulates: [Not_available]. *)

val dir : string -> Eio.Fs.dir_ty Eio.Path.t
(** [dir what] is a directory each operation of which raises [refused what]. *)

val net : [ `Generic ] Eio.Net.ty Eio.Resource.t
(** [net] is a network each operation of which raises [refused "env#net"]. *)

val process_mgr : [ `Generic ] Eio.Process.mgr_ty Eio.Resource.t
(** [process_mgr] refuses each process and pipe, and has an empty environment.
*)

val domain_mgr : Eio.Domain_manager.ty Eio.Resource.t
(** [domain_mgr] runs its work in the calling fiber. *)

val stdin : Eio.Flow.source_ty Eio.Resource.t
(** [stdin] is empty. *)

val sink : Output.t -> Eio.Flow.sink_ty Eio.Resource.t
(** [sink out] writes to [out]. *)

val entropy : Random.State.t -> Eio.Flow.source_ty Eio.Resource.t
(** [entropy s] is an endless source of bytes drawn from [s]. *)

val traceln : Output.t -> Eio.Debug.traceln
(** [traceln out] writes each line of [Eio.traceln] to [out]. *)
