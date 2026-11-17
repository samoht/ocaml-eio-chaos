(** The operations a part asks about. *)

type t = {
  device : string;
  kind : string;
  target : string;
  at : int;
  length : int;
}
(** The type for operations. *)

val pp : t Fmt.t
(** [pp] formats an operation as a trace line shows it: [fs write /f @1+5]. *)
