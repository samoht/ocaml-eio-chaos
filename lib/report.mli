(** The reports of a failed check. *)

(** The type for how a run with a failure ended. *)
type ended = Returned | Raised of exn | Cut  (** By a power cut. *)

(** The type for where a scenario failed. *)
type stage =
  | Setup
  | Run
  | Lost of ended  (** A part lost a promise, before the restart. *)
  | Inspect of ended
  | Restart of ended

type failure = { stage : stage; error : string }
(** The type for the failure of a scenario. *)

val exn_text : exn -> string
(** [exn_text x] is [x] as a report shows it. *)

val pp_trace : Plan.trace Fmt.t
(** [pp_trace] formats the operations of a run, one per line, with the failure
    each met: a block of operations repeated three times or more shows its first
    and last instances, and a long trace its first and last 100 lines and those
    around its first failure. *)

val first : name:string -> printed:string -> Plan.trace -> string -> 'a
(** [first ~name ~printed trace error] fails with the report of the run with no
    failure, which failed with [error]. *)

val scenario :
  name:string ->
  replay:(string -> string) ->
  ?matched:string ->
  printed:string ->
  Plan.scenario ->
  Plan.trace ->
  failure ->
  'a
(** [scenario ~name ~replay ~matched ~printed s trace f] fails with the report
    of [s], whose run made [trace], printed [printed] and failed with [f].
    [replay key] is the line that runs [s] alone. *)
