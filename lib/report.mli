(** The report of a failed check: its text, which a [Failure] carries, and the
    styles of its parts, which an output in colour shows. *)

(** The type for the lines of a trace. *)
type step =
  | Op of string  (** An operation, as ["#1 lookup db"]. *)
  | Fault of string * string
      (** An operation and the fault it met, as ["times out"]. *)
  | Elided of string  (** What stands for lines left out. *)

type t = {
  name : string;  (** The check, as ["Eio_chaos.run"]. *)
  title : string;  (** What failed. *)
  fields : (string * string) list;
      (** Names and texts, as [("faults", "#1 lookup db times out")]. A field
          whose text is empty is left out. *)
  trace : step list;  (** The operations of the run, if any. *)
  replay : string option;  (** The line that runs the case alone. *)
}
(** The type for reports. *)

val title : string -> string -> t
(** [title name s] is the report of [name] that is its title [s] alone. *)

val to_string : t -> string
(** [to_string t] is the text of [t]: ["name: title"], then each field, the
    trace and the replay line on lines of their own, the names in a column as
    wide as the longest of them and at least nine. It ends with no line feed. *)

val pp : t Fmt.t
(** [pp] formats a report: its text, as {!to_string} gives it, in the parts of
    the formatter's theme, which show iff its styles are on. The title is bold,
    in the colour of a failure; the names are muted; a faulted operation is bold
    and its fault in the colour of a warning; the replay line is bold, in the
    accent colour. *)

val fail : t -> 'a
(** [fail t] raises [Failure (to_string t)]. Left uncaught, that exception
    prints as {!val-pp} does where the program gave [Fmt.stderr] colour, as
    [Console_tty.setup ()] does on a terminal, and as its text elsewhere.
    Caught, [Printexc.to_string] of it is its text. *)

val reporting : string -> unit
(** [reporting name] makes a [Failure] whose text starts with [name ^ ": "]
    print as that text, its lines kept, though {!fail} did not raise it. *)
