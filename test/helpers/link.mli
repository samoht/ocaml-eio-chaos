(** A toy stream network device: links, each with two ends, [a] and [b], and
    what one end sends the other receives. A program reaches it through the
    functions of this module, with [env#chaos]. *)

type t
(** The type for networks of links. *)

val v : unit -> t
(** [v ()] is a network with no link. *)

val device : t -> Eio_chaos.device
(** [device t] is [t] as a device named ["link"]. Its operations, each of the
    instance of its link, and the failures each allows:
    - ["open"] of a link: ["refused"]. Its first one in a run also proposes the
      condition ["down"], under which every open of the run is refused;
    - ["send"] of bytes at an end, thinned: ["cut"], which ends the link, and
      ["slow"] only as the second failure of a pair. Its end is a place of the
      receives at the other end;
    - ["recv"] at an end, thinned: ["cut"], and ["mute"], after which the link
      carries nothing more and the receive waits. Its first one in a run also
      proposes ["trickle"], under which each receive returns one byte. A cut
      inside a receive returns the bytes before it, and the next receive raises
      [End_of_file].

    [End_of_file] is what a failure of a link makes a program raise. A receive
    that fails 100 times in a row stops the run as a hang. *)

type stream
(** The type for links, each a stream either way. *)

val open_ : Eio_chaos.env -> stream
(** [open_ env] is a new link.

    @raise Eio.Io if the open fails. *)

val send : Eio_chaos.env -> stream -> [ `A | `B ] -> string -> unit
(** [send env l e s] sends [s] from end [e].

    @raise End_of_file if [l] was cut. *)

val recv : Eio_chaos.env -> stream -> [ `A | `B ] -> int -> string
(** [recv env l e n] is at most [n] bytes received at end [e], at least one; it
    waits for them.

    @raise End_of_file if [l] was cut. *)

val down : t -> bool
(** [down t] is [true] iff [down] holds in the current run of [t]. *)

val places : t -> int list
(** [places t] is the {!Eio_chaos.place} of each failure [t] acted out, the
    first first. *)

val runs : t -> string list list
(** [runs t] is the failures [t] acted out in each run, each as
    [kind:link=fault], the first run first. *)
