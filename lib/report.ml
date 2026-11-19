open! Hermetic
open Console

type step = Op of string | Fault of string * string | Elided of string

type t = {
  name : string;
  title : string;
  fields : (string * string) list;
  trace : step list;
  replay : string option;
}

let title name title = { name; title; fields = []; trace = []; replay = None }

(* The parts of a report a style tells apart. *)
type part = Text | Name | Title | Label | Faulted | Marker | Note | Replay

(* The lines of [t], each a sequence of parts. *)
let lines t =
  let w =
    List.fold_left
      (fun w (k, v) -> if v = "" then w else max w (String.length k + 1))
      9 t.fields
  in
  let label k =
    [
      (Text, "  ");
      (Label, k ^ ":");
      (Text, String.make (w - String.length k) ' ');
    ]
  in
  let field (k, v) = if v = "" then None else Some (label k @ [ (Text, v) ]) in
  let step = function
    | Op s -> [ (Text, "    " ^ s) ]
    | Fault (s, f) ->
        [ (Text, "    "); (Faulted, s); (Text, "  "); (Marker, "<- " ^ f) ]
    | Elided s -> [ (Text, "    "); (Note, s) ]
  in
  let trace =
    match t.trace with
    | [] -> []
    | l -> [ (Text, "  "); (Label, "trace:") ] :: List.map step l
  in
  let replay =
    match t.replay with
    | None -> []
    | Some r -> [ label "replay" @ [ (Replay, r) ] ]
  in
  let title, more =
    match String.split_on_char '\n' t.title with
    | first :: more -> (first, more)
    | [] -> (t.title, [])
  in
  [ (Name, t.name ^ ": "); (Title, title) ]
  :: List.map (fun l -> [ (Text, l) ]) more
  @ List.filter_map field t.fields
  @ trace @ replay

let to_string t =
  String.concat "\n"
    (List.map (fun l -> String.concat "" (List.map snd l)) (lines t))

let part (p, s) =
  let v = View.text s in
  match p with
  | Text -> v
  | Name | Faulted -> View.styled Style.bold v
  | Title -> View.styled Style.bold (View.role `Failed v)
  | Label | Note -> View.role `Muted v
  | Marker -> View.role `Warning v
  | Replay -> View.styled Style.bold (View.role `Accent v)

let view t =
  View.concat ~sep:(View.text "\n")
    (List.map (fun l -> View.concat (List.map part l)) (lines t))

let pp ppf t = View.pp ppf (view t)

(* The reports raised, while their text lives. *)
module Raised = Ephemeron.K1.Make (struct
  type t = string

  let equal = ( == )
  let hash = Hashtbl.hash
end)

let raised = Raised.create 8
let lock = Mutex.create ()

let fail t =
  let s = to_string t in
  Mutex.protect lock (fun () -> Raised.replace raised s t);
  failwith s

(* The runtime runs [at_exit] just before it prints an uncaught exception:
   a report printed after that is the uncaught one. Before, its text goes
   where the program puts it, as a test compares it. *)
let exiting = Atomic.make false
let () = at_exit (fun () -> Atomic.set exiting true)

(* [t] as standard error prints it, uncaught, if the program gave it
   colour. *)
let styled t =
  let err = Fmt.stderr in
  match Fmt.style_renderer err with
  | _ when not (Atomic.get exiting) -> None
  | `None -> None
  | `Ansi_tty ->
      let b = Buffer.create 1024 in
      let ppf = Format.formatter_of_buffer b in
      Fmt.set_style_renderer ppf `Ansi_tty;
      Fmt.set_utf_8 ppf (Fmt.utf_8 err);
      Color.set_depth ppf (Color.depth err);
      Theme.set ppf (Theme.get err);
      Fmt.pf ppf "%a@?" pp t;
      Some (Buffer.contents b)

let () =
  Printexc.register_printer (function
    | Failure m ->
        Mutex.protect lock (fun () -> Raised.find_opt raised m)
        |> Option.map (fun t -> Option.value (styled t) ~default:m)
    | _ -> None)
