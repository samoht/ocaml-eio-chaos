open! Hermetic
open Console

type step = Op of string | Fault of string * string | Elided of string

type difference = {
  case : string;
  operations : string;
  expected : string;
  got : string;
}

type t = {
  name : string;
  title : string;
  fields : (string * string) list;
  differences : difference list;
  footer : string;
  trace : step list;
  replay : string option;
}

let title name title =
  {
    name;
    title;
    fields = [];
    differences = [];
    footer = "";
    trace = [];
    replay = None;
  }

(* The parts of a report a style tells apart. *)
type part = Text | Name | Title | Label | Faulted | Marker | Note | Replay

(* The lines of [t] above its table, and those below it, each a sequence of
   parts. *)
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
  ( [ (Name, t.name ^ ": "); (Title, title) ]
    :: List.map (fun l -> [ (Text, l) ]) more
    @ List.filter_map field t.fields,
    trace @ replay )

let part (p, s) =
  let v = View.text s in
  match p with
  | Text -> v
  | Name | Faulted -> View.styled Style.bold v
  | Title -> View.styled Style.bold (View.role `Failed v)
  | Label | Note -> View.role `Muted v
  | Marker -> View.role `Warning v
  | Replay -> View.styled Style.bold (View.role `Accent v)

let line l = View.concat (List.map part l)

(* [f width] two cells in, [width] being what that leaves of the room. *)
let indented f =
  View.fit (fun room ->
      let width = Option.map (fun w -> max 1 (w - 2)) (Room.width room) in
      View.hcat ~gap:0 [ View.text "  "; f width ])

(* The family of a case named ["family: member"], where [family] has no
   colon. *)
let family d =
  let n = String.length d.case in
  let rec find i =
    if i + 1 >= n then None
    else if d.case.[i] <> ':' then find (i + 1)
    else if d.case.[i + 1] = ' ' then
      Some (String.sub d.case 0 i, String.sub d.case (i + 2) (n - i - 2))
    else None
  in
  find 0

(* The differences, each family of two or more in one group at the place of
   its first: [(Some family, members)] or [(None, [ d ])]. *)
let groups ds =
  let count f =
    List.length (List.filter (fun d -> Option.map fst (family d) = Some f) ds)
  in
  let rec go seen = function
    | [] -> []
    | d :: rest -> (
        match family d with
        | Some (f, _) when count f > 1 ->
            if List.mem f seen then go seen rest
            else
              let members =
                List.filter_map
                  (fun d ->
                    match family d with
                    | Some (f', m) when f' = f -> Some (m, d)
                    | _ -> None)
                  (d :: rest)
              in
              (Some f, members) :: go (f :: seen) rest
        | _ -> (None, [ (d.case, d) ]) :: go seen rest)
  in
  go [] ds

let widest_word s =
  List.fold_left
    (fun w x -> max w (String.length x))
    0
    (String.split_on_char ' ' s)

(* The cell of a member of a family: the theme's guide, then its name, its
   lines set in after the guide. *)
let member ~last name =
  let guide utf_8 theme = if utf_8 then Theme.guide theme else Guide.ascii in
  let glyphs g =
    if last then (Guide.last g, Guide.space g)
    else (Guide.branch g, Guide.pipe g)
  in
  let width = String.length (Guide.branch Guide.ascii) in
  View.fit
    ~min_width:(width + widest_word name)
    (fun room ->
      let g = guide (Room.utf_8 room) (Room.theme room) in
      let first, next = glyphs g in
      let glyph s = View.role `Muted (View.text s) in
      let lines =
        match Room.width room with
        | Some w when w > View.width (View.text first) ->
            View.wrap (w - View.width (View.text first)) (View.text name)
        | _ -> [ View.text name ]
      in
      View.vcat
        (List.mapi
           (fun i l -> View.(glyph (if i = 0 then first else next) ++ l))
           lines))

(* The widest line of [s]. *)
let natural s =
  List.fold_left
    (fun w l -> max w (View.width (View.text l)))
    0
    (String.split_on_char '\n' s)

(* Widths for columns of natural widths [ns] in [room] cells: each its
   natural width if all fit, else the narrow ones theirs and the others an
   equal share of what is left. *)
let shares room ns =
  let rec go room ns =
    let wide = List.filter (fun n -> n > 0) ns in
    match wide with
    | [] -> ns
    | _ ->
        let share = max 1 (room / List.length wide) in
        if List.for_all (fun n -> n > share) wide then
          List.map (fun n -> if n > 0 then share else 0) ns
        else
          let fixed = List.filter (fun n -> n > 0 && n <= share) ns in
          let left = room - List.fold_left ( + ) 0 fixed in
          let rest = List.map (fun n -> if n <= share then 0 else n) ns in
          List.map2 (fun n r -> if r = 0 then n else r) ns (go left rest)
  in
  if List.fold_left ( + ) 0 ns <= room then ns else go room ns

let gap = 2

(* [s] in the colour of a warning, broken at its spaces as text is: a view
   in a role breaks only where its room ends. *)
let warned s =
  View.fit ~min_width:(widest_word s) (fun room ->
      let lines =
        match Room.width room with
        | Some w -> View.wrap w (View.text s)
        | None -> [ View.text s ]
      in
      View.vcat (List.map (View.role `Warning) lines))

(* The texts of [d] after its case, in the columns of a table [ops] or not. *)
let texts ~ops d =
  (if ops then [ d.operations ] else []) @ [ d.expected; d.got ]

(* The natural widths of the columns of [groups] under [heads]: the first
   that of the widest case, a member after its guide. *)
let naturals ~ops heads groups ds =
  let guide = String.length (Guide.branch Guide.ascii) in
  let first =
    List.fold_left
      (fun w (family, members) ->
        let g = if family = None then 0 else guide in
        List.fold_left (fun w (m, _) -> max w (g + natural m)) w members)
      (String.length (List.hd heads))
      groups
  in
  first
  :: List.fold_left
       (fun ws d -> List.map2 (fun w s -> max w (natural s)) ws (texts ~ops d))
       (List.map String.length (List.tl heads))
       ds

(* The widths of the columns in [width] cells: the cases at most a third,
   the others their shares of what is left. *)
let widths width ns =
  match (width, ns) with
  | None, _ | _, [] -> ns
  | Some w, first :: others ->
      let room = w - (gap * (List.length ns - 1)) in
      let first = min first (room / 3) in
      first :: shares (room - first) others

let table ds width =
  let ops = List.exists (fun d -> d.operations <> "") ds in
  let groups = groups ds in
  let heads =
    "scenario"
    :: ((if ops then [ "operations" ] else []) @ [ "expected"; "got" ])
  in
  let cells case d =
    let texts = texts ~ops d in
    case
    :: List.mapi
         (fun i s ->
           if i = List.length texts - 1 then warned s else View.text s)
         texts
  in
  let rows =
    List.concat_map
      (function
        | None, members ->
            List.map (fun (m, d) -> cells (View.text m) d) members
        | Some f, members ->
            let n = List.length members in
            [ View.styled Style.bold (View.text f) ]
            :: List.mapi
                 (fun i (m, d) -> cells (member ~last:(i = n - 1) m) d)
                 members)
      groups
  in
  let columns =
    List.map2
      (fun h w -> Table.column ~width:w ~max_width:w (View.text h))
      heads
      (widths width (naturals ~ops heads groups ds))
  in
  Table.view ~border:Border.simple ~gap columns rows

let footer s width =
  let lines =
    match width with
    | Some w -> View.wrap w (View.text s)
    | None -> [ View.text s ]
  in
  View.vcat (List.map (View.role `Muted) lines)

let view t =
  let head, tail = lines t in
  View.vcat
    (List.map line head
    @ (match t.differences with [] -> [] | ds -> [ indented (table ds) ])
    @ (if t.footer = "" then [] else [ indented (footer t.footer) ])
    @ List.map line tail)

let pp ppf t = View.pp ppf (view t)

(* A report is laid out in 80 columns, whatever prints it. *)
let render ?(setup = ignore) t =
  let b = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer b in
  Format.pp_set_margin ppf 80;
  setup ppf;
  Fmt.pf ppf "%a@?" pp t;
  Buffer.contents b

let to_string t =
  render
    ~setup:(fun ppf ->
      Fmt.set_style_renderer ppf `None;
      Fmt.set_utf_8 ppf false)
    t

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
      Some
        (render
           ~setup:(fun ppf ->
             Fmt.set_style_renderer ppf `Ansi_tty;
             Fmt.set_utf_8 ppf (Fmt.utf_8 err);
             Color.set_depth ppf (Color.depth err);
             Theme.set ppf (Theme.get err))
           t)

let () =
  Printexc.register_printer (function
    | Failure m ->
        Mutex.protect lock (fun () -> Raised.find_opt raised m)
        |> Option.map (fun t -> Option.value (styled t) ~default:m)
    | _ -> None)
