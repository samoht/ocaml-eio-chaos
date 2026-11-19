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

(* The cells a terminal draws [s] in. *)
let cells s = View.width (View.text s)
let widest f l = List.fold_left (fun w x -> max w (f x)) 0 l
let widest_word s = widest cells (String.split_on_char ' ' s)

(* The operations of a line of a sequence, each but the last with its
   [";"]. *)
let steps l =
  let n = String.length l in
  let rec go acc start i =
    if i + 1 >= n then List.rev (String.sub l start (n - start) :: acc)
    else if l.[i] = ';' && l.[i + 1] = ' ' then
      go (String.sub l start (i + 1 - start) :: acc) (i + 2) (i + 2)
    else go acc start (i + 1)
  in
  go [] 0 0

(* The lines of [s] in [w] cells, broken at its spaces; if [seq], between
   two operations before inside one. *)
let broken ~seq w s =
  let pack l =
    let flush line acc =
      match line with None -> acc | Some l -> View.text l :: acc
    in
    let rec go acc line = function
      | [] -> List.rev (flush line acc)
      | op :: rest -> (
          match line with
          | Some l when cells (l ^ " " ^ op) <= w ->
              go acc (Some (l ^ " " ^ op)) rest
          | _ ->
              let acc = flush line acc in
              if cells op <= w then go acc (Some op) rest
              else
                go (List.rev_append (View.wrap w (View.text op)) acc) None rest)
    in
    go [] None (steps l)
  in
  if seq then List.concat_map pack (String.split_on_char '\n' s)
  else View.wrap w (View.text s)

(* A sequence of operations, in the lines of {!broken}. *)
let sequence s =
  View.fit ~min_width:(widest_word s) (fun room ->
      match Room.width room with
      | Some w -> View.vcat (broken ~seq:true w s)
      | None -> View.text s)

(* The cell of a member of a family: the theme's guide, then its name, its
   lines set in after the guide. *)
let member ~last name =
  let guide utf_8 theme = if utf_8 then Theme.guide theme else Guide.ascii in
  let glyphs g =
    if last then (Guide.last g, Guide.space g)
    else (Guide.branch g, Guide.pipe g)
  in
  let width = cells (Guide.branch Guide.ascii) in
  View.fit
    ~min_width:(width + widest_word name)
    (fun room ->
      let g = guide (Room.utf_8 room) (Room.theme room) in
      let first, next = glyphs g in
      let glyph s = View.role `Muted (View.text s) in
      let lines =
        match Room.width room with
        | Some w when w > cells first ->
            View.wrap (w - cells first) (View.text name)
        | _ -> [ View.text name ]
      in
      View.vcat
        (List.mapi
           (fun i l -> View.(glyph (if i = 0 then first else next) ++ l))
           lines))

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

(* A cell as the widths count it: its text set in by [indent] cells, a
   sequence of operations if [seq]. *)
type cell = { indent : int; text : string; seq : bool }

(* The cells [c] takes at its natural size, and in a column [w] wide. *)
let natural c = c.indent + widest cells (String.split_on_char '\n' c.text)

let laid w c =
  if w <= c.indent then natural c
  else c.indent + widest View.width (broken ~seq:c.seq (w - c.indent) c.text)

(* The widest part of [c] that a break would split: an operation, or a
   word. *)
let piece c =
  let lines = String.split_on_char '\n' c.text in
  let split = if c.seq then steps else String.split_on_char ' ' in
  c.indent + widest cells (List.concat_map split lines)

let sum = List.fold_left ( + ) 0

(* Widths for columns of natural widths [ns] in [room] cells, each at least
   its floor in [fs]: each [min n (max f l)] at the highest level [l] that
   fits, then a cell each to the columns still short of their natural width
   while some are left, in [order]. *)
let fill room ns fs order =
  let at l = List.map2 (fun n f -> min n (max f l)) ns fs in
  let top = widest Fun.id ns in
  let rec level l =
    if l < top && sum (at (l + 1)) <= room then level (l + 1) else l
  in
  let ws = Array.of_list (at (level 0)) and ns = Array.of_list ns in
  let left = ref (room - Array.fold_left ( + ) 0 ws) in
  List.iter
    (fun i ->
      if !left > 0 && ws.(i) < ns.(i) then (
        ws.(i) <- ws.(i) + 1;
        decr left))
    order;
  Array.to_list ws

(* The widths of columns of cells [cols] in [room] cells: their natural
   widths if they fit. Else each keeps its widest piece whole while the
   pieces fit, the first column a third of the room too, and they share
   the rest evenly, none wider than its natural width, a cell left over
   going to the column [first] first. Each then narrows to the widest line
   its cells break into. *)
let widths ~first room cols =
  let ns = List.map (widest natural) cols in
  if sum ns <= room then ns
  else
    let order =
      first :: List.filter (( <> ) first) (List.init (List.length cols) Fun.id)
    in
    let fs =
      List.mapi
        (fun i (n, col) ->
          min n (max (widest piece col) (if i = 0 then room / 3 else 0)))
        (List.combine ns cols)
    in
    let fs = if sum fs <= room then fs else List.map (min 1) ns in
    List.map2
      (fun w col -> min w (widest (laid w) col))
      (fill room ns fs order) cols

(* The rows of a table of [groups], each family under its name. *)
let rows ~ops groups =
  let row case d =
    let texts = texts ~ops d in
    let n = List.length texts in
    case
    :: List.mapi
         (fun i s ->
           if i = n - 1 then warned s
           else if ops && i = 0 then sequence s
           else View.text s)
         texts
  in
  List.concat_map
    (function
      | None, members -> List.map (fun (m, d) -> row (View.text m) d) members
      | Some f, members ->
          let n = List.length members in
          [ View.styled Style.bold (View.text f) ]
          :: List.mapi
               (fun i (m, d) -> row (member ~last:(i = n - 1) m) d)
               members)
    groups

(* The cells of each column of a table of [groups] under [heads], as the
   widths count them. *)
let columns ~ops heads groups ds =
  let cell ?(indent = 0) ?(seq = false) text = { indent; text; seq } in
  let guide = cells (Guide.branch Guide.ascii) in
  let cases =
    List.concat_map
      (function
        | None, members -> List.map (fun (m, _) -> cell m) members
        | Some f, members ->
            cell f :: List.map (fun (m, _) -> cell ~indent:guide m) members)
      groups
  in
  let others =
    List.mapi
      (fun i h ->
        cell h
        :: List.map
             (fun d -> cell ~seq:(ops && i = 0) (List.nth (texts ~ops d) i))
             ds)
      (List.tl heads)
  in
  (cell (List.hd heads) :: cases) :: others

let table ds width =
  let ops = List.exists (fun d -> d.operations <> "") ds in
  let groups = groups ds in
  let heads =
    "scenario"
    :: ((if ops then [ "operations" ] else []) @ [ "expected"; "got" ])
  in
  let cols = columns ~ops heads groups ds in
  let ws =
    match width with
    | None -> List.map (widest natural) cols
    | Some w ->
        let room = w - (gap * (List.length cols - 1)) in
        widths ~first:(if ops then 1 else 0) room cols
  in
  let columns =
    List.map2
      (fun h w -> Table.column ~width:w ~max_width:w (View.text h))
      heads ws
  in
  Table.view ~border:Border.simple ~gap columns (rows ~ops groups)

let footer s width =
  let lines =
    match width with
    | Some w -> View.wrap w (View.text s)
    | None -> [ View.text s ]
  in
  View.vcat (List.map (View.role `Muted) lines)

(* [s] with each CRLF and each lone CR a line feed: a CR is no line end
   to console, and on a terminal it would draw over the line. *)
let lf s =
  if not (String.contains s '\r') then s
  else
    let b = Buffer.create (String.length s) in
    String.iteri
      (fun i c ->
        match c with
        | '\r' when i + 1 < String.length s && s.[i + 1] = '\n' -> ()
        | '\r' -> Buffer.add_char b '\n'
        | c -> Buffer.add_char b c)
      s;
    Buffer.contents b

(* [t] with the line ends of {!lf}. *)
let line_ends t =
  let step = function
    | Op s -> Op (lf s)
    | Fault (s, f) -> Fault (lf s, lf f)
    | Elided s -> Elided (lf s)
  in
  let difference d =
    {
      case = lf d.case;
      operations = lf d.operations;
      expected = lf d.expected;
      got = lf d.got;
    }
  in
  {
    name = lf t.name;
    title = lf t.title;
    fields = List.map (fun (k, v) -> (lf k, lf v)) t.fields;
    differences = List.map difference t.differences;
    footer = lf t.footer;
    trace = List.map step t.trace;
    replay = Option.map lf t.replay;
  }

let view t =
  let t = line_ends t in
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
