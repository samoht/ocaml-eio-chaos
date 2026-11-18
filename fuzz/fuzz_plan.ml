open Alcobar

let ints = pp_list pp_int

(* {1 Sampling} *)

let test_sample seed n len =
  let n = 1 + n and l = List.init len Fun.id in
  let s = Plan.sample (Random.State.make [| seed |]) n l in
  check_eq ~pp:pp_int (List.length s) (min n len);
  if len > n then
    List.iteri
      (fun i x ->
        if x < i * len / n || x >= (i + 1) * len / n then
          failf "draw %d is %d, outside its run" i x)
      s
  else check_eq ~pp:ints s l

let test_interleave seed len =
  let l = List.init len Fun.id in
  let o = Plan.interleave (Random.State.make [| seed |]) l in
  check_eq ~pp:ints (List.sort compare o) l

let test_share budget linear classes =
  let budget = 1 + budget and classes = 1 + classes in
  let share = Plan.sampled_share ~budget ~linear ~classes in
  check (share >= 0);
  check (share * classes <= max 0 (budget - linear));
  if budget - linear >= budget / 40 then
    check ((share + 1) * classes > min (budget - linear) (budget / 40))

(* {1 Keys} *)

let pos_gen =
  map
    [ range 3; range 1000; choose [ const 0; range 100 ] ]
    (fun k rank at ->
      {
        Plan.device = "fs";
        kind = List.nth [ "write"; "sync"; "open" ] k;
        rank;
        at;
      })

let state_gen =
  choose
    [
      const Plan.Drop;
      const Plan.Keep;
      map [ list (range 5) ] (fun v -> Plan.Enum v);
      map [ range 10_000 ] (fun s -> Plan.Seed s);
      map [ list (range 5) ] (fun v -> Plan.Shape v);
    ]

let cut_gen =
  map [ option pos_gen; state_gen ] (fun before state -> { Plan.before; state })

let fault_gen =
  map
    [ pos_gen; range 3 ]
    (fun pos n ->
      let name = List.nth [ "eio"; "torn3"; "evict-eio" ] n in
      { Plan.pos; name; text = name; alone = n = 2 })

let test_key faults cut rcut single =
  let s = { Plan.none with faults; cut = Some cut; rcut } in
  let k = Plan.key ~single s in
  if not (Plan.well_formed k) then failf "%S is not well formed" k;
  match Plan.split_state_key k with
  | None -> failf "%S has no state" k
  | Some (k', st) ->
      check_eq ~pp:pp_string k'
        (Plan.key ~single { s with cut = Some { cut with state = Drop } });
      check_eq ~pp:pp_string (Plan.state_key st) (Plan.state_key cut.state)

(* {1 States} *)

(* The states of a cut whose draws have these widths, one by one from the
   first: each sequence of choices once. *)
let test_choices widths =
  let widths = List.map (fun w -> 1 + w) (List.take 5 widths) in
  let trail v = List.rev (List.map2 (fun c n -> (c, n)) v widths) in
  let pad v = v @ List.init (List.length widths - List.length v) (fun _ -> 0) in
  let rec go seen v =
    let v = pad v in
    if List.mem v seen then failf "%a twice" ints v;
    match Plan.next_choices (trail v) with
    | None -> v :: seen
    | Some next -> go (v :: seen) next
  in
  let n = List.length (go [] []) in
  check_eq ~pp:pp_int n (List.fold_left ( * ) 1 widths)

let test_pairs cap adds =
  let t = Plan.Pairs.v cap in
  List.iter
    (fun (a, b) -> Plan.Pairs.add t (List.init a Fun.id, List.init b Fun.id))
    adds;
  let total = List.fold_left (fun n (a, b) -> n + a + b) 0 adds in
  let kept = List.length (Plan.Pairs.to_list t) in
  check (kept <= max 0 cap);
  check (kept >= min (max 0 cap) (min total cap))

(* {1 Structure} *)

let op_gen =
  map
    [ range 3; range 3; range 4; range 2000; range 6 ]
    (fun k s t length i ->
      let kind = List.nth [ "write"; "sync"; "stat" ] k in
      let structure =
        match s with 0 -> Op.Every | 1 -> Op.Thin | _ -> Op.After "write"
      in
      {
        Op.device = "fs";
        kind;
        target = Fmt.str "/f%d" t;
        instance = (if i < 2 then "" else Fmt.str "i%d" (t + i));
        offset = 0;
        length;
        detail = "";
        structure;
        changes = Never;
      })

let trace ops =
  let ranks = Hashtbl.create 8 in
  List.mapi
    (fun i (op : Op.t) ->
      let rank = 1 + Option.value ~default:0 (Hashtbl.find_opt ranks op.kind) in
      Hashtbl.replace ranks op.kind rank;
      { Plan.index = i + 1; op; rank; faults = No_faults; places = [] })
    ops

let test_structural ops =
  let t = trace ops in
  let kept, rest = Plan.structural t in
  let index (s : Plan.step) = s.index in
  check_eq ~pp:ints
    (List.sort compare (List.map index (kept @ rest)))
    (List.map index t);
  List.iter
    (fun (s : Plan.step) ->
      if s.op.structure = Every && s.op.instance = "" then
        failf "#%d is Every and not kept" s.index)
    rest;
  (* Sure holds for no operation [structural] leaves out. *)
  let sure = Plan.Sure.v () in
  List.iter
    (fun (s : Plan.step) ->
      if Plan.Sure.add sure s.op && List.memq s rest then
        failf "#%d is sure but left out" s.index)
    t

let suite =
  ( "plan",
    [
      test_case "sample: one draw per run of neighbours"
        [ int; range 50; range 200 ]
        test_sample;
      test_case "interleave: a permutation" [ int; range 300 ] test_interleave;
      test_case "sampled_share: within the budget"
        [ range 100_000; range 50_000; range 4 ]
        test_share;
      test_case "key: well formed, its state split off"
        [ list fault_gen; cut_gen; option cut_gen; bool ]
        test_key;
      test_case "next_choices: each state once" [ list (range 3) ] test_choices;
      test_case "Pairs: at most its cap"
        [ range 50; list (pair (range 20) (range 20)) ]
        test_pairs;
      test_case "structural: a partition, Sure within it"
        [ list op_gen ]
        test_structural;
    ] )
