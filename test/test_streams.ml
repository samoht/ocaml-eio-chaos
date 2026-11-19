(* The runner on a toy device of linked streams: failures inside a read, many
   links alike, mild failures, split operations, conditions of a whole run,
   and what a hang report and a device that sees a spin say. *)

let check = Eio_chaos.run
let met l = List.sort_uniq compare (List.concat (Link.runs l))
let singles l = List.filter (fun r -> List.length r = 1) (Link.runs l)
let pairs l = List.filter (fun r -> List.length r = 2) (Link.runs l)

(* At most [n] bytes, or [End_of_file] once a second passed. *)
let recv env l e n =
  match
    Eio.Time.with_timeout env#clock 1.0 (fun () -> Ok (Link.recv env l e n))
  with
  | Ok s -> s
  | Error `Timeout -> raise End_of_file

(* Two sends from [a], all of whose bytes [b] reads, with [reads] receives
   of at most 64 bytes. *)
let two_sends ~reads env =
  let l = Link.open_ env in
  Link.send env l `A "abc";
  Link.send env l `A "de";
  let rec go acc n =
    if n = 0 || String.length acc >= 5 then acc
    else go (acc ^ recv env l `B 64) (n - 1)
  in
  let got = go "" reads in
  if got <> "abcde" then Fmt.failwith "got %S" got

(* A receive returns the bytes of both sends: a cut falls at the end of the
   first, inside it. *)
let inside () =
  let l = Link.v () in
  check
    [ Link.device l ]
    (fun env -> try two_sends ~reads:5 env with End_of_file | Eio.Io _ -> ());
  if not (List.mem 3 (Link.places l)) then
    Alcotest.failf "no failure after 3 bytes: %a"
      Fmt.(Dump.list int)
      (Link.places l)

let inside_report () =
  let r =
    Expect.report (fun () ->
        check
          [ Link.device (Link.v ()) ]
          (fun env ->
            try two_sends ~reads:1 env with End_of_file | Eio.Io _ -> ()))
  in
  if not (String.ends_with ~suffix:"/recv:1@3.cut" (Expect.replay r)) then
    Alcotest.failf "not the cut after 3 bytes: %s" r;
  if not (Expect.contains (Expect.field "faults" r) "cut after 3 bytes") then
    Alcotest.failf "the faults do not say where: %s" r

(* A hundred links alike: the linear tier tries a few, so a small budget
   holds it. *)
let alike () =
  let l = Link.v () in
  check ~budget:200
    [ Link.device l ]
    (fun env ->
      for _ = 1 to 100 do
        try
          let k = Link.open_ env in
          Link.send env k `A "x";
          ignore (recv env k `B 8 : string)
        with End_of_file | Eio.Io _ -> ()
      done);
  let opens =
    List.filter_map
      (function
        | [ k ] when String.starts_with ~prefix:"open:" k -> Some k | _ -> None)
      (singles l)
  in
  List.iter
    (fun k -> if not (List.mem k opens) then Alcotest.failf "%s not tried" k)
    [ "open:1=refused"; "open:2=refused"; "open:3=refused"; "open:100=refused" ]

(* A mild failure is tried only second in a pair. *)
let mild () =
  let l = Link.v () in
  check ~budget:2000
    [ Link.device l ]
    (fun env ->
      for _ = 1 to 5 do
        try two_sends ~reads:5 env with End_of_file | Eio.Io _ -> ()
      done);
  let slow r = List.exists (fun k -> Expect.contains k "=slow") r in
  if List.exists slow (singles l) then Alcotest.fail "slow tried alone";
  if not (List.exists slow (pairs l)) then Alcotest.fail "slow never paired"

(* One-byte receives that the program answers each: the receives a
   condition split do not count, the answers do. *)
let split () =
  let r =
    Expect.report (fun () ->
        check
          [ Link.device (Link.v ()) ]
          (fun env ->
            try
              let l = Link.open_ env in
              Link.send env l `A (String.make 120_000 'x');
              let n = ref 0 in
              while !n < 120_000 do
                n := !n + String.length (recv env l `B 65536);
                Link.send env l `B "k"
              done
            with End_of_file | Eio.Io _ -> ()))
  in
  if
    not
      (Expect.contains (Expect.field "error" r)
         "the run made 100,000 link operations, more than a check plans from: \
          a busy loop, or a program too large for a check; trickle multiplied \
          its operations")
  then Alcotest.failf "not the operations a condition split: %s" r

(* A condition of the whole run, which the device learns as the run starts:
   every open of the run is refused. *)
let whole_run () =
  let l = Link.v () in
  check
    [ Link.device l ]
    (fun env ->
      let opened = try Some (Link.open_ env) with Eio.Io _ -> None in
      if Link.down l then begin
        if opened <> None then failwith "opened while down";
        match Link.open_ env with
        | _ -> failwith "opened again while down"
        | exception Eio.Io _ -> ()
      end);
  if not (List.mem "open:1=down" (met l)) then Alcotest.fail "never down"

(* A switch waits on a fiber that receives from a link nobody sends on. *)
let waits () =
  let r =
    Expect.report (fun () ->
        check
          [ Link.device (Link.v ()) ]
          (fun env ->
            Eio.Switch.run @@ fun sw ->
            let l = Link.open_ env in
            Eio.Fiber.fork ~sw (fun () ->
                ignore (Link.recv env l `B 8 : string))))
  in
  if
    not
      (Expect.contains r
         "the run hangs: every fiber waits and no timer is due. The program's \
          own fiber waits on 1 fiber still running, as a switch does once its \
          body returned: a fiber in a recv, last #2 recv 1b@0+8")
  then Alcotest.failf "the report does not name the fiber: %s" r

(* A program that receives again at once from a link that was cut: the device
   sees it spin. *)
let spin () =
  let r =
    Expect.report (fun () ->
        check
          [ Link.device (Link.v ()) ]
          (fun env ->
            let l = Link.open_ env in
            Link.send env l `A "abc";
            let rec go n =
              if n > 1000 then failwith "not stopped"
              else
                match Link.recv env l `B 8 with
                | _ -> ()
                | exception End_of_file -> go (n + 1)
            in
            go 0))
  in
  Alcotest.(check string)
    "error" "the run hangs: a receive failed 100 times in a row"
    (Expect.field "error" r)

(* A failure of the disk reaches the program as the end of a stream, as a
   client of a server the failure stopped reads. *)
let eof env =
  try Eio.Path.symlink ~link_to:"1" Eio.Path.(env#fs / "a")
  with Eio.Io _ -> raise End_of_file

let indeterminate () =
  let r = Expect.report (fun () -> check [ Disk.device (Disk.v ()) ] eof) in
  Alcotest.(check string)
    "title" "Eio_chaos.run: the program failed under a fault" (Expect.title r);
  check [ Disk.device (Disk.v ()); Link.device (Link.v ()) ] eof

(* Turns move the clock, other fibers waiting. *)
let turns () =
  check [] (fun env ->
      let t0 = Eio.Time.now env#clock in
      for _ = 1 to 1000 do
        Eio_chaos.turn env#chaos
      done;
      if Eio.Time.now env#clock -. t0 < 0.000_999 then
        Alcotest.fail "the clock stood still")

let faulted () =
  check
    [ Link.device (Link.v ()) ]
    (fun env ->
      if Eio_chaos.faulted env#chaos then failwith "faulted at the start";
      match Link.open_ env with
      | l -> (
          match Link.send env l `A "x" with
          | () -> ()
          | exception End_of_file ->
              if not (Eio_chaos.faulted env#chaos) then
                failwith "a failure not seen")
      | exception Eio.Io _ -> ())

let suite =
  ( "streams",
    [
      Alcotest.test_case "a failure inside a receive" `Quick inside;
      Alcotest.test_case "a report says where in the receive" `Quick
        inside_report;
      Alcotest.test_case "links alike fit a small budget" `Quick alike;
      Alcotest.test_case "a mild failure comes second" `Quick mild;
      Alcotest.test_case "split operations do not count" `Quick split;
      Alcotest.test_case "a condition of the whole run" `Quick whole_run;
      Alcotest.test_case "a hang names the fibers waited on" `Quick waits;
      Alcotest.test_case "a device stops a spin" `Quick spin;
      Alcotest.test_case "what a failure makes a program raise" `Quick
        indeterminate;
      Alcotest.test_case "turns move the clock" `Quick turns;
      Alcotest.test_case "a run knows it met a failure" `Quick faulted;
    ] )
