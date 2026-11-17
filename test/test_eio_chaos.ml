(* The runner on two toy parts, a disk and a resolver: which runs it makes,
   how it judges each, and the env each gets. *)

module P = Eio.Path

let write env k v = P.symlink ~link_to:v P.(env#fs / k)
let read env k = P.read_link P.(env#fs / k)
let sync env = Eio.Path.sync_dir env#fs
let resolve env host = ignore (Eio.Net.getaddrinfo env#net host : _ list)

(* What the runs met, by key: the single failures, the pairs. *)
let singles runs = List.filter (fun r -> List.length r = 1) runs
let pairs runs = List.filter (fun r -> List.length r = 2) runs
let strings = Alcotest.(list string)
let sort = List.sort compare

(* {1 Runs} *)

let save env =
  write env "a" "1";
  sync env;
  ignore (read env "a" : string)

let clean () =
  let d = Disk.v () in
  Eio_chaos.run [ Eio_chaos.quiet (Disk.part d) ] save;
  Alcotest.(check int) "the first run alone" 1 (List.length (Disk.runs d));
  Alcotest.(check (list bool)) "no restart" [] (Disk.restarts d)

let every_fault () =
  let d = Disk.v () in
  Eio_chaos.run [ Disk.part d ] save;
  Alcotest.(check strings)
    "each failure at each operation, once"
    [
      "fs:read:a@1=eio";
      "fs:sync:/@1=cut";
      "fs:sync:/@1=eio";
      "fs:write:a@1=eio";
      "fs:write:a@1=full";
    ]
    (sort (List.concat (singles (Disk.runs d))))

(* Writes that each may fail, and the program goes on. *)
let many env =
  for i = 0 to 99 do
    try write env (Fmt.str "k%d" i) "v" with Eio.Io _ -> ()
  done

let drawn seed =
  let d = Disk.v () in
  Eio_chaos.run ~budget:300 ~seed [ Disk.part d ] many;
  pairs (Disk.runs d)

let seeded_pairs () =
  let p0 = drawn 0 in
  if p0 = [] then Alcotest.fail "no pair";
  Alcotest.(check (list strings)) "the same seed, the same pairs" p0 (drawn 0);
  if p0 = drawn 1 then Alcotest.fail "another seed drew the same pairs"

(* {1 The restart rule} *)

(* A save in two writes: a failure between them leaves "half", which the
   program refuses at its start. *)
let two_writes env =
  (match read env "a" with
  | "done" -> ()
  | v -> Fmt.failwith "corrupt: %S" v
  | exception Eio.Io _ -> ());
  write env "a" "half";
  write env "a" "done"

let escaped_io () =
  Eio_chaos.run [ Disk.part (Disk.v ()) ] (fun env -> write env "a" "1")

let restart_fails ?replay () =
  Eio_chaos.run ?replay [ Disk.part (Disk.v ()) ] two_writes

let restart_failed () =
  let r = Expect.report restart_fails in
  Alcotest.(check string)
    "title"
    "Eio_chaos.run: the restart failed: the program, run again with no fault \
     on what the faulty run left, did not return"
    (Expect.title r);
  Alcotest.(check string)
    "faults" "eio at fs write a @2" (Expect.field "faults" r);
  if not (Expect.contains (Expect.field "error" r) "corrupt") then
    Alcotest.failf "not the restart's error: %s" r

let other_exception () =
  let r =
    Expect.report (fun () ->
        Eio_chaos.run
          [ Disk.part (Disk.v ()) ]
          (fun env ->
            try write env "a" "1" with Eio.Io _ -> failwith "write failed"))
  in
  Alcotest.(check string)
    "title" "Eio_chaos.run: the program failed under a fault" (Expect.title r)

(* A failed write leaves a promise no fiber resolves. *)
let hang () =
  let r =
    Expect.report (fun () ->
        Eio_chaos.run
          [ Disk.part (Disk.v ()) ]
          (fun env ->
            let p, u = Eio.Promise.create () in
            (try
               write env "a" "1";
               Eio.Promise.resolve u ()
             with Eio.Io _ -> ());
            Eio.Promise.await p))
  in
  if not (Expect.contains (Expect.field "error" r) "hangs") then
    Alcotest.failf "not a hang: %s" r

let replay () =
  let r = Expect.report restart_fails in
  let key = Expect.replay r in
  Alcotest.(check bool)
    "a hash and a key" true
    (Expect.contains key "/fs:write:a@2=eio");
  Alcotest.(check string)
    "the replay fails the same way" r
    (Expect.report (restart_fails ~replay:key))

let replay_one_run () =
  let d = Disk.v () in
  let key = Expect.replay (Expect.report restart_fails) in
  ignore
    (Expect.report (fun () ->
         Eio_chaos.run ~replay:key [ Disk.part d ] two_writes));
  Alcotest.(check (list strings))
    "the two runs with no fault, then the one replayed"
    [ []; []; [ "fs:write:a@2=eio" ] ]
    (Disk.runs d)

(* {1 Two parts} *)

let both env =
  let found =
    try
      resolve env "db";
      true
    with Eio.Io _ -> false
  in
  (try write env "a" "1"
   with Eio.Io _ when not found -> failwith "no database and no disk");
  sync env

let two_parts () =
  let d = Disk.v () and n = Dns.v () in
  let r =
    Expect.report (fun () -> Eio_chaos.run [ Disk.part d; Dns.part n ] both)
  in
  Alcotest.(check (list string))
    "the resolver failed"
    [ "net:resolve:db@1=refused"; "net:resolve:db@1=timeout" ]
    (List.sort_uniq compare (List.concat (Dns.runs n)));
  if singles (Disk.runs d) = [] then Alcotest.fail "the disk never failed";
  let key = Expect.replay r in
  if
    not
      (Expect.contains key "net:resolve:db@1="
      && Expect.contains key "+fs:write:a@1=")
  then Alcotest.failf "not a pair across the parts: %s" r

let same_capability () =
  match
    Eio_chaos.run
      [ Disk.part (Disk.v ()); Dns.part (Dns.v ()); Disk.part (Disk.v ()) ]
      save
  with
  | () -> Alcotest.fail "two parts named fs"
  | exception Invalid_argument _ -> ()

(* {1 Power cuts} *)

(* Acknowledged after the sync, or before it. *)
let acked ~early d env =
  write env "a" "1";
  if early then Disk.ack d "a";
  sync env;
  if not early then Disk.ack d "a"

let power () =
  let d = Disk.v () in
  Eio_chaos.run [ Disk.part d ] (acked ~early:false d);
  let r = Disk.restarts d in
  if not (List.mem true r && List.mem false r) then
    Alcotest.fail "a restart after the power cut, and others"

let lost () =
  let d = Disk.v () in
  let r =
    Expect.report (fun () ->
        Eio_chaos.run [ Disk.part d ] (acked ~early:true d))
  in
  Alcotest.(check string)
    "title" "Eio_chaos.run: a part lost what it promised, before the restart"
    (Expect.title r);
  Alcotest.(check string)
    "error" "a held \"1\" when acknowledged, and is now missing"
    (Expect.field "error" r)

(* The cut cancels every fiber of the program, even when the fiber that met
   it swallows the cancellation. *)
let cut_cancels () =
  let d = Disk.v () in
  let cut = ref false and outlived = ref false in
  Eio_chaos.run
    [ Disk.part d ]
    (fun env ->
      cut := false;
      Eio.Fiber.both
        (fun () ->
          try
            write env "a" "1";
            sync env
          with Eio.Cancel.Cancelled _ -> cut := true)
        (fun () ->
          Eio.Time.sleep env#clock 10.;
          if !cut then outlived := true));
  if not (List.mem true (Disk.restarts d)) then Alcotest.fail "no power cut";
  if !outlived then Alcotest.fail "a fiber outlived the power cut"

(* {1 The env} *)

let hex env =
  let b = Cstruct.create 8 in
  Eio.Flow.read_exact env#secure_random b;
  Cstruct.to_hex_string b

let printed ~seed () =
  Expect.field "output"
    (Expect.report (fun () ->
         Eio_chaos.run ~seed
           [ Disk.part (Disk.v ()) ]
           (fun env ->
             Eio.Flow.copy_string (hex env ^ "\n") env#stdout;
             write env "a" "1";
             failwith "done")))

let entropy () =
  let a = printed ~seed:0 () in
  Alcotest.(check string) "the same seed, the same bytes" a (printed ~seed:0 ());
  if a = printed ~seed:1 () then Alcotest.fail "another seed, the same bytes"

(* A run starts at [0.], and a sleep takes no time; the restart goes on with
   the clock of its run. *)
let clocks () =
  let starts = ref [] in
  Eio_chaos.run
    [ Disk.part (Disk.v ()) ]
    (fun env ->
      let t0 = Eio.Time.now env#clock in
      starts := t0 :: !starts;
      Eio.Time.sleep env#clock 1000.;
      Eio.Time.Mono.sleep env#mono_clock 1.;
      if Eio.Time.now env#clock < t0 +. 1001. then
        failwith "the clock did not move";
      write env "a" "1");
  Alcotest.(check (float 0.))
    "the first run starts at 0." 0.
    (List.hd (List.rev !starts))

let traced () =
  let r =
    Expect.report (fun () ->
        Eio_chaos.run
          [ Disk.part (Disk.v ()) ]
          (fun env ->
            Eio.traceln "hello";
            Eio.Flow.copy_string "on stderr\n" env#stderr;
            write env "a" "1";
            failwith "done"))
  in
  Alcotest.(check string) "the output" "hello" (Expect.field "output" r)

let refused name f () =
  let r = Expect.report (fun () -> Eio_chaos.run [ Disk.part (Disk.v ()) ] f) in
  if
    not
      (String.starts_with
         ~prefix:"Eio_chaos.run: the run with no fault failed: Eio.Io"
         (Expect.title r)
      && Expect.contains r name)
  then Alcotest.failf "not refused: %s" r

let process env =
  Eio.Switch.run @@ fun sw ->
  ignore (Eio.Process.spawn ~sw env#process_mgr [ "true" ] : _ Eio.Process.t)

let domains () =
  Eio_chaos.run
    [ Disk.part (Disk.v ()) ]
    (fun env ->
      let n = Eio.Domain_manager.run env#domain_mgr (fun () -> 42) in
      if n <> 42 then failwith "domain";
      write env "a" "1")

let random () =
  Random.init 7;
  let before = Random.get_state () in
  let draws = ref [] in
  Eio_chaos.run
    [ Disk.part (Disk.v ()) ]
    (fun env ->
      draws := Random.bits () :: !draws;
      write env "a" "1");
  (match List.rev !draws with
  | a :: b :: _ -> Alcotest.(check int) "two runs draw the same" a b
  | _ -> Alcotest.fail "fewer than two runs");
  Alcotest.(check int)
    "Random as it was" (Random.State.bits before) (Random.bits ())

(* {1 Determinism} *)

let same_report () =
  Alcotest.(check string)
    "two checks print the same"
    (Expect.report restart_fails)
    (Expect.report restart_fails)

(* A counter that outlives the runs names a new target each time. *)
let counter () =
  let n = ref 0 in
  let r =
    Expect.report (fun () ->
        Eio_chaos.run
          [ Disk.part (Disk.v ()) ]
          (fun env ->
            incr n;
            write env (Fmt.str "k%d" !n) "v"))
  in
  if
    not
      (Expect.contains (Expect.title r)
         "two runs with no fault made different operations")
  then Alcotest.failf "not a divergence: %s" r

let bad_arguments () =
  let invalid name f =
    match f () with
    | () -> Alcotest.failf "%s accepted" name
    | exception Invalid_argument _ -> ()
  in
  invalid "budget 0" (fun () -> Eio_chaos.run ~budget:0 [] ignore);
  invalid "a replay" (fun () -> Eio_chaos.run ~replay:"nope" [] ignore);
  invalid "a key" (fun () ->
      Eio_chaos.run ~replay:"0123abcd/fs:write" [] ignore)

let suite =
  ( "eio_chaos",
    [
      Alcotest.test_case "a quiet part: the first run alone" `Quick
        (Expect.passes clean);
      Alcotest.test_case "every failure at every operation" `Quick
        (Expect.passes every_fault);
      Alcotest.test_case "pairs drawn from the seed" `Quick
        (Expect.passes seeded_pairs);
      Alcotest.test_case "an escaped Eio.Io: the restart decides" `Quick
        (Expect.passes escaped_io);
      Alcotest.test_case "a failed restart" `Quick restart_failed;
      Alcotest.test_case "another exception fails" `Quick other_exception;
      Alcotest.test_case "a hang fails" `Quick hang;
      Alcotest.test_case "a replay fails the same way" `Quick replay;
      Alcotest.test_case "a replay runs one case" `Quick replay_one_run;
      Alcotest.test_case "two parts, and a pair across them" `Quick two_parts;
      Alcotest.test_case "two parts with one name" `Quick same_capability;
      Alcotest.test_case "restart ~power" `Quick (Expect.passes power);
      Alcotest.test_case "a promise lost to a power cut" `Quick lost;
      Alcotest.test_case "a power cut cancels the program" `Quick
        (Expect.passes cut_cancels);
      Alcotest.test_case "secure_random is seeded" `Quick entropy;
      Alcotest.test_case "the clocks are simulated" `Quick
        (Expect.passes clocks);
      Alcotest.test_case "output and traceln are captured" `Quick traced;
      Alcotest.test_case "process_mgr is refused" `Quick
        (refused "env#process_mgr" process);
      Alcotest.test_case "a net no part gives is refused" `Quick
        (refused "env#net" (fun env -> resolve env "db"));
      Alcotest.test_case "domain_mgr runs in the loop" `Quick
        (Expect.passes domains);
      Alcotest.test_case "Random is reset, then restored" `Quick
        (Expect.passes random);
      Alcotest.test_case "two checks print the same" `Quick same_report;
      Alcotest.test_case "a divergence is reported" `Quick counter;
      Alcotest.test_case "bad arguments" `Quick bad_arguments;
    ] )
