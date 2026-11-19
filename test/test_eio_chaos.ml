(* The runner on two toy devices, a disk and a resolver: which runs it makes,
   how it judges each, and the env each gets. *)

module P = Eio.Path

let write env k v = P.symlink ~link_to:v P.(env#fs / k)
let read env k = P.read_link P.(env#fs / k)
let sync env = Eio.Path.sync_dir env#fs
let resolve env host = ignore (Eio.Net.getaddrinfo env#net host : _ list)

(* What the runs met: the single failures, the pairs. *)
let singles runs = List.filter (fun r -> List.length r = 1) runs
let pairs runs = List.filter (fun r -> List.length r = 2) runs
let strings = Alcotest.(list string)
let met d = List.sort_uniq compare (List.concat (singles (Disk.runs d)))

(* {1 Runs} *)

let save env =
  write env "a" "1";
  sync env;
  ignore (read env "a" : string)

let clean () =
  let d = Disk.v () in
  Eio_chaos.run [ Eio_chaos.quiet (Disk.device d) ] save;
  Alcotest.(check int) "the first run alone" 1 (List.length (Disk.runs d));
  Alcotest.(check (list bool)) "no restart" [] (Disk.restarts d)

let every_fault () =
  let d = Disk.v () in
  Eio_chaos.run [ Disk.device d ] save;
  Alcotest.(check strings)
    "each failure, condition and lone failure at each operation"
    [
      "read:a=eio";
      "sync:/=eio";
      "sync:/=evict";
      "write:a=eio";
      "write:a=full";
      "write:a=full-from";
    ]
    (met d)

(* Writes that each may fail, and the program goes on. *)
let many env =
  for i = 0 to 99 do
    try write env (Fmt.str "k%d" i) "v" with Eio.Io _ -> ()
  done

let drawn seed =
  let d = Disk.v () in
  Eio_chaos.run ~budget:2000 ~seed [ Disk.device d ] many;
  pairs (Disk.runs d)

let seeded_pairs () =
  let p0 = drawn 0 in
  if p0 = [] then Alcotest.fail "no pair";
  Alcotest.(check (list strings)) "the same seed, the same pairs" p0 (drawn 0);
  if p0 = drawn 1 then Alcotest.fail "another seed drew the same pairs"

(* A thousand writes of one name: the linear tier thins them, so a small
   budget holds it. *)
let thinned () =
  Eio_chaos.run ~budget:200
    [ Disk.device (Disk.v ()) ]
    (fun env ->
      for _ = 1 to 1000 do
        try write env "k" "v" with Eio.Io _ -> ()
      done)

(* A thousand syncs: each one is in the linear tier, which needs more runs
   than the budget long before the first run ends, and it stops there. *)
let early_stop () =
  let syncs = ref 0 in
  let r =
    Expect.report (fun () ->
        Eio_chaos.run ~budget:100
          [ Disk.device (Disk.v ()) ]
          (fun env ->
            for _ = 1 to 1000 do
              incr syncs;
              try sync env with Eio.Io _ -> ()
            done))
  in
  if
    not
      (String.starts_with
         ~prefix:
           "Eio_chaos.run: the linear tier needs more than the budget of 100 \
            runs by operation #"
         r)
  then Alcotest.failf "not stopped by the budget: %s" r;
  if !syncs >= 1000 then Alcotest.fail "the first run did not stop"

(* {1 Conditions and guards} *)

(* Three tries get past a failure, not a lasting condition. *)
let lasting () =
  let r =
    Expect.report (fun () ->
        Eio_chaos.run
          [ Disk.device (Disk.v ()) ]
          (fun env ->
            let rec save n =
              match write env "a" "1" with
              | () -> ()
              | exception Eio.Io _ when n < 3 -> save (n + 1)
              | exception Eio.Io _ -> failwith "three tries failed"
            in
            save 1))
  in
  Alcotest.(check string)
    "faults" "the disk is full from #1 write a on" (Expect.field "faults" r)

(* A retry with no pause spins on a lasting condition: the run hangs. *)
let livelock () =
  let r =
    Expect.report (fun () ->
        Eio_chaos.run
          [ Disk.device (Disk.v ()) ]
          (fun env ->
            let rec save n =
              if n > 50_000 then failwith "the spin was not stopped";
              match write env "a" "1" with
              | () -> ()
              | exception Eio.Io _ -> save (n + 1)
            in
            save 0))
  in
  Alcotest.(check string)
    "error"
    "the run hangs: the same operation on a failed 10003 times while its clock \
     stood still"
    (Expect.field "error" r)

(* A poll of a read that never yields stops at the run's operations. *)
let too_many () =
  Alcotest.(check string)
    "title"
    "Eio_chaos.run: the run with no fault failed: the run made 100,000 disk \
     operations, more than a check plans from: a busy loop, or a program too \
     large for a check"
    (Expect.title
       (Expect.report (fun () ->
            Eio_chaos.run
              ~setup:(fun env -> write env "a" "1")
              [ Disk.device (Disk.v ()) ]
              (fun env ->
                let n = ref 0 in
                while read env "a" = "1" && !n < 1_000_000 do
                  incr n
                done))))

(* A sync takes 1 ms of the run's clock, a failed one none. *)
let op_time () =
  Eio_chaos.run
    [ Disk.device (Disk.v ()) ]
    (fun env ->
      let t0 = Eio.Time.Mono.now env#mono_clock in
      match sync env with
      | () ->
          let d = Mtime.span t0 (Eio.Time.Mono.now env#mono_clock) in
          if Mtime.Span.compare d Mtime.Span.(1 * ms) < 0 then
            Alcotest.failf "a sync took %a" Mtime.Span.pp d
      | exception Eio.Io _ ->
          if not (Mtime.equal t0 (Eio.Time.Mono.now env#mono_clock)) then
            Alcotest.fail "a failed sync took time")

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
  Eio_chaos.run [ Disk.device (Disk.v ()) ] (fun env -> write env "a" "1")

let restart_fails ?os ?replay () =
  Eio_chaos.run ?os ?replay [ Disk.device (Disk.v ()) ] two_writes

let restart_failed () =
  let r = Expect.report restart_fails in
  Alcotest.(check string)
    "title"
    "Eio_chaos.run: the restart failed: the program, run again with no fault \
     on what the faulty run left, raised Failure(\"corrupt: \\\"half\\\"\")"
    (Expect.title r);
  Alcotest.(check string)
    "faults" "#3 write a fails with EIO" (Expect.field "faults" r);
  if not (Expect.contains (Expect.field "error" r) "corrupt") then
    Alcotest.failf "not the restart's error: %s" r

(* A restart that finds "half" waits for ever. *)
let restart_hung () =
  let r =
    Expect.report (fun () ->
        Eio_chaos.run
          [ Disk.device (Disk.v ()) ]
          (fun env ->
            (match read env "a" with
            | "half" -> Eio.Promise.await (fst (Eio.Promise.create ()))
            | _ | (exception Eio.Io _) -> ());
            write env "a" "half";
            write env "a" "done"))
  in
  Alcotest.(check string)
    "title"
    "Eio_chaos.run: the restart failed: the program, run again with no fault \
     on what the faulty run left, hung"
    (Expect.title r)

let other_exception () =
  let r =
    Expect.report (fun () ->
        Eio_chaos.run
          [ Disk.device (Disk.v ()) ]
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
          [ Disk.device (Disk.v ()) ]
          (fun env ->
            let p, u = Eio.Promise.create () in
            (try
               write env "a" "1";
               Eio.Promise.resolve u ()
             with Eio.Io _ -> ());
            Eio.Promise.await p))
  in
  Alcotest.(check string)
    "error" "the run hangs: every fiber waits and no timer is due"
    (Expect.field "error" r)

let replay () =
  let r = Expect.report restart_fails in
  let key = Expect.replay r in
  if not (String.starts_with ~prefix:"fs/" key) then
    Alcotest.failf "not a replay of the disk: %s" key;
  if Expect.contains (Expect.field "replay" r) "~os" then
    Alcotest.failf "a replay under Linux passes ~os: %s" r;
  Alcotest.(check string)
    "the replay fails the same way" r
    (Expect.report (restart_fails ~replay:key))

(* A shortcut of another package names its reports and its replay line. *)
let named () =
  let check ?replay () =
    Eio_chaos.run ~name:"Toy.run" ?replay [ Disk.device (Disk.v ()) ] two_writes
  in
  let r = Expect.report check in
  if not (String.starts_with ~prefix:"Toy.run: the restart failed" r) then
    Alcotest.failf "not named: %s" r;
  let line = Expect.field "replay" r in
  if
    not
      (String.starts_with ~prefix:"Toy.run ~replay:" line
      && String.ends_with ~suffix:"\" app" line)
  then Alcotest.failf "not the shortcut's replay: %s" r;
  Alcotest.(check string)
    "the replay fails the same way" r
    (Expect.report (check ~replay:(Expect.replay r)))

let refused_replay f =
  match f () with
  | () -> Alcotest.fail "the replay ran"
  | exception Invalid_argument m -> m

(* A report under macOS or Windows replays under that kernel: its line
   passes ~os, its key names the kernel, and a replay under another kernel is
   refused. *)
let replay_under (os : Eio_chaos.os) ~arg ~kernel () =
  let r = Expect.report (restart_fails ~os) in
  let line = Expect.field "replay" r in
  if
    not (String.starts_with ~prefix:("Eio_chaos.run " ^ arg ^ " ~replay:") line)
  then Alcotest.failf "the replay does not pass ~os: %s" r;
  let key = Expect.replay r in
  Alcotest.(check string)
    "the replay fails the same way" r
    (Expect.report (restart_fails ~os ~replay:key));
  let m = refused_replay (restart_fails ~replay:key) in
  if not (Expect.contains m arg) then
    Alcotest.failf "the refusal does not say to pass ~os: %s" m;
  if not (Expect.contains m kernel) then
    Alcotest.failf "the refusal does not name %s: %s" kernel m;
  let linux = Expect.replay (Expect.report restart_fails) in
  let m = refused_replay (restart_fails ~os ~replay:linux) in
  if not (Expect.contains m "Linux") then
    Alcotest.failf "the refusal does not name Linux: %s" m

(* The first operation alone needs more runs than a budget of one: the
   report has no trace to show. *)
let over_budget_at_first () =
  let r =
    Expect.report (fun () ->
        Eio_chaos.run ~budget:1
          [ Disk.device (Disk.v ()) ]
          (fun env -> write env "a" "1"))
  in
  if Expect.contains r "trace:" then Alcotest.failf "an empty trace: %S" r

(* {1 Two devices} *)

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

let two_devices () =
  let d = Disk.v () and n = Dns.v () in
  let r =
    Expect.report (fun () -> Eio_chaos.run [ Disk.device d; Dns.device n ] both)
  in
  Alcotest.(check strings)
    "the resolver failed"
    [ "resolve:db=refused"; "resolve:db=timeout" ]
    (List.sort_uniq compare (List.concat (Dns.runs n)));
  if singles (Disk.runs d) = [] then Alcotest.fail "the disk never failed";
  let key = Expect.replay r in
  if
    not
      (String.starts_with ~prefix:"fs+net/" key
      && Expect.contains key "net:resolve:1."
      && Expect.contains key "+fs:write:1.")
  then Alcotest.failf "not a pair across the devices: %s" r

(* What a device says of a cut is labelled with its name once a run has two
   devices. *)
let cut_labelled () =
  let unsafe env =
    write env "b" "1";
    sync env;
    write env "a" "1";
    Disk.ack env "a"
  in
  let r =
    Expect.report (fun () -> Eio_chaos.run [ Disk.device (Disk.v ()) ] unsafe)
  in
  Alcotest.(check string) "one device" "b" (Expect.field "kept" r);
  let r =
    Expect.report (fun () ->
        Eio_chaos.run
          [ Disk.device (Disk.v ()); Dns.device (Dns.v ()) ]
          (fun env ->
            (try resolve env "db" with Eio.Io _ -> ());
            unsafe env))
  in
  Alcotest.(check string) "two devices" "b" (Expect.field "fs kept" r);
  if Expect.contains r "\n  kept:" then
    Alcotest.failf "an unlabelled line: %s" r

let same_name () =
  match
    Eio_chaos.run
      [
        Disk.device (Disk.v ()); Dns.device (Dns.v ()); Disk.device (Disk.v ());
      ]
      save
  with
  | () -> Alcotest.fail "two devices named fs"
  | exception Invalid_argument _ -> ()

(* {1 Power cuts} *)

(* Acknowledged after the sync, or before it. *)
let acked ~early env =
  write env "a" "1";
  if early then Disk.ack env "a";
  sync env;
  if not early then Disk.ack env "a"

let power () =
  let d = Disk.v () in
  Eio_chaos.run [ Disk.device d ] (acked ~early:false);
  let r = Disk.restarts d in
  if not (List.mem true r && List.mem false r) then
    Alcotest.fail "a restart after a power cut, and others"

(* The sync that drops the cache loses the promise first, before any cut. *)
let lost () =
  let r =
    Expect.report (fun () ->
        Eio_chaos.run [ Disk.device (Disk.v ()) ] (acked ~early:true))
  in
  Alcotest.(check string)
    "title"
    "Eio_chaos.run: a fault lost a: it held \"1\" when acknowledged, and is \
     now missing"
    (Expect.title r);
  Alcotest.(check string)
    "faults" "#2 sync / fails with EIO, its cache dropped"
    (Expect.field "faults" r)

(* [b], then [a] that names it, with no sync between: a cut may keep the
   later write and lose the earlier one, a state neither simplest one is. *)
let states () =
  let r =
    Expect.report (fun () ->
        Eio_chaos.run
          [ Disk.device (Disk.v ()) ]
          (fun env ->
            let get k = try read env k with Eio.Io _ -> "" in
            if get "a" = "1" && get "b" <> "1" then failwith "a without b";
            try
              write env "b" "1";
              write env "a" "1"
            with Eio.Io _ -> ()))
  in
  let cut = Expect.field "cut" r in
  if not (Expect.contains cut "of those the cut allows") then
    Alcotest.failf "not a state the cut allows: %s" r

(* A repair the restart acknowledges before it syncs it: only a cut during
   the restart loses it. *)
let cut_in_restart () =
  let r =
    Expect.report (fun () ->
        Eio_chaos.run
          [ Disk.device (Disk.v ()) ]
          (fun env ->
            match read env "a" with
            | "tmp" ->
                write env "a" "1";
                Disk.ack env "a";
                sync env
            | _ | (exception Eio.Io _) -> (
                try
                  write env "a" "tmp";
                  sync env;
                  write env "a" "1";
                  sync env;
                  Disk.ack env "a"
                with Eio.Io _ -> ())))
  in
  if Expect.field "recovery" r = "" then
    Alcotest.failf "the failure is not in the restart: %s" r

(* The cut cancels every fiber of the program, even when the fiber that met
   it swallows the cancellation. *)
let cut_cancels () =
  let d = Disk.v () in
  let cut = ref false and outlived = ref false in
  Eio_chaos.run
    [ Disk.device d ]
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
           [ Disk.device (Disk.v ()) ]
           (fun env ->
             Eio.Flow.copy_string (hex env ^ "\n") env#stdout;
             write env "a" "1";
             failwith "done")))

(* A replay needs no seed: the bytes a program draws do not depend on it. *)
let entropy () =
  let a = printed ~seed:0 () in
  Alcotest.(check string) "the same bytes" a (printed ~seed:0 ());
  Alcotest.(check string)
    "the same bytes, whatever the seed" a (printed ~seed:1 ())

(* A run starts at [0.], and a sleep takes no time. *)
let clocks () =
  let starts = ref [] in
  Eio_chaos.run
    [ Disk.device (Disk.v ()) ]
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
          [ Disk.device (Disk.v ()) ]
          (fun env ->
            Eio.traceln "hello";
            write env "a" "1";
            failwith "done"))
  in
  Alcotest.(check string) "the output" "hello" (Expect.field "output" r)

let refused name f () =
  let r =
    Expect.report (fun () -> Eio_chaos.run [ Disk.device (Disk.v ()) ] f)
  in
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
    [ Disk.device (Disk.v ()) ]
    (fun env ->
      let n = Eio.Domain_manager.run env#domain_mgr (fun () -> 42) in
      if n <> 42 then failwith "domain";
      if env#backend_id <> "chaos" then failwith "backend_id";
      write env "a" "1")

let random () =
  Random.init 7;
  let before = Random.get_state () in
  let draws = ref [] in
  Eio_chaos.run
    [ Disk.device (Disk.v ()) ]
    (fun env ->
      draws := Random.bits () :: !draws;
      write env "a" "1");
  (match List.rev !draws with
  | a :: b :: _ -> Alcotest.(check int) "two runs draw the same" a b
  | _ -> Alcotest.fail "fewer than two runs");
  Alcotest.(check int)
    "Random as it was" (Random.State.bits before) (Random.bits ())

let ack_outside () =
  let kept = ref None in
  Eio_chaos.run
    [ Eio_chaos.quiet (Disk.device (Disk.v ())) ]
    (fun env ->
      kept := Some env;
      write env "a" "1");
  match Disk.ack (Option.get !kept) "a" with
  | () -> Alcotest.fail "a device found after its run"
  | exception Invalid_argument _ -> ()

(* {1 Determinism} *)

let same_report () =
  Alcotest.(check string)
    "two checks print the same"
    (Expect.report restart_fails)
    (Expect.report restart_fails)

(* A counter that outlives the runs makes a different number of operations:
   the faults cannot be aimed, and the check fails. *)
let counter () =
  let n = ref 0 in
  let r =
    Expect.report (fun () ->
        Eio_chaos.run
          [ Disk.device (Disk.v ()) ]
          (fun env ->
            incr n;
            for i = 1 to 1 + (!n mod 2) do
              write env (Fmt.str "k%d" i) "v"
            done))
  in
  if
    not
      (Expect.contains (Expect.title r)
         "two runs with no fault made different operations")
  then Alcotest.failf "not a divergence: %s" r

(* Only the names differ: the faults are aimed by kind and place, and a
   report says where the runs differed. *)
let names_differ () =
  let n = ref 0 in
  let r =
    Expect.report (fun () ->
        Eio_chaos.run
          [ Disk.device (Disk.v ()) ]
          (fun env ->
            incr n;
            try write env (Fmt.str "k%d" !n) "v"
            with Eio.Io _ -> failwith "lost"))
  in
  if not (Expect.contains (Expect.field "diverged" r) "different targets") then
    Alcotest.failf "no divergence reported: %s" r

let bad_arguments () =
  let invalid name f =
    match f () with
    | () -> Alcotest.failf "%s accepted" name
    | exception Invalid_argument _ -> ()
  in
  let d () = [ Disk.device (Disk.v ()) ] in
  invalid "budget 0" (fun () -> Eio_chaos.run ~budget:0 (d ()) ignore);
  invalid "a replay" (fun () -> Eio_chaos.run ~replay:"nope" (d ()) ignore);
  invalid "another device's replay" (fun () ->
      Eio_chaos.run ~replay:"net/0123abcd/none" (d ()) ignore)

let suite =
  ( "eio_chaos",
    [
      Alcotest.test_case "a quiet device: the first run alone" `Quick
        (Expect.passes clean);
      Alcotest.test_case "every failure at every operation" `Quick
        (Expect.passes every_fault);
      Alcotest.test_case "pairs drawn from the seed" `Quick
        (Expect.passes seeded_pairs);
      Alcotest.test_case "a thinned kind fits a small budget" `Quick
        (Expect.passes thinned);
      Alcotest.test_case "the first run stops at the budget" `Quick early_stop;
      Alcotest.test_case "a lasting condition outlasts retries" `Quick lasting;
      Alcotest.test_case "a retry with no pause hangs" `Quick livelock;
      Alcotest.test_case "a run stops at 100,000 operations" `Quick too_many;
      Alcotest.test_case "operations take time" `Quick (Expect.passes op_time);
      Alcotest.test_case "an escaped Eio.Io: the restart decides" `Quick
        (Expect.passes escaped_io);
      Alcotest.test_case "a failed restart" `Quick restart_failed;
      Alcotest.test_case "a restart that hangs" `Quick restart_hung;
      Alcotest.test_case "the first operation over the budget" `Quick
        over_budget_at_first;
      Alcotest.test_case "another exception fails" `Quick other_exception;
      Alcotest.test_case "a hang fails" `Quick hang;
      Alcotest.test_case "a replay fails the same way" `Quick replay;
      Alcotest.test_case "a replay under macOS" `Quick
        (replay_under `Macos ~arg:"~os:`Macos" ~kernel:"macOS");
      Alcotest.test_case "a replay under Windows" `Quick
        (replay_under `Windows ~arg:"~os:`Windows" ~kernel:"Windows");
      Alcotest.test_case "a shortcut names its reports" `Quick named;
      Alcotest.test_case "two devices, and a pair across them" `Quick
        two_devices;
      Alcotest.test_case "a cut's lines name their device" `Quick cut_labelled;
      Alcotest.test_case "two devices with one name" `Quick same_name;
      Alcotest.test_case "restart ~power" `Quick (Expect.passes power);
      Alcotest.test_case "a promise lost to a power cut" `Quick lost;
      Alcotest.test_case "the states a cut allows" `Quick states;
      Alcotest.test_case "a cut during the restart" `Quick cut_in_restart;
      Alcotest.test_case "a power cut cancels the program" `Quick
        (Expect.passes cut_cancels);
      Alcotest.test_case "secure_random does not depend on the seed" `Quick
        entropy;
      Alcotest.test_case "the clocks are simulated" `Quick
        (Expect.passes clocks);
      Alcotest.test_case "output and traceln are captured" `Quick traced;
      Alcotest.test_case "process_mgr is refused" `Quick
        (refused "spawn true" process);
      Alcotest.test_case "a net no device gives is refused" `Quick
        (refused "env#net" (fun env -> resolve env "db"));
      Alcotest.test_case "domain_mgr runs in the loop" `Quick
        (Expect.passes domains);
      Alcotest.test_case "Random is reset, then restored" `Quick
        (Expect.passes random);
      Alcotest.test_case "a device is found only while it runs" `Quick
        ack_outside;
      Alcotest.test_case "two checks print the same" `Quick same_report;
      Alcotest.test_case "different operations are refused" `Quick counter;
      Alcotest.test_case "different names are reported" `Quick names_differ;
      Alcotest.test_case "bad arguments" `Quick bad_arguments;
    ] )
