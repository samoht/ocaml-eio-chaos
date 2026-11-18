(* The toy resolver: what it answers, and the faults a check meets on it. *)

let lookup env = Eio.Net.getaddrinfo_stream env#net "db"

let resolves () =
  Eio_chaos.run
    [ Eio_chaos_toy.resolver () ]
    (fun env ->
      match lookup env with
      | [ `Tcp (ip, 80) ]
        when String.equal (Fmt.str "%a" Eio.Net.Ipaddr.pp ip) "127.0.0.1" ->
          ()
      | _ -> failwith "not the loopback address"
      | exception Eio.Io _ -> ())

(* A program that turns a failed lookup into its own exception fails at the
   first fault. *)
let fails () =
  let r =
    Expect.report (fun () ->
        Eio_chaos.run
          [ Eio_chaos_toy.resolver () ]
          (fun env ->
            match lookup env with
            | _ -> ()
            | exception Eio.Io _ -> failwith "no database"))
  in
  Alcotest.(check string)
    "faults" "#1 lookup db times out" (Expect.field "faults" r);
  let key = Expect.replay r in
  if
    not
      (String.starts_with ~prefix:"dns/" key
      && String.ends_with ~suffix:"/lookup:1.timeout" key)
  then Alcotest.failf "not the lookup's timeout: %s" r

(* A retry after a pause gets past the two faults a run meets at most. *)
let retries () =
  Eio_chaos.run
    [ Eio_chaos_toy.resolver () ]
    (fun env ->
      let rec go n =
        match lookup env with
        | _ -> ()
        | exception Eio.Io _ when n > 0 ->
            Eio.Time.sleep env#clock 1.;
            go (n - 1)
      in
      go 2)

let other_operations () =
  match
    Eio_chaos.run
      [ Eio_chaos_toy.resolver () ]
      (fun env ->
        Eio.Switch.run @@ fun sw ->
        ignore
          (Eio.Net.connect ~sw env#net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 80))
            : _ Eio.Net.stream_socket))
  with
  | () -> Alcotest.fail "a connect passed"
  | exception Failure r ->
      if not (Expect.contains r "Toy not supported") then
        Alcotest.failf "not refused: %s" r

let suite =
  ( "eio_chaos_toy",
    [
      Alcotest.test_case "a name resolves to the loopback address" `Quick
        (Expect.passes resolves);
      Alcotest.test_case "a lookup fails" `Quick fails;
      Alcotest.test_case "a retry gets past the faults" `Quick
        (Expect.passes retries);
      Alcotest.test_case "other operations are refused" `Quick other_operations;
    ] )
