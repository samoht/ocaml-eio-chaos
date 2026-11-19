(* How a report prints while the program runs. The uncaught one, styled
   where standard error has colour, is the cram test of uncaught/. *)

let report () =
  Expect.report (fun () ->
      Eio_chaos.run
        [ Eio_chaos_toy.resolver () ]
        (fun env ->
          match Eio.Net.getaddrinfo_stream env#net "db" with
          | _ -> ()
          | exception Eio.Io _ -> failwith "no database"))

(* [Printexc.to_string] of [Failure r] with standard error set to
   [renderer]. *)
let printed renderer r =
  let ppf = Fmt.stderr in
  let old = Fmt.style_renderer ppf in
  Fmt.set_style_renderer ppf renderer;
  Fun.protect
    ~finally:(fun () -> Fmt.set_style_renderer ppf old)
    (fun () -> Printexc.to_string (Failure r))

let plain () =
  let r = report () in
  Alcotest.(check string) "plain" r (printed `None r)

(* Alcotest gives standard error colour under dune, and a test compares
   the text: a report caught and printed is its text, colour or not. *)
let coloured () =
  let r = report () in
  Alcotest.(check string) "coloured" r (printed `Ansi_tty r)

(* A report of another check is [Failure] with its text, caught as a run's
   is: its table and its summary, laid out in 80 columns. *)
let check () =
  let d = Eio_chaos.difference in
  let differences =
    [
      d ~operations:"write a x; stat A" ~expected:"file 1" ~got:"ENOENT"
        "case lookup";
      d ~expected:"EADDRINUSE" ~got:"ok" "hold: listen ::";
      d ~expected:"ok ok ok ok ok ok would wait would wait accepted=6 then ok"
        ~got:"ok ok ok Refused ok ok ok would wait accepted=6 then ok"
        "hold: listen 127.0.0.1";
    ]
  in
  match
    Eio_chaos.fail ~name:"Toy.check" ~differences ~footer:"2 passed, 3 differed"
      "3 of 5 cases differ"
  with
  | () -> Alcotest.fail "fail returned"
  | exception Failure r ->
      Alcotest.(check string)
        "text"
        {|Toy.check: 3 of 5 cases differ
  scenario              operations         expected           got
  -----------------------------------------------------------------------------
  case lookup           write a x; stat A  file 1             ENOENT
  hold
  +-- listen ::                            EADDRINUSE         ok
  +-- listen 127.0.0.1                     ok ok ok ok ok ok  ok ok ok Refused
                                           would wait would   ok ok ok would
                                           wait accepted=6    wait accepted=6
                                           then ok            then ok
  2 passed, 3 differed|}
        r;
      List.iter
        (fun l ->
          if String.length l > 80 then Alcotest.failf "wider than 80: %S" l)
        (String.split_on_char '\n' r);
      Alcotest.(check string) "printed" r (printed `Ansi_tty r)

(* A [Failure] that is no report prints as before. *)
let other () =
  Alcotest.(check string) "other" "Failure(\"x\")" (printed `Ansi_tty "x")

let suite =
  ( "report",
    [
      Alcotest.test_case "its text without colour" `Quick plain;
      Alcotest.test_case "its text with colour" `Quick coloured;
      Alcotest.test_case "another check's" `Quick check;
      Alcotest.test_case "another Failure" `Quick other;
    ] )
