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
  ----------------------------------------------------------------------------
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

(* The text of the report of [differences]. *)
let text differences =
  match Eio_chaos.fail ~name:"Toy.check" ~differences "some cases differ" with
  | () -> Alcotest.fail "fail returned"
  | exception Failure r -> r

(* A cell of operations breaks between two of them: no operation is split
   while its column holds it, and no column is wider than the lines its
   cells break into. *)
let operations () =
  let d = Eio_chaos.difference in
  let r =
    text
      [
        d ~operations:"write a x; open A x" ~expected:"EEXIST" ~got:"ok"
          "case exclusive create";
        d ~operations:"write a x; rename a A; ls .; read a" ~expected:"x"
          ~got:"ENOENT" "case rename";
        d ~operations:"mkdir d; mkdir d; mkdir d/; write f x; mkdir f; mkdir f/"
          ~expected:"ENOTDIR" ~got:"EEXIST *" "mkdir exists";
        d ~operations:"symlink t/ l; open l c" ~expected:"ENOENT" ~got:"EISDIR"
          "open dangling slash link";
        d ~operations:"write f x; read f" ~expected:"x"
          ~got:"exn Failure(\"no such file\")" "read back";
      ]
  in
  Alcotest.(check string)
    "text"
    {|Toy.check: some cases differ
  scenario                  operations            expected  got
  ------------------------------------------------------------------------------
  case exclusive create     write a x; open A x   EEXIST    ok
  case rename               write a x;            x         ENOENT
                            rename a A; ls .;
                            read a
  mkdir exists              mkdir d; mkdir d;     ENOTDIR   EEXIST *
                            mkdir d/; write f x;
                            mkdir f; mkdir f/
  open dangling slash link  symlink t/ l;         ENOENT    EISDIR
                            open l c
  read back                 write f x; read f     x         exn Failure("no such
                                                            file")|}
    r

(* Names in UTF-8 take the cells a terminal draws them in: the columns line
   up after an accented letter, a decomposed one and the Kelvin sign. *)
let utf_8 () =
  let d = Eio_chaos.difference in
  let r =
    text
      [
        d ~operations:"write \xc3\xa9 x; stat \xc3\x89" ~expected:"file 1"
          ~got:"ENOENT" "case non-ascii";
        d ~operations:"write k x; stat \xe2\x84\xaa" ~expected:"file 1"
          ~got:"ENOENT" "case kelvin";
        d ~operations:"write e\xcc\x81 x; write \xc3\xa9 y; ls ."
          ~expected:"[e\xcc\x81]" ~got:"[e\xcc\x81/\xc3\xa9]" "nfd create";
      ]
  in
  Alcotest.(check string)
    "text"
    "Toy.check: some cases differ\n\
    \  scenario        operations                  expected  got\n\
    \  ------------------------------------------------------------\n\
    \  case non-ascii  write \xc3\xa9 x; stat \xc3\x89           file 1    \
     ENOENT\n\
    \  case kelvin     write k x; stat \xe2\x84\xaa           file 1    ENOENT\n\
    \  nfd create      write e\xcc\x81 x; write \xc3\xa9 y; ls .  \
     [e\xcc\x81]       [e\xcc\x81/\xc3\xa9]"
    r

(* An error whose text ends its lines with CRLF, as Windows'
   [Unix.error_message] does. *)
exception Crlf

let () =
  Printexc.register_printer (function
    | Crlf -> Some "first line\r\nsecond line"
    | _ -> None)

(* A CR in a report's text is a line end, CRLF one line end: its text has
   no CR and no blank line for one. *)
let crlf () =
  let r =
    Expect.report (fun () ->
        Eio_chaos.run
          [ Eio_chaos_toy.resolver () ]
          (fun env ->
            (try ignore (Eio.Net.getaddrinfo_stream env#net "db")
             with Eio.Io _ -> ());
            raise Crlf))
  in
  Alcotest.(check string)
    "title" "Eio_chaos.run: the run with no fault failed: first line"
    (Expect.title r);
  if String.contains r '\r' then Alcotest.failf "a CR in %S" r;
  if not (Expect.contains r "first line\nsecond line") then
    Alcotest.failf "not one line end in %S" r;
  match Eio_chaos.fail ~name:"Toy.check" "a\r\nb\rc" with
  | () -> Alcotest.fail "fail returned"
  | exception Failure r ->
      Alcotest.(check string) "lone CR" "Toy.check: a\nb\nc" r

(* A [Failure] that is no report prints as before. *)
let other () =
  Alcotest.(check string) "other" "Failure(\"x\")" (printed `Ansi_tty "x")

let suite =
  ( "report",
    [
      Alcotest.test_case "its text without colour" `Quick plain;
      Alcotest.test_case "its text with colour" `Quick coloured;
      Alcotest.test_case "another check's" `Quick check;
      Alcotest.test_case "its operations whole" `Quick operations;
      Alcotest.test_case "its names in UTF-8" `Quick utf_8;
      Alcotest.test_case "its line ends" `Quick crlf;
      Alcotest.test_case "another Failure" `Quick other;
    ] )
