(* A check of another package that fails, its report left uncaught. *)

let d = Eio_chaos.difference

let () =
  Console_tty.setup ();
  Eio_chaos.fail ~name:"Toy.check"
    ~differences:
      [
        d ~operations:"write a x; stat A" ~expected:"file 1" ~got:"ENOENT"
          "case lookup";
        d ~operations:"tcp4 bind; listen; close" ~expected:"EADDRINUSE"
          ~got:"ok" "dual-stack hold: listen :: client ::1 server closes first";
        d ~operations:"unix-stream connect" ~expected:"Not_found" ~got:"Refused"
          "failed connect: path";
        d ~operations:"tcp6 bind; listen"
          ~expected:"ok ok ok ok ok ok would wait would wait accepted=6 then ok"
          ~got:"ok ok ok Refused ok ok ok would wait accepted=6 then ok"
          "dual-stack hold: listen :: client 127.0.0.1 server closes first";
      ]
    ~footer:
      "1 passed, 4 differed, 132 left out: 13 that wait 60 seconds for a port \
       to be free again, 5 that use up every port, 114 that send off the \
       loopback interface"
    "4 of 5 cases differ from Linux"
