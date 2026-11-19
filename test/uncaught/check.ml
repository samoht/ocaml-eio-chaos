(* A check of another package that fails, its report left uncaught. *)

let () =
  Console_tty.setup ();
  Eio_chaos.fail ~name:"Toy.check"
    "1 of 3 cases differs\n  case a: expected 1, got 2\n  2 passed, 1 differed"
