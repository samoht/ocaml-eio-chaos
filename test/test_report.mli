(** Tests of how a report prints while the program runs. *)

val suite : string * unit Alcotest.test_case list
(** [suite] checks that [Printexc.to_string] of a report is its text, whether
    standard error has colour or not. *)
