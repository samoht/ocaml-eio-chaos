(** Tests of [Eio_chaos.run] on a toy disk and a toy resolver. *)

val suite : string * unit Alcotest.test_case list
(** [suite] checks the runs the runner makes, the restart rule, replay, two
    parts at once, power cuts and the env a program gets. *)
