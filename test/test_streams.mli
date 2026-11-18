(** Tests of [Eio_chaos.run] on a toy part of linked streams. *)

val suite : string * unit Alcotest.test_case list
(** [suite] checks failures inside an operation, instances alike, mild and split
    operations, conditions of a whole run, hang reports and what a failure makes
    a program raise. *)
