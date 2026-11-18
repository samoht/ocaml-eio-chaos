(** Tests of [Eio_chaos_toy], the toy resolver. *)

val suite : string * unit Alcotest.test_case list
(** [suite] checks what the resolver answers, the faults a check meets on it,
    and that it refuses every other operation of a network. *)
