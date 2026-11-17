(* Quick check: [dune build @fuzz]; with AFL: [crow start --cpus=4]. Plan
   and Op are private to eio-chaos: their sources are compiled in here. *)

let () = Alcobar.run "eio-chaos" [ Fuzz_plan.suite ]
