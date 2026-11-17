(* The sampler of the sampled tier, on a copy of its private module. *)

(* The sampled tier draws one candidate in each of [n] runs of neighbours. *)
let draw seed n len =
  Plan.sample (Random.State.make [| seed |]) n (List.init len Fun.id)

let ints = Alcotest.(list int)

let test_sample_small () =
  Alcotest.check ints "all of a short list" [ 0; 1; 2 ] (draw 0 3 3);
  Alcotest.check ints "all of a shorter list" [ 0; 1 ] (draw 0 3 2)

let test_sample_strata () =
  for seed = 0 to 99 do
    let l = draw seed 3 10 in
    Alcotest.(check int) "one per run" 3 (List.length l);
    List.iteri
      (fun i x ->
        if x < i * 10 / 3 || x >= (i + 1) * 10 / 3 then
          Alcotest.failf "seed %d: draw %d is %d, outside its run" seed i x)
      l
  done

let test_sample_seed () =
  Alcotest.check ints "same seed, same draw" (draw 7 10 30) (draw 7 10 30);
  if List.for_all (fun s -> draw s 10 30 = draw 0 10 30) [ 1; 2; 3 ] then
    Alcotest.fail "every seed draws the same"

(* Candidates that alternate between two kinds, as two faults at each
   operation do: every other one meets only the first kind, the draw of
   seed 0 meets both, and some seed meets each candidate. *)
let test_sample_period () =
  let kinds l = List.sort_uniq compare (List.map (fun x -> x mod 2) l) in
  Alcotest.check ints "both kinds" [ 0; 1 ] (kinds (draw 0 10 20));
  let met = Array.make 20 false in
  for seed = 0 to 63 do
    List.iter (fun x -> met.(x) <- true) (draw seed 10 20)
  done;
  Array.iteri (fun x m -> if not m then Alcotest.failf "no seed draws %d" x) met

(* The pairs of a first fault are kept by prefix: each prefix of 2^k of the
   order holds one candidate of each of 2^k runs of neighbours, and any
   candidate may come first. *)
let test_interleave_prefix () =
  let len = 64 in
  let first = Array.make len false in
  for seed = 0 to 1023 do
    let rng = Random.State.make [| seed |] in
    let l = Plan.interleave rng (List.init len Fun.id) in
    Alcotest.(check (list int))
      "a permutation" (List.init len Fun.id) (List.sort compare l);
    first.(List.hd l) <- true;
    List.iter
      (fun k ->
        let runs =
          List.filteri (fun i _ -> i < k) l
          |> List.map (fun x -> x * k / len)
          |> List.sort_uniq compare
        in
        Alcotest.(check (list int))
          (Fmt.str "seed %d: the first %d" seed k)
          (List.init k Fun.id) runs)
      [ 1; 2; 4; 8; 16; 32; 64 ]
  done;
  Array.iteri
    (fun x m -> if not m then Alcotest.failf "no seed puts %d first" x)
    first

let () =
  Alcotest.run "eio-chaos plan"
    [
      ( "sample",
        [
          Alcotest.test_case "a short list is taken whole" `Quick
            test_sample_small;
          Alcotest.test_case "one draw in each run of neighbours" `Quick
            test_sample_strata;
          Alcotest.test_case "a seed fixes the draw" `Quick test_sample_seed;
          Alcotest.test_case "a period hides no candidate" `Quick
            test_sample_period;
          Alcotest.test_case "each prefix of the pairs' order is spread" `Quick
            test_interleave_prefix;
        ] );
    ]
