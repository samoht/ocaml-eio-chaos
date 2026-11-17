let passes f () =
  match f () with () -> () | exception Failure r -> Alcotest.fail r

let report f =
  match f () with
  | () -> Alcotest.fail "the check passed"
  | exception Failure r -> r

let title r = List.hd (String.split_on_char '\n' r)

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
  in
  go 0

let field name r =
  let prefix = "  " ^ name ^ ":" in
  match
    List.find_opt (String.starts_with ~prefix) (String.split_on_char '\n' r)
  with
  | Some l ->
      String.trim
        (String.sub l (String.length prefix)
           (String.length l - String.length prefix))
  | None -> Alcotest.failf "no %s in %s" name r

let replay r =
  let l = field "replay" r in
  match String.index_opt l '"' with
  | Some i -> (
      match String.index_from_opt l (i + 1) '"' with
      | Some j -> String.sub l (i + 1) (j - i - 1)
      | None -> Alcotest.failf "no replay in %s" r)
  | None -> Alcotest.failf "no replay in %s" r
