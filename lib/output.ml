open! Hermetic

(* What a report shows: the tail. The buffer holds at most twice that. *)
let shown = 2048

type t = { out : Buffer.t; mutable dropped : int }

let v () = { out = Buffer.create 256; dropped = 0 }

let add t s =
  Buffer.add_string t.out s;
  let n = Buffer.length t.out in
  if n > 2 * shown then begin
    let tail = Buffer.sub t.out (n - shown) shown in
    t.dropped <- t.dropped + n - shown;
    Buffer.clear t.out;
    Buffer.add_string t.out tail
  end

let mark t line = if Buffer.length t.out > 0 then add t (line ^ "\n")

(* The tail of [s] from the line that starts at or after [i], or else from
   the first character there. *)
let from s i =
  let n = String.length s in
  match String.index_from_opt s i '\n' with
  | Some j when j + 1 < n -> (j + 1, String.sub s (j + 1) (n - j - 1))
  | _ ->
      let rec char j =
        if j >= n || Uchar.utf_decode_is_valid (String.get_utf_8_uchar s j) then
          j
        else char (j + 1)
      in
      let j = char i in
      (j, String.sub s j (n - j))

let printed t =
  let s = Buffer.contents t.out in
  let n = String.length s in
  let cut, tail = if n > shown then from s (n - shown) else (0, s) in
  match t.dropped + cut with
  | 0 -> tail
  | d -> Fmt.str "... %d bytes before\n%s" d tail
