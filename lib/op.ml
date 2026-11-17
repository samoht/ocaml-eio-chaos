open! Hermetic

type t = {
  device : string;
  kind : string;
  target : string;
  at : int;
  length : int;
}

let pp ppf t =
  Fmt.pf ppf "%s %s %s @%d" t.device t.kind t.target t.at;
  if t.length > 0 then Fmt.pf ppf "+%d" t.length
