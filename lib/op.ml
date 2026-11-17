open! Hermetic

type structure = Every | Thin | After of string
type changes = Never | Maybe | Always

type t = {
  device : string;
  kind : string;
  target : string;
  offset : int;
  length : int;
  detail : string;
  structure : structure;
  changes : changes;
}

let pp ppf t =
  Fmt.pf ppf "%s %s" t.kind t.target;
  if t.length > 0 then Fmt.pf ppf "@%d+%d" t.offset t.length

let same a b =
  String.equal a.device b.device
  && String.equal a.kind b.kind
  && String.equal a.target b.target
  && a.offset = b.offset && a.length = b.length
  && String.equal a.detail b.detail
