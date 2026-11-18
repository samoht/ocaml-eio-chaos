open! Hermetic

type structure = Every | Thin | After of string
type changes = Never | Maybe | Always

type t = {
  device : string;
  kind : string;
  target : string;
  instance : string;
  offset : int;
  length : int;
  detail : string;
  structure : structure;
  changes : changes;
}

let v ?(instance = "") ?(offset = 0) ?(length = 0) ?(detail = "")
    ?(structure = Every) ~device ~kind ~changes target =
  { device; kind; target; instance; offset; length; detail; structure; changes }

let device t = t.device
let kind t = t.kind
let target t = t.target
let instance t = t.instance
let offset t = t.offset
let length t = t.length
let detail t = t.detail
let structure t = t.structure
let changes t = t.changes

let pp ppf t =
  Fmt.pf ppf "%s %s" t.kind t.target;
  if t.length > 0 then Fmt.pf ppf "@%d+%d" t.offset t.length

let same a b =
  String.equal a.device b.device
  && String.equal a.kind b.kind
  && String.equal a.target b.target
  && a.offset = b.offset && a.length = b.length
  && String.equal a.detail b.detail
