open! Hermetic
module Op = Op

type t = unit

let none = ()
let decide () _ ~name:_ _ = None
let power_cut () = invalid_arg "Eio_chaos.power_cut: not implemented"
let lost () _ = invalid_arg "Eio_chaos.lost: not implemented"

type os = [ `Linux | `Macos ]

type capability =
  | Fs of Eio.Fs.dir_ty Eio.Path.t * Eio.Fs.dir_ty Eio.Path.t
  | Net of [ `Generic ] Eio.Net.ty Eio.Resource.t

module type PART = sig
  type chaos
  type t

  val name : string

  val start :
    t ->
    os:os ->
    sw:Eio.Switch.t ->
    clock:float Eio.Time.clock_ty Eio.Resource.t ->
    mono_clock:Eio.Time.Mono.ty Eio.Resource.t ->
    chaos ->
    capability

  val restart : t -> power:bool -> capability
end
with type chaos := t

type part = unit

module Part (P : PART) = struct
  let v (_ : P.t) = ()
end

let quiet () = ()

type env =
  < stdin : Eio.Flow.source_ty Eio.Resource.t
  ; stdout : Eio.Flow.sink_ty Eio.Resource.t
  ; stderr : Eio.Flow.sink_ty Eio.Resource.t
  ; net : [ `Generic ] Eio.Net.ty Eio.Resource.t
  ; domain_mgr : Eio.Domain_manager.ty Eio.Resource.t
  ; process_mgr : [ `Generic ] Eio.Process.mgr_ty Eio.Resource.t
  ; clock : float Eio.Time.clock_ty Eio.Resource.t
  ; mono_clock : Eio.Time.Mono.ty Eio.Resource.t
  ; fs : Eio.Fs.dir_ty Eio.Path.t
  ; cwd : Eio.Fs.dir_ty Eio.Path.t
  ; secure_random : Eio.Flow.source_ty Eio.Resource.t
  ; debug : Eio.Debug.t
  ; backend_id : string
  ; chaos : t >

let run ?os:_ ?budget:_ ?seed:_ ?replay:_ ?setup:_ ?inspect:_ _ _ =
  failwith "Eio_chaos.run: not implemented"

module Private = struct
  let run ~name ?os:_ ?budget:_ ?seed:_ ?replay:_ ?setup:_ ?inspect:_ _ _ =
    failwith (name ^ ": not implemented")
end
