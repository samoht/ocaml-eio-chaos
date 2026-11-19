open! Hermetic

type Eio.Exn.Backend.t += Not_simulated of string

let () =
  Eio.Exn.Backend.register_pp (fun ppf -> function
    | Not_simulated what ->
        Fmt.pf ppf "%s: no device of the check simulates it" what;
        true
    | _ -> false)

let refused what = Eio.Exn.create (Eio.Exn.Not_available (Not_simulated what))

module Dir = struct
  type t = string

  let no t = raise (refused t)
  let open_in t ~sw:_ ~follow:_ _ = no t
  let open_out t ~sw:_ ~follow:_ ~append:_ ~create:_ _ = no t
  let mkdir t ~perm:_ _ = no t
  let open_subtree t ~sw:_ _ = no t
  let read_dir t _ = no t
  let with_dir_entries t _ _ = no t
  let stat t ~follow:_ _ = no t
  let unlink t _ = no t
  let rmdir t _ = no t
  let rename t _ _ _ = no t
  let read_link t _ = no t
  let symlink ~link_to:_ t _ = no t
  let chmod t ~follow:_ ~perm:_ _ = no t
  let chown ~follow:_ ?uid:_ ?gid:_ t _ = no t
  let sync_dir t _ = no t
  let pp = Fmt.string
  let native _ _ = None

  let split p =
    match String.rindex_opt p '/' with
    | None -> if p = "" then None else Some ("", p)
    | Some i ->
        Some (String.sub p 0 i, String.sub p (i + 1) (String.length p - i - 1))

  let join a b =
    if a = "" || String.starts_with ~prefix:"/" b then b else a ^ "/" ^ b
end

let dir_handler = Eio.Resource.handler [ H (Eio.Fs.Pi.Dir, (module Dir)) ]

let dir what : Eio.Fs.dir_ty Eio.Path.t =
  (Eio.Resource.T (what, dir_handler), "")

module Net = struct
  type t = unit
  type tag = [ `Generic ]

  let no () = raise (refused "env#net")
  let listen () ~reuse_addr:_ ~reuse_port:_ ~backlog:_ ~sw:_ _ = no ()
  let connect () ~bind_to:_ ~options:_ ~sw:_ _ = no ()
  let datagram_socket () ~reuse_addr:_ ~reuse_port:_ ~sw:_ _ = no ()
  let getaddrinfo () ~service:_ _ = no ()
  let getnameinfo () _ = no ()
end

let net = Eio.Resource.T ((), Eio.Net.Pi.network (module Net))

module Processes = struct
  type tag = [ `Generic ]
  type t = unit

  let environment () = Eio.Process.Env.empty
  let getenv_opt () _ = None
  let pipe () ~sw:_ = raise (refused "env#process_mgr: pipe")

  let spawn () ~sw:_ ?cwd:_ ?stdin:_ ?stdout:_ ?stderr:_ ?env:_ ?executable:_
      args =
    raise (refused ("env#process_mgr: spawn " ^ String.concat " " args))
end

let process_mgr = Eio.Resource.T ((), Eio.Process.Pi.mgr (module Processes))

module Domains = struct
  type t = unit

  let run () fn = fn ~cancelled:(fst (Eio.Promise.create ()))
  let run_raw () fn = fn ()
end

let domain_mgr = Eio.Resource.T ((), Eio.Domain_manager.Pi.mgr (module Domains))

module Stdin = struct
  type t = unit

  let read_methods = []
  let single_read () _ = raise End_of_file
end

let stdin = Eio.Resource.T ((), Eio.Flow.Pi.source (module Stdin))

module Sink = struct
  type t = Output.t

  let single_write t bufs =
    List.fold_left
      (fun n b ->
        Output.add t (Cstruct.to_string b);
        n + Cstruct.length b)
      0 bufs

  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
end

let sink out = Eio.Resource.T (out, Eio.Flow.Pi.sink (module Sink))

module Entropy = struct
  type t = Random.State.t

  let read_methods = []

  let single_read t buf =
    for i = 0 to Cstruct.length buf - 1 do
      Cstruct.set_uint8 buf i (Random.State.int t 256)
    done;
    Cstruct.length buf
end

let entropy s = Eio.Resource.T (s, Eio.Flow.Pi.source (module Entropy))

let traceln out =
  {
    Eio.Debug.traceln =
      (fun ?__POS__:_ fmt ->
        Format.kasprintf (fun s -> Output.add out (s ^ "\n")) fmt);
  }
