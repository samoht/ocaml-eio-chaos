(* A program whose check fails, its report left uncaught. *)

let () =
  Console_tty.setup ();
  Eio_chaos.run
    [ Eio_chaos_toy.resolver () ]
    (fun env ->
      match Eio.Net.getaddrinfo_stream env#net "db" with
      | _ -> ()
      | exception Eio.Io _ -> failwith "no database")
