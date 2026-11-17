(* Copied from eio-net's lib/chaos/posix.ml. Outside the Hermetic prelude,
   which refuses Eio_unix: this names the effect of
   Eio_unix.run_in_systhread, and reaches nothing. *)

let handle : type a. a Effect.t -> (unit -> a) option = function
  | Eio_unix.Private.Thread_pool.Run_in_systhread f ->
      Some
        (fun () ->
          let r =
            match f () with
            | v -> Ok v
            | exception x -> Error (x, Printexc.get_raw_backtrace ())
          in
          ( r,
            Eio_unix.Private.Thread_pool.create
              ~sleep_q:(Eio_utils.Zzz.create ()) ))
  | _ -> None
