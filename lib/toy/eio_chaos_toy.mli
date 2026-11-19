(** A toy device, to try {!Eio_chaos.run} and to read as an example of a device.
*)

val resolver : unit -> Eio_chaos.device
(** [resolver ()] is a device named ["dns"] that gives [env#net]. Each name
    resolves to the loopback address, port 80, and a lookup may time out or be
    refused, which raises [Eio.Io]. Every other operation of the network raises
    [Eio.Io]. *)
