# eio-chaos

Chaos tests of [Eio](https://github.com/ocaml-multicore/eio) programs, on
simulated devices.

An Eio program receives the outside world as values, such as `env#fs` and
`env#net`. `eio-chaos` builds that `env` from simulated devices and runs
the program on it many times. In each run a device fails as a real one can,
for instance with a full disk or a connection reset. After each fault, the
program starts again on what the fault left, as a machine restarts a
service, and must still work.

The devices come from other packages.
[eio-fs](https://github.com/samoht/ocaml-eio-fs) gives a disk and
[eio-net](https://github.com/samoht/ocaml-eio-net) a network. The examples
below use a toy DNS resolver, whose
[source](lib/toy/eio_chaos_toy.ml) is also an example of a device; it is not
installed.

## Installation

<!-- $MDX skip -->
```sh
opam install eio-chaos
```

## Testing a program

`Eio_chaos.run devices main` runs `main` with an `env` built from `devices`.
`main` reads `env` as it does under `Eio_main.run`, so the program does not
change. This one looks up its database and gives up when the lookup fails:

```ocaml
# let main (env : Eio_chaos.env) =
    match Eio.Net.getaddrinfo_stream (Eio.Stdenv.net env) "db" with
    | _ -> ()
    | exception Eio.Io _ -> failwith "no database";;
val main : Eio_chaos.env -> unit = <fun>
# match Eio_chaos.run [ Eio_chaos_toy.resolver () ] main with
  | () -> print_endline "passed"
  | exception Failure report -> print_endline report;;
Eio_chaos.run: the program failed under a fault
  faults:   #1 lookup db times out
  error:    Failure("no database")
  trace:
    #1 lookup db  <- times out
  replay:   Eio_chaos.run ~replay:"dns/355f1c0e/lookup:1.timeout" devices app
- : unit = ()
```

The report says which fault was injected, what the program did, and gives
one line that replays that case alone. Left uncaught in a program that uses
[console](https://github.com/samoht/ocaml-console), it prints in colour on a
terminal, and as the plain text above anywhere else:

![The report on a terminal, its title in red and the faulted lookup highlighted](doc/report.svg)

A run meets at most two faults, so a program that tries three times passes.
The pause between tries takes no time, as the run's clock jumps to the next
timer:

```ocaml
# let rec lookup (env : Eio_chaos.env) n =
    match Eio.Net.getaddrinfo_stream env#net "db" with
    | addrs -> addrs
    | exception Eio.Io _ when n > 0 ->
        Eio.Time.sleep env#clock 1.;
        lookup env (n - 1);;
val lookup : Eio_chaos.env -> int -> Eio.Net.Sockaddr.stream list = <fun>
# Eio_chaos.run [ Eio_chaos_toy.resolver () ] (fun env -> ignore (lookup env 2));;
- : unit = ()
```

A program that uses several devices is run on all of them, as
`Eio_chaos.run [ Eio_fs_chaos.disk (); Eio_net_chaos.network () ] main`,
and its runs fail the disk, the network, or both.

## Faults and restarts

The first run has no fault, and records the operations the program makes on
each device. Each later run starts from the same state and injects one or two
faults at those operations, or cuts the power. Each device proposes only the
faults the real one can produce at that operation, such as an error, a
read that stops half-way or a disk that fills up.
After each run, the program restarts with no fault on what the devices kept.

The check fails if a run hangs, if the program raises an exception the
device cannot explain, if a device lost something the program had promised to
keep, or if the restart fails. An `Eio.Io` error that escapes a faulty run
is not a failure in itself, since the device did fail: the restart decides.

Everything in `env` is simulated, including the clocks and the random
seed, so a check is deterministic and its replay line always reproduces the
case. A direct call to `Unix` or `open_in` escapes the simulation;
[hermetic](https://github.com/samoht/ocaml-hermetic) makes the compiler
reject such calls.

A pass means that no fault tried broke the program, not that none can. The
faults fall only on the operations of the first run, at most two at a time,
and a long program gets a sample of its pairs rather than all of them.
[`eio_chaos.mli`](lib/eio_chaos.mli) says how the faults are chosen, how the
run budget is spent, and what `env` holds.

## Writing a device

A device is a module of type `Eio_chaos.DEVICE`, given to
`Eio_chaos.Device`. At each operation, the device asks `Eio_chaos.decide`
whether it fails, listing the faults the kernel allows there, and acts out
the answer. In production it gets `Eio_chaos.none`, which never fails
anything, so the same device serves both. The rest of the module says how
the device starts, what a power cut leaves, and how it restarts. The [toy
resolver](lib/toy/eio_chaos_toy.ml) is a whole device in about 60 lines.

## Related work

[FoundationDB](https://apple.github.io/foundationdb/testing.html),
[TigerBeetle](https://docs.tigerbeetle.com/concepts/safety/),
[turmoil](https://github.com/tokio-rs/turmoil) and
[madsim](https://github.com/madsim-rs/madsim) run disk and network in one
deterministic simulation with one source of choices; `Eio_chaos.decide` is
that source here. Enumerating the states a power cut leaves follows
[ALICE](https://research.cs.wisc.edu/adsl/Software/alice/) and
[CrashMonkey](https://github.com/utsaslab/crashmonkey).

## Licence

ISC, see [LICENSE.md](LICENSE.md).
