# eio-chaos

Chaos tests of [Eio](https://github.com/ocaml-multicore/eio) programs, on
simulated devices that fail together.

An Eio program receives the outside world as values, such as `env#fs` and
`env#net`. `eio-chaos` builds that `env` from simulated devices, called
parts, runs the program on it many times, and makes one part fail in each
run. After each failure it restarts the program on what the failure left.

The parts come from other packages: a disk from
[eio-fs](https://github.com/samoht/ocaml-eio-fs) and a network from
[eio-net](https://github.com/samoht/ocaml-eio-net), as they move onto this
runner. Neither does yet.

## Installation

<!-- $MDX skip -->
```sh
opam install eio-chaos
```

## Testing a program

`Eio_chaos.run parts main` runs `main` with an `env` built from `parts`, such
as a disk and a network:

```ocaml
let check ~disk ~net main = Eio_chaos.run [ disk; net ] main
```

`main` takes `env` and reads it with the `Eio.Stdenv` accessors, as it does
under `Eio_main.run`. The program is not changed.

The first run has no failure. Each later run starts from the same state and
meets one failure, or two, that a part allows at an operation of the
program, such as a reset connection or a power cut. After each of those
runs, every part is restarted, and the program runs again with no failure on
what the parts kept, as a machine restarts a service.

The check fails if a run hangs, if it raises anything other than `Eio.Io` or
`End_of_file`, if a part lost something it promised, or if the restart does
not return. An `Eio.Io` error that escapes a run with a failure is not a
failure in itself: the device did fail, so the restart decides.

The report of a failure gives the failure, the operations of the run on
every part, what the program printed, the error, and one line to paste,
which runs that case alone. Here is one from the tests, on their toy disk:

<!-- $MDX skip -->
```
Eio_chaos.run: the restart failed: the program, run again with no fault on what the faulty run left, did not return
  faults:   eio at fs write a @2
  ended:    the faulty run raised Eio.Io Disk eio,
              creating symlink <<disk>:a> -> done
  error:    Failure("corrupt: \"half\"")
  trace:
    #1 fs read a @1
    #2 fs write a @1+4
    #3 fs write a @2+4  <- eio
  replay:   Eio_chaos.run ~replay:"6aacc4aa/fs:write:a@2=eio" parts app
```

To simulate a part without making it fail, wrap it in `Eio_chaos.quiet`.

## How failures are picked

The first run records the operations of every part, in one sequence. Then
two tiers of runs follow.

1. Every failure a part allows is tried at every operation, one per run. Of
   many operations of one kind on one target, only the first three, the
   last and one of each length class are tried.
2. Half as many runs again, at least 200 and within `~budget` (20,000 by
   default), are drawn from `~seed`. They try the other operations, and
   pairs of failures in one run, on one part or across two. The second
   failure goes first to the operations that the first one led to.

Adding a part adds its runs to the total: it does not multiply them. A pass
means that no counter-example was found among the failures tried.

`~replay` reruns one reported case. Two checks with the same seed make the
same runs and print the same report.

## The env

`env` has every method of `Eio.Stdenv`:

- `fs`, `cwd` and `net` come from the parts. A capability no part gives
  raises `Eio.Io` at each operation;
- `clock` and `mono_clock` start at 0 and jump to the next timer when every
  fiber waits, so a sleep takes no time;
- `secure_random` and OCaml's default `Random` are seeded from `~seed`;
- `stdin` is empty, and `stdout`, `stderr` and `Eio.traceln` are kept for the
  report;
- `domain_mgr` runs its work in the run's own event loop;
- `process_mgr` refuses to start a process;
- `chaos` is the run's `Eio_chaos.t`, for a device the program builds itself.

```ocaml
# Eio_chaos.run [] (fun env ->
    Eio.Time.sleep env#clock 60.;
    assert (Eio.Time.now env#clock >= 60.));;
- : unit = ()
```

## Use hermetic

The check controls what the program reaches through `env`. A direct call to
`Unix`, `open_in`, `Random.self_init` or `Domain.spawn` escapes it.
[hermetic](https://github.com/samoht/ocaml-hermetic) makes the compiler
reject those calls: start each source file with `open! Hermetic`. This holds
for programs and for devices alike.

## Writing a device that can fail

At each operation, a device asks `Eio_chaos.decide` whether the operation
fails, with the failures the kernel allows there, and acts out the answer.
In production it passes `Eio_chaos.none`, which never fails anything:

```ocaml
# let read chaos key =
    let op =
      { Eio_chaos.Op.device = "cache"; kind = "read"; target = key; at = 1;
        length = 0 }
    in
    match Eio_chaos.decide chaos op ~name:(fun `Stale -> "stale") [ `Stale ] with
    | Some `Stale -> "old"
    | None -> "new";;
val read : Eio_chaos.t -> string -> string = <fun>
# read Eio_chaos.none "k";;
- : string = "new"
```

A device the program builds from `env` takes `env#chaos`. A device that
keeps state across a restart is a part. It is a module with `name`, `start`
and `restart`, given to `Eio_chaos.Part`. `start` makes the device new for a
run and returns its capability. `restart ~power` returns the capability for
the program started again, with what survives a restart or a power cut. A
disk asks `decide` about its power cut, and calls `Eio_chaos.power_cut` when
the answer is yes. From `restart`, it calls `Eio_chaos.lost` for each
promise the power cut broke.

## Limitations

- A part gives `env#fs` and `env#cwd`, or `env#net`: one part each.
- Faults fall only on the operations the program made in its first run, at
  most two per run.
- Power cuts during the restart are not tried.
- `Eio_unix.Stdenv.base` is not provided: `env` has the generic Eio types.

## Related work

FoundationDB, TigerBeetle, turmoil and madsim run disk and network in one
deterministic simulation with one source of choices; `Eio_chaos.decide` is
that source here. The first tier enumerates failures, as ALICE and
CrashMonkey enumerate crash states. The event loop is a copy of
`Eio_mock`'s, with a clock that prints nothing and stops a run that hangs.
