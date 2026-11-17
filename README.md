# eio-chaos

Chaos tests of [Eio](https://github.com/ocaml-multicore/eio) programs, on
simulated devices that fail together.

An Eio program receives the outside world as values, such as `env#fs` and
`env#net`. `eio-chaos` builds that `env` from simulated devices, called
parts, runs the program on it many times, and in each run makes a part fail
or cuts the power. After each failure it restarts the program on what the
failure left.

The parts come from other packages. [eio-fs](https://github.com/samoht/ocaml-eio-fs)
gives a disk, `Eio_fs_chaos.disk ()`; a network from
[eio-net](https://github.com/samoht/ocaml-eio-net) is to follow.

## Installation

<!-- $MDX skip -->
```sh
opam install eio-chaos
```

## Testing a program

`Eio_chaos.run parts main` runs `main` with an `env` built from `parts`.
`main` takes `env` and reads it with the `Eio.Stdenv` accessors, as it does
under `Eio_main.run`, so the program is not changed. This one saves its
configuration in place, and refuses an empty one when it starts:

```ocaml
# let start env =
    let config = Eio.Path.(env#fs / "config") in
    (match Eio.Path.load config with
    | "" -> failwith "empty config"
    | _ -> ()
    | exception Eio.Io _ -> ());
    Eio.Path.save ~create:(`Or_truncate 0o644) config "new";;
val start : < fs : [> Eio.Fs.dir_ty ] Eio.Path.t; .. > -> unit = <fun>
```

With the disk of eio-fs, `Eio_chaos.run [ Eio_fs_chaos.disk () ] start`
checks it; `Eio_fs_chaos.run start` is the same check, its reports named
after it.

The first run has no failure. Each later run starts from the same state and
meets the failures that a part allows at an operation of the program:

- one operation fails, as a write with `ENOSPC`, or two in one run;
- a condition holds from one operation on, as a disk that is full;
- the power is cut after an operation that changes what a part keeps, or at
  the end, and each part keeps what a cut may leave: the disk keeps what was
  synced, and some or all of the rest.

After each of those runs, every part is restarted, and the program runs
again with no failure on what the parts kept, as a machine restarts a
service.

The check fails if a run hangs, if it raises anything other than `Eio.Io`,
if a part lost something it promised, as a file the program acknowledged
with `Eio_fs_chaos.ack`, or if the restart does not return. An `Eio.Io`
error that escapes a run with a failure is not a failure in itself: the
device did fail, so the restart decides. Here a full disk leaves the file
truncated and empty, and the restart refuses it:

<!-- $MDX skip -->
```
Eio_chaos.run: the restart failed: the program, run again with no fault on what the faulty run left, did not return
  faults:   #3 write /config@0+3 fails with ENOSPC
  ended:    the faulty run raised Eio.Io chaos:ENOSPC /config
  error:    Failure("empty config")
  trace:
    #1 open /config
    #2 open-write /config
    #3 write /config@0+3  <- fails with ENOSPC
  replay:   Eio_chaos.run ~replay:"fs/eed0151a/write:1.enospc" parts app
```

The report gives the failure, the operations of the run on every part, what
the program printed, the error, and one line to paste, which runs that case
alone. To simulate a part without making it fail, wrap it in
`Eio_chaos.quiet`.

## How failures are picked

Each part proposes, at each operation, the failures the kernel allows there,
and says which of its operations matter most: a disk thins the many reads
and writes of one file, and its syncs follow the writes they cover. The
runner chooses among them, in two tiers:

1. The linear tier runs whole: each failure at each operation the part keeps
   first, and a power cut after each change, in its two simplest states,
   every unsynced change lost or every one kept. A check never passes on a
   partial one: the first run stops as soon as the tier needs more runs than
   `~budget` (20,000 by default).
2. The sampled tier spends a fortieth of the budget, or half as much as the
   linear tier if that is more, on runs drawn from `~seed`: the other
   operations, pairs of failures, on one part or across two, power cuts
   during the restart, and the other states a cut may leave. A second
   failure goes first to the changes on the path the first one opened.

Adding a part adds its runs to the total: it does not multiply them. A pass
means that no counter-example was found among the failures tried.

Two runs with no failure must make the same kinds of operation in the same
order, as a failure is aimed by its kind and its place among those of its
kind; where only the names differ, a report says where. A run that retries a
failed operation at once spins: it fails as a hang once it retries 10,000
times more than the first run made it, with no time passing. Operations take
time on the run's clock, a sync 1 ms in eio-fs, so a fiber that waits on a
timer still runs.

`~replay` reruns one reported case. Two checks with the same seed make the
same runs and print the same report.

## The env

`env` has every method of `Eio.Stdenv`:

- `fs`, `cwd` and `net` come from the parts. A capability no part gives
  raises `Eio.Io` at each operation;
- `clock` and `mono_clock` start at 0 and jump to the next timer when every
  fiber waits, so a sleep takes no time;
- `secure_random` and OCaml's default `Random` are seeded, the same in each
  run, whatever `~seed`, so a replay needs none;
- `stdin` is empty, and `stdout`, `stderr` and `Eio.traceln` are kept for the
  report;
- `domain_mgr` runs its work in the run's own event loop;
- `process_mgr` refuses to start a process;
- `backend_id` is `"chaos"`;
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
      { Eio_chaos.Op.device = "cache"; kind = "read"; target = key;
        offset = 0; length = 0; detail = ""; structure = Every;
        changes = Never }
    in
    match Eio_chaos.decide chaos op ~name:(fun `Stale -> "stale") [ `Stale ] with
    | Some `Stale -> "old"
    | None -> "new";;
val read : Eio_chaos.t -> string -> string = <fun>
# read Eio_chaos.none "k";;
- : string = "new"
```

A device the program builds from `env` takes `env#chaos`. A device that
keeps state across a restart is a part: a module of type `Eio_chaos.PART`,
given to `Eio_chaos.Part`. `start` makes it new for a run, `run` gives the
program its capability on what `setup` left, `cut` cuts its power, and
`restart ~power` gives the capability for the program started again, with
what survives a restart or a power cut. From `restart`, a part calls
`Eio_chaos.lost` for each promise it broke.

## Limitations

- A part gives `env#fs` and `env#cwd`, or `env#net`: one part each.
- Failures fall only on the operations the program made in its first run, at
  most two per run, or one condition.
- `Eio_unix.Stdenv.base` is not provided: `env` has the generic Eio types.

## Related work

FoundationDB, TigerBeetle, turmoil and madsim run disk and network in one
deterministic simulation with one source of choices; `Eio_chaos.decide` is
that source here. The linear tier enumerates failures, as ALICE and
CrashMonkey enumerate crash states. The event loop is a copy of
`Eio_mock`'s, with a clock that prints nothing and stops a run that hangs.
