An uncaught report prints as its text where console gives no colour, as on
this pipe:

  $ ./fail.exe
  Fatal error: exception Eio_chaos.run: the program failed under a fault
    faults:   #1 lookup db times out
    error:    Failure("no database")
    trace:
      #1 lookup db  <- times out
    replay:   Eio_chaos.run ~replay:"dns/355f1c0e/lookup:1.timeout" parts app
  [2]

console colours an output it records, as on a terminal: the title, the
labels, the faulted operation and the replay line are styled.

  $ CONSOLE_CAST=/dev/null TERM=xterm ./fail.exe 2>&1 | cat -v
  Fatal error: exception ^[[1mEio_chaos.run: ^[[0m^[[1;31mthe program failed under a fault^[[0m
    ^[[2mfaults:^[[0m   #1 lookup db times out
    ^[[2merror:^[[0m    Failure("no database")
    ^[[2mtrace:^[[0m
      ^[[1m#1 lookup db^[[0m  ^[[33m<- times out^[[0m
    ^[[2mreplay:^[[0m   ^[[1;36mEio_chaos.run ~replay:"dns/355f1c0e/lookup:1.timeout" parts app^[[0m

NO_COLOR and TERM=dumb print it plain there too.

  $ NO_COLOR=1 CONSOLE_CAST=/dev/null TERM=xterm ./fail.exe 2>&1 | cat -v
  Fatal error: exception Eio_chaos.run: the program failed under a fault
    faults:   #1 lookup db times out
    error:    Failure("no database")
    trace:
      #1 lookup db  <- times out
    replay:   Eio_chaos.run ~replay:"dns/355f1c0e/lookup:1.timeout" parts app

  $ CONSOLE_CAST=/dev/null TERM=dumb ./fail.exe 2>&1 | cat -v
  Fatal error: exception Eio_chaos.run: the program failed under a fault
    faults:   #1 lookup db times out
    error:    Failure("no database")
    trace:
      #1 lookup db  <- times out
    replay:   Eio_chaos.run ~replay:"dns/355f1c0e/lookup:1.timeout" parts app
