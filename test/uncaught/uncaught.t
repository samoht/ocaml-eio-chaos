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

A check of another package raises its report with Eio_chaos.fail: a table
of what differs, a family of cases grouped under its name, and a summary,
laid out in 80 columns. It prints as a run's does, its title, its header and
what each case got in the theme's colours.

  $ ./check.exe
  Fatal error: exception Toy.check: 4 of 5 cases differ from Linux
    scenario                  operations        expected          got
    ------------------------------------------------------------------------------
    case lookup               write a x; stat   file 1            ENOENT
                              A
    dual-stack hold
    +-- listen :: client ::1  tcp4 bind;        EADDRINUSE        ok
    |   server closes first   listen; close
    +-- listen :: client      tcp6 bind;        ok ok ok ok ok    ok ok ok Refused
        127.0.0.1 server      listen            ok would wait     ok ok ok would
        closes first                            would wait        wait accepted=6
                                                accepted=6 then   then ok
                                                ok
    failed connect: path      unix-stream       Not_found         Refused
                              connect
    1 passed, 4 differed, 132 left out: 13 that wait 60 seconds for a port to be
    free again, 5 that use up every port, 114 that send off the loopback interface
  [2]

  $ CONSOLE_CAST=/dev/null TERM=xterm ./check.exe 2>&1 | cat -v
  Fatal error: exception ^[[1mToy.check: ^[[0m^[[1;31m4 of 5 cases differ from Linux^[[0m
    ^[[1mscenario                ^[[0m  ^[[1moperations      ^[[0m  ^[[1mexpected        ^[[0m  ^[[1mgot^[[0m
    ^[[2m------------------------------------------------------------------------------^[[0m
    case lookup               write a x; stat   file 1            ^[[33mENOENT^[[0m
                              A
    ^[[1mdual-stack hold^[[0m
    ^[[2m+-- ^[[0mlisten :: client ::1  tcp4 bind;        EADDRINUSE        ^[[33mok^[[0m
    ^[[2m|   ^[[0mserver closes first   listen; close
    ^[[2m+-- ^[[0mlisten :: client      tcp6 bind;        ok ok ok ok ok    ^[[33mok ok ok Refused^[[0m
    ^[[2m    ^[[0m127.0.0.1 server      listen            ok would wait     ^[[33mok ok ok would^[[0m
    ^[[2m    ^[[0mcloses first                            would wait        ^[[33mwait accepted=6^[[0m
                                                accepted=6 then   ^[[33mthen ok^[[0m
                                                ok
    failed connect: path      unix-stream       Not_found         ^[[33mRefused^[[0m
                              connect
    ^[[2m1 passed, 4 differed, 132 left out: 13 that wait 60 seconds for a port to be^[[0m
    ^[[2mfree again, 5 that use up every port, 114 that send off the loopback interface^[[0m
