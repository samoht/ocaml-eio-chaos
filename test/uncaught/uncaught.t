An uncaught report prints as its text, the text the Failure carries. The
transcript is plain: NO_COLOR is set for every test here, as a report's
colours are console's theme, which a release of console may change.

  $ ./fail.exe
  Fatal error: exception Eio_chaos.run: the program failed under a fault
    faults:   #1 lookup db times out
    error:    Failure("no database")
    trace:
      #1 lookup db  <- times out
    replay:   Eio_chaos.run ~replay:"dns/355f1c0e/lookup:1.timeout" parts app
  [2]

A check of another package raises its report with Eio_chaos.fail: a table
of what differs, a family of cases grouped under its name, and a summary,
laid out in 80 columns.

  $ ./check.exe
  Fatal error: exception Toy.check: 4 of 5 cases differ from Linux
    scenario                  operations           expected        got
    ----------------------------------------------------------------------------
    case lookup               write a x; stat A    file 1          ENOENT
    dual-stack hold
    +-- listen :: client ::1  tcp4 bind; listen;   EADDRINUSE      ok
    |   server closes first   close
    +-- listen :: client      tcp6 bind; listen    ok ok ok ok ok  ok ok ok
        127.0.0.1 server                           ok would wait   Refused ok ok
        closes first                               would wait      ok would wait
                                                   accepted=6      accepted=6
                                                   then ok         then ok
    failed connect: path      unix-stream connect  Not_found       Refused
    1 passed, 4 differed, 132 left out: 13 that wait 60 seconds for a port to be
    free again, 5 that use up every port, 114 that send off the loopback interface
  [2]
