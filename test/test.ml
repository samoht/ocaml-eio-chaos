let () =
  Alcotest.run "eio-chaos"
    [ Test_eio_chaos.suite; Test_streams.suite; Test_eio_chaos_toy.suite ]
