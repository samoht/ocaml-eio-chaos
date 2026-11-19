(lang dune 3.21)

(using mdx 0.4)

(name eio-chaos)

(generate_opam_files true)
(implicit_transitive_deps false)

(license ISC)
(authors "Thomas Gazagnaire <thomas@gazagnaire.org>")
(maintainers "Thomas Gazagnaire <thomas@gazagnaire.org>")

(source (github samoht/ocaml-eio-chaos))

(package
 (name eio-chaos)
 (synopsis "Chaos tests of Eio programs on simulated devices that fail together")
 (tags (eio test chaos))
 (description
  "eio-chaos runs an Eio program many times on simulated devices, each
time with a fault the real kernel can produce or a power cut, and restarts it
on what the fault left. The devices come from other packages, such as a
disk or a network; its README builds a toy one, to read as an
example. Eio_chaos.run builds the program's env from them, with simulated
clocks, seeded entropy and captured output, and reports the first failing run
with a line that replays it.")
 (depends
  (hermetic (>= 0.1.0))
  (ocaml (>= 5.4))
  (eio (>= 1.7))
  fmt
  (console (>= 0.9.0))
  (cstruct (>= 6.2.0))
  (mtime (>= 2.0.0))
  psq
  domain-local-await
  (mdx (and :with-test (>= 2.3.0)))
  (alcotest (and :with-test (>= 1.7.0)))
  (alcobar :with-test)))
