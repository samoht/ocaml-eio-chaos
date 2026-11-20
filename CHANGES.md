## unreleased

- `decide` no longer suspends the calling fiber when a run stops at one of its
  limits, as its documentation says. A device that held a `Mutex.t` across it
  left the mutex locked, and on musl the GC's free of the locked mutex
  corrupted the heap: eio-net's chaos tests crashed on Alpine.

## 0.1.0 (19/11/2026)

Initial release.
