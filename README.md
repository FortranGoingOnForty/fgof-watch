# fgof-watch

[![CI](https://github.com/FortranGoingOnForty/fgof-watch/actions/workflows/ci.yml/badge.svg)](https://github.com/FortranGoingOnForty/fgof-watch/actions/workflows/ci.yml)

Portable file watching helpers for modern Fortran tools.

`fgof-watch` is intended to be a small, standalone library for directory and file watching in shells, editors, live-reload tools, sync utilities, and developer loops.

It is part of the [FortranGoingOnForty lib-modules](https://github.com/FortranGoingOnForty/lib-modules) catalog, but it is intended to stand on its own as a normal `fpm` package.

Current v1 target:

- high-level watch-session API
- portable polling backend as a dependable baseline
- normalized events for create, modify, remove, and move
- debounce and filtering hooks that app authors can actually use
- clean boundaries with `fgof-process` and future `fgof-devloop`

Future scope:

- native backends once the public API is stable
- richer event coalescing and ignore-rule helpers
- higher-level dev-loop orchestration in companion packages

## Status

Initial scaffold is in place.

Implemented today:

- public `fgof_watch` and `fgof_watch_types` modules
- polling-backed `watch_session`, `watch_options`, and `watch_event` types
- initialization, reset, and batch polling helpers
- normalized create, modify, remove, and move events
- recursive and nonrecursive polling behavior
- smoke-test and change-detection coverage with CI wiring

Still to implement:

- filtering and ignore rules
- debounce and event coalescing helpers
- native backend strategy

## Why Use It

- file watching is still one of the biggest remaining tooling gaps in Fortran
- app authors want a reusable library surface, not only standalone watcher tools
- it should compose naturally with `fgof-process`, future `fgof-devloop`, editors, and live-reload workflows

## Public API Shape

Primary modules:

- `fgof_watch`
- `fgof_watch_types`

Public types:

- `watch_event`
- `watch_entry`
- `watch_options`
- `watch_session`

Current public procedures:

- `init_watch`
- `poll_watch`
- `reset_watch`

Event constants:

- `FGOF_WATCH_EVT_NONE`
- `FGOF_WATCH_EVT_CREATED`
- `FGOF_WATCH_EVT_MODIFIED`
- `FGOF_WATCH_EVT_REMOVED`
- `FGOF_WATCH_EVT_MOVED`

## Quick Start

```fortran
program demo_watch
  use fgof_watch, only : init_watch, poll_watch
  use fgof_watch_types, only : watch_event, watch_session
  implicit none

  type(watch_event), allocatable :: events(:)
  type(watch_session) :: session

  call init_watch(session, "src")
  events = poll_watch(session)
  print "(I0)", size(events)
end program demo_watch
```

## Build And Test

```bash
fpm test
```

That is the baseline verification command locally and in CI.

## Supported Platforms

- macOS
- Linux

## Boundaries

- intended to stay independently versioned and releasable
- focused on reusable watch primitives, not a full dev-loop tool
- polling will be the first dependable backend; native backends can come later without changing the high-level surface
- the current polling backend reports event batches and suppresses directory-only metadata churn, so nested file activity is the signal that rises to the top

## License

MIT
