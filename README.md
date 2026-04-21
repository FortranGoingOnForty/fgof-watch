# fgof-watch

Portable file watching helpers for modern Fortran tools.

`fgof-watch` is intended to be a small, standalone library for directory and file watching in shells, editors, live-reload tools, sync utilities, and developer loops.

It is part of the [FortranGoingOnForty lib-modules](https://github.com/FortranGoingOnForty/lib-modules) catalog, but it is intended to stand on its own as a normal `fpm` package.

Current v1 target:

- high-level watch-session API
- portable polling backend as a dependable baseline
- normalized events for create, modify, remove, and move
- room for future native backends without breaking callers
- clean composition with `fgof-process` and future `fgof-devloop`

## Status

Initial scaffold is in place.

Implemented today:

- public `fgof_watch` and `fgof_watch_types` modules
- baseline watch-session, watch-options, and watch-event types
- minimal initialization, reset, and polling helpers
- smoke-test coverage

Still to implement:

- real directory snapshots and change detection
- recursive traversal and filtering
- debounce, coalescing, and native backend strategy

## Build And Test

```bash
fpm test
```

## Supported Platforms

- macOS
- Linux

## License

MIT
