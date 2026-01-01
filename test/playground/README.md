# Publicatorg Playground

A synthetic test environment for benchmarking and testing publicatorg features.

## Structure

```
playground/
├── notes/           # 101 synthetic org notes
├── output/          # Build output (generated)
├── generate-notes.el   # Note generator
├── build-rules.el      # Minimal project config
├── benchmark.el        # Benchmarking utilities
└── README.md
```

## Note Distribution

The playground contains 101 notes with various dependency patterns:

- **40 independent notes** - no dependencies
- **30 chain notes** - 5 chains of 6 notes (A→B→C→D→E→F)
- **20 diamond notes** - 5 diamond patterns (A←B,C←D)
- **11 hub notes** - 1 central hub with 10 dependents

## Usage

### Generate Notes

Regenerate synthetic notes (clears existing):

```bash
eldev exec '(progn (load "test/playground/generate-notes.el") (playground-generate-notes))'
```

### Run Build

```bash
# With file cache (default)
eldev exec '(progn (load "test/playground/build-rules.el") (porg-run "playground"))'

# With SQLite cache
eldev exec '(progn
  (setq playground-cache-backend '\''sqlite)
  (load "test/playground/build-rules.el")
  (porg-run "playground"))'
```

### Run Benchmarks

```bash
# Full rebuild comparison
eldev exec '(progn
  (load "test/playground/benchmark.el")
  (playground-benchmark-full))'

# Incremental build comparison
eldev exec '(progn
  (load "test/playground/benchmark.el")
  (playground-benchmark-incremental))'

# Cache comparison across scenarios
eldev exec '(progn
  (load "test/playground/benchmark.el")
  (playground-benchmark-compare-caches))'
```

## Configuration

Set these variables before loading `build-rules.el`:

```elisp
;; Cache backend: 'file or 'sqlite
(setq playground-cache-backend 'sqlite)

;; Parallel workers (nil = sequential, N = N workers)
(setq playground-parallel 4)
```

## Expected Results

### File vs SQLite Cache

- **Cold build**: Similar performance (both create new cache)
- **Cached build (no changes)**: SQLite should be faster (no file parsing)
- **Incremental (1 note changed)**: SQLite should be faster (incremental read/write)

### Parallel Execution

Due to Emacs GIL (Global Interpreter Lock), parallel execution provides
minimal benefit for CPU-bound work. It only helps when threads are waiting
on I/O (external processes, network).

## Cleanup

```bash
rm -rf test/playground/output
```
