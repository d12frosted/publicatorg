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

The playground generates notes based on a configurable scale factor:

| Scale | Total Notes | Independent | Chains      | Diamonds    | Hubs        |
|-------|-------------|-------------|-------------|-------------|-------------|
| 1     | ~100        | 40          | 5×6 = 30    | 5×4 = 20    | 1×11 = 11   |
| 10    | ~1000       | 400         | 50×6 = 300  | 50×4 = 200  | 10×11 = 110 |
| 100   | ~10000      | 4000        | 500×6 = 3000| 500×4 = 2000| 100×11=1100 |

Dependency patterns:
- **Independent** - no dependencies
- **Chains** - A→B→C→D→E→F (6 notes per chain)
- **Diamonds** - A←B,C←D (4 notes per diamond)
- **Hubs** - 1 central hub with 10 spokes

## Usage

### Generate Notes

Regenerate synthetic notes (clears existing):

```bash
# Default: 1000 notes (scale=10)
eldev exec '(progn (load "test/playground/generate-notes.el") (playground-generate-notes))'

# Custom scale: 100 notes
eldev exec '(progn (load "test/playground/generate-notes.el") (playground-generate-notes 1))'

# Large scale: 10000 notes
eldev exec '(progn (load "test/playground/generate-notes.el") (playground-generate-notes 100))'
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

For note generation, set `playground-scale` before calling `playground-generate-notes`:

```elisp
;; Or pass scale directly to the function
(playground-generate-notes 10)  ; 1000 notes
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
