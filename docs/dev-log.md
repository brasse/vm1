# vm1 Dev Log

## 2025-05-22

- Implemented support for dynamic return values.
  - `frame.return-values` now holds a list of returned values.
  - `ret`, `get-ret`, and `get-ret-count` instructions added.
- Modified `run-program` to handle `:return` control directive.

### Next

- Remove `frame-stack-*` helpers. Just keep the push and pop.
- Add assembler so that we get labels to use for jumps.
- Tests of course.

## 2025-05-23

- Removed most frame-stack-* helpers
- Implemted tiny assebler to get labels

### Next

- Tests of course, always the tests.

## 2025-05-26

- Added some example programs
- Added `tail-call` instruction
  - `tail-call` reuses the current frame instead of pushing
    a new one.

### Next

- Tests of course, always the tests.
