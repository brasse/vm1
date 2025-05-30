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

## 2025-05-28

- Add not instruction
- Add bool type
  - Make all comparison operators return bool

### Next

- Need to move all vm-value related functions to vm-value.lisp.
  vm.lisp will just use those functions when defining instructions.
- Decide on how polymorphic things should be. I'm leaning towards
  not too polymorphic.
  - concat - yes, concats strings
  - substr - yes
  - Maybe this is it. Don't know yet.
- Tests of course, always the tests.

## 2025-05-30

- Decided to stop doing the translation from VM error condigion to a
  trap directive. Might introduce this later again if needed.
- Added string interning. There's now a string table containing all
  strings created. Strings are immutable in vm1 so we reuse common
  strings and can use eq from string comparison. Very cool!
- Finally some tests.

### Next

- The resolve-value function now needs a string-table to be able to
  create string literals. Fix this by defining a closure over
  string-table that has the same signature as resolve-value today.
