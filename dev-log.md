# vm1 Dev Log

## 2025-05-22

- Implemented support for dynamic return values.
  - `frame.return-values` now holds a list of returned values.
  - `ret`, `get-ret`, and `get-ret-count` instructions added.
- Modified `run-program` to handle `:return` control directive.

### Next

- Remove `frame-stack-*` helpers. Just keep the push and pop.
- Tests of course.
