#!/bin/bash

pre () {
  file=$1
  echo $file
  echo '<pre>'
  cat $file
  echo '</pre>'
  echo
}

cat <(pre package.lisp) \
    <(pre vm-error.lisp) \
    <(pre vm-value.lisp) \
    <(pre frame.lisp) \
    <(pre vm.lisp) \
    <(pre assembler.lisp) \
    <(pre compiler.lisp) \
    <(pre examples/examples-asm.lisp) \
    <(pre tests/frame-test.lisp) \
    <(pre tests/instruction-test.lisp) \
    <(pre tests/vm-value-test.lisp) \
    | xsel --clipboard
