(in-package :vm1)

(defun capture-output (thunk)
  (split-sequence #\NewLine
                  (with-output-to-string (*standard-output*)
                    (funcall thunk))
                  :remove-empty-subseqs t))
