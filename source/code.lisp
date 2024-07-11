(cl:defpackage #:multiswank
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms)
  (:export #:define-swanks))

(cl:in-package #:multiswank)

(defun read-until (stream limits output)
  (loop :for char := (read-char stream nil nil)
        :do (let ((end (member char limits)))
              (unless (null end)
                (return-from read-until (first end)))
              (write-char char output))))

(defun read-host (stream)
  (let* ((port nil)
         (host (make-string-output-stream))
         (last-char (read-until stream '(#\newline #\:) host)))
    (setf host (get-output-stream-string host))
    (when (eql last-char #\:)
      (let ((port-stream (make-string-output-stream)))
        (read-until stream '(#\newline) port-stream)
        (setf port (parse-integer (get-output-stream-string port-stream)))))
    (if (null port)
        (list host)
        (list host port))))

(defun multiswank-reader-impl (stream)
  (let* ((*read-suppress* t)
         (host (read-host stream))
         (text
           (with-output-to-string (output)
             (read-preserving-whitespace
              (make-echo-stream stream output)
              t
              nil
              nil))))
    (print
     (list host text))))

(defvar *swanks* (make-hash-table :test 'equal))

(defmacro define-swanks (&body specs)
  `(progn
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (label host port) spec
                   `(setf (gethash ,(string-upcase label) *swanks*) (list ,host ,port))))
               specs)))

(defun multiswank-reader (stream char)
  (declare (ignore char))
  (destructuring-bind ((host &optional port) text) (multiswank-reader-impl stream)
    (if (null port)
        `(destructuring-bind (host port) (gethash ,(string-upcase host) *swanks*)
           (usocket:socket-connect host port)
           (swank-client:with-slime-connection (connection host port)
             (swank-client:slime-eval `(eval (read-from-string ,,text)) connection)))
        `(swank-client:with-slime-connection (connection ,host ,port)
           (swank-client:slime-eval `(eval (read-from-string ,,text)) connection)))))

(set-macro-character #\@ 'multiswank-reader)
