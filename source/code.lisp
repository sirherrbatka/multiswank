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
    (list host text)))

(defvar *swanks* (make-hash-table :test 'equal))

(defmacro define-swanks (&body specs)
  `(progn
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (label host port) spec
                   `(setf (gethash ,(string-upcase label) *swanks*) (list ,host ,port))))
               specs)))

(defun multiswank-string-reader (stream char1 char2)
  (declare (ignore char))
  (destructuring-bind ((host &optional port) text) (multiswank-reader-impl stream)
    (if (null port)
        `(destructuring-bind (host port) (gethash ,(string-upcase host) *swanks*)
           (swank-client:with-slime-connection (connection host port)
             (swank-client:slime-eval `(eval (read-from-string ,,text)) connection)))
        `(swank-client:with-slime-connection (connection ,host ,port)
           (swank-client:slime-eval `(eval (read-from-string ,,text)) connection)))))

(defun multiswank-reader (stream char1 char2)
  (declare (ignore char1 char2))
  (destructuring-bind ((host &optional port) text) (multiswank-reader-impl stream)
    (if (null port)
        `(let ((spec (gethash ,(string-upcase host) *swanks*)))
           (when (null spec)
             (error "No ~a SWANK endpoint defined!" ,(string-upcase host)))
           (destructuring-bind (host port) spec
             (usocket:socket-connect host port)
             (swank-client:with-slime-connection (connection host port)
               (swank-client:slime-eval ',(read-from-string text) connection))))
        `(swank-client:with-slime-connection (connection ,host ,port)
           (swank-client:slime-eval ',(read-from-string text) connection)))))

(set-dispatch-macro-character #\# #\@ 'multiswank-reader)
(set-dispatch-macro-character #\# #\% 'multiswank-string-reader)
