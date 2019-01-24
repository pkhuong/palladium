(defpackage "OCCUR-CHECK"
  (:use "CL")
  (:export "WITH-OCCUR-CHECK-ENVIRONMENT" "WITH-OCCUR-CHECK"))

(in-package "OCCUR-CHECK")

(defvar *occur-check*)
(declaim (type hash-table *occur-check*))

(defun call-with-occur-check-environment (test thunk)
  (let ((*occur-check* (make-hash-table :test test)))
    (funcall thunk)))

(defmacro with-occur-check-environment ((test) &body body)
  `(block nil
     (call-with-occur-check-environment ,test (lambda () ,@body))))

(defun call-with-occur-check (key on-failure on-success)
  (multiple-value-bind (old-value old-foundp)
      (gethash key *occur-check*)
    (unwind-protect
         (cond (old-foundp
                (funcall on-failure))
               (t
                (incf (gethash key *occur-check* 0))
                (funcall on-success)))
      (if old-foundp
          (setf (gethash key *occur-check*) old-value)
          (remhash key *occur-check*)))))

(defmacro with-occur-check (((key) &body on-failure) &body on-success)
  `(block nil
     (call-with-occur-check ,key
                            (lambda () ,@on-failure)
                            (lambda () ,@on-success))))
