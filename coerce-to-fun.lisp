(defpackage "COERCE-TO-FUN"
  (:use "CL")
  (:export "COERCE-TO-FUN"))

(in-package "COERCE-TO-FUN")
(defun coerce-to-fun (callable)
  (if (functionp callable)
      callable
      (fdefinition callable)))
