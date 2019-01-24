;;; Track and expand spread replacements.
(defpackage "SPREAD-TO-POLY"
  (:use "CL")
  (:export "WITH-TABLE" "EXPAND" "EXPAND-OR-CREATE")
  (:local-nicknames ("IN" "PATTERN-TYPE")
                    ("OUT" "POLY-TYPE")))

(in-package "SPREAD-TO-POLY")

;;; merge expansions
(defun merge-vars (&rest vars)
  "Returns a new out:var with the union of the flows in vars."
  (assert (every #'out:var-p vars))
  (let ((flows (ordered:set)))
    (map nil (lambda (var)
               (destructuring-bind (var-flows)
                   (out:split var)
                 (ordered:record-all flows var-flows)))
         vars)
    (out:var (ordered:entries flows))))

(defun merge-expansions (expansions fail-tag)
  "Returns a new spread expansion that merges the flows var-by-var"
  (check-type expansions cons)
  (when (null (rest expansions))
    (return-from merge-expansions (first expansions)))
  (unless (apply 'alexandria:length= expansions)
    (throw fail-tag "Mismatching expansions"))
  (apply 'mapcar #'merge-vars expansions))

;;; intern new patterns in the *spread-table*, or find existing ones.

;; hash table of flow var to list of poly-type:var
(defvar *spread-table*)
(declaim (type hash-table *spread-table*))

(defmacro with-table (() &body body)
  `(let ((*spread-table* (make-hash-table)))
     (locally ,@body)))

(defun ensure-spread (spread count fail-tag)
  "Returns an expansion for the spread variable's flows that matches
   the type count, or fails."
  (check-type spread in:spread)
  (check-type count (and unsigned-byte fixnum))
  (flet ((intern-expansion (flow)
           (multiple-value-bind (expansion foundp)
               (gethash flow *spread-table*)
             (cond (foundp
                    (unless (alexandria:length= count expansion)
                      (throw fail-tag "Incorrect spread expansion size."))
                    expansion)
                   (t
                    (setf (gethash flow *spread-table*)
                          (loop repeat count
                             collect (out:var
                                      (list (condition:fresh '#:s))))))))))
    (merge-expansions (destructuring-bind (flows)
                          (in:split spread)
                        (mapcar #'intern-expansion flows))
                      fail-tag)))

(defun expand-or-create (pattern-types num-mono fail-tag)
  "Expands any spread in pattern-types and makes sure the pattern's
   type count matches num-mono."
  (assert (every #'in:type-p pattern-types))
  (check-type num-mono (and unsigned-byte fixnum))
  (let* ((num-spread (count-if #'in:spread-p pattern-types))
         (num-args (- (length pattern-types) num-spread)))
    (when (> num-spread 1)
      (throw fail-tag "Multiple spread in the same type list."))
    (when (and (zerop num-spread)
               (/= num-args num-mono))
      (throw fail-tag "Incompatible type list length."))
    (when (> num-args num-mono)
      (throw fail-tag "Pattern type list too long."))
    (loop for pattern in pattern-types
       if (in:spread-p pattern)
       append (ensure-spread pattern (- num-mono num-args) fail-tag)
       else collect pattern)))

(defun expand (spread fail-tag)
  "Convert the spread variable to a list of poly variables."
  (check-type spread in:spread)
  (merge-expansions
   (destructuring-bind (flows)
       (in:split spread)
     (mapcar (lambda (flow)
               (multiple-value-bind (expansion foundp)
                   (gethash flow *spread-table*)
                 (unless foundp
                   (throw fail-tag "Failed to find expansion."))
                 expansion))
             flows))
   fail-tag))
