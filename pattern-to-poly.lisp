;;; Eliminate "spread" values in patterns.
;;;
;;; As an implementation accident, spread variables live in their own
;;; namespace.
(defpackage "PATTERN-TO-POLY"
  (:use "CL")
  (:local-nicknames ("M" "MONO-TYPE")
                    ("SPREAD" "SPREAD-TO-POLY")
                    ("IN" "PATTERN-TYPE")
                    ("OUT" "POLY-TYPE"))
  (:export "CONVERT"))

(in-package "PATTERN-TO-POLY")

;;; %convert pattern mono -> poly
(defgeneric %convert (in mono)
  (:documentation "Recursively converts a pattern type to a poly type
  that matches the mono type, if possible."))

(defun match-pattern-list (pattern-types mono-types)
  "Expands any spread variable in pattern types and recursively
   converts the pattern to a poly type."
  (assert (every #'in:type-p pattern-types))
  (assert (every #'m:type-p mono-types))
  (mapcar #'%convert
          (spread:expand-or-create pattern-types (length mono-types) 'fail)
          mono-types))

(defmethod %convert (in mono)
  (throw 'fail (format nil "Unhandled pattern ~A ~A" in mono)))

(defmethod %convert ((in in:function) (mono m:function))
  (destructuring-bind (in-args in-results)
      (in:split in)
    (destructuring-bind (mono-args mono-results)
        (m:split mono)
      (out:function (match-pattern-list in-args mono-args)
                    (match-pattern-list in-results mono-results)))))

(defmethod %convert ((in in:box) (mono m:box))
  (destructuring-bind (in-contents)
      (in:split in)
    (destructuring-bind (mono-contents)
        (m:split mono)
      (out:box (%convert in-contents mono-contents)))))

(defmethod %convert ((in in:base) (mono m:base))
  (destructuring-bind (flow sort condition)
      (in:split in)
    (unless (eql sort '*)
      (destructuring-bind (mono-sort mono-condition)
          (m:split mono)
        (declare (ignore mono-condition))
        (unless (eql sort mono-sort)
          (throw 'fail "Sort mismatch in base types."))))
    (out:base flow sort condition)))

(defmethod %convert ((in in:var) (mono m:type))
  (apply #'out:var (in:split in)))

;; values introduced by spread expansion are already out:vars
(defmethod %convert ((out out:var) (mono m:type))
  out)

;;; %to-poly pattern -> poly (for return types)
(defgeneric %to-poly (pattern))

(defmethod %to-poly ((pattern in:function))
  (destructuring-bind (arguments results)
      (in:split pattern)
    (out:function (expand-pattern-list arguments)
                  (expand-pattern-list results))))

(defmethod %to-poly ((pattern in:box))
  (destructuring-bind (contents)
      (in:split pattern)
    (out:box (%to-poly contents))))

(defmethod %to-poly ((pattern in:base))
  (apply #'out:base (in:split pattern)))

(defmethod %to-poly ((pattern in:var))
  (apply #'out:var (in:split pattern)))

(defun expand-pattern-list (pattern-list)
  "Expands up to one spreadable placeholder in a return type list. All
   the flows attached to the placeholder must have already been
   unified with something in the monotype arguments."
  (assert (every #'in:type-p pattern-list))
  (loop with found-spread = nil
     for result in pattern-list
     if (in:spread-p result)
     append (progn
              (when found-spread
                (throw 'fail "Multiple spreads in result"))
              (setf found-spread t)
              (spread:expand result 'fail))
     else collect (%to-poly result)))

;;; convert
(defun convert (in arguments)
  "Converts the function pattern type to a function polytype that
   matches the list of monotype arguments."
  (check-type in in:function)
  (assert (every #'m:type-p arguments))
  (spread:with-table ()
    (flet ((convert ()
             (destructuring-bind (pattern-args pattern-res)
                 (in:split in)
               (out:function (match-pattern-list pattern-args arguments)
                             (expand-pattern-list pattern-res)))))
      (format *error-output* "Failure in ~S: ~A~%"
              'convert
              (catch 'fail (return-from convert (convert)))))
    nil))
