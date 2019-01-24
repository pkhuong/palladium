;;; Eliminate "spread" values in return patterns that must match skeleton types.
;;;
;;; Returns spreads are independent of polymorphic pattern for the
;;; calls. Again, as an implementation accident, spread variables live
;;; in their own namespace.
;;;
;;; There's a lot of overlap with PATTERN-TO-POLY, but I don't think
;;; it's worth complexifying the type hierarchy just to avoid a big of
;;; copy paste.
(defpackage "RETURN-PATTERN-TO-POLY"
  (:use "CL")
  (:local-nicknames ("S" "SKELETON-TYPE")
                    ("SPREAD" "SPREAD-TO-POLY")
                    ("IN" "PATTERN-TYPE")
                    ("OUT" "POLY-TYPE"))
  (:export "CONVERT"))

(in-package "RETURN-PATTERN-TO-POLY")

;;; %convert pattern skeleton -> poly
(defgeneric %convert (in skel)
  (:documentation "Recursively converts a pattern type to a poly type
  that matches the skeleton, if possible."))

(defun match-pattern-list (pattern-types skel-types)
  "Expands any spread variable in patttern types and recursively
   converts the pattern to a poly type."
  (assert (every #'in:type-p pattern-types))
  (assert (every #'s:type-p skel-types))
  (mapcar #'%convert
          (spread:expand-or-create pattern-types (length skel-types) 'fail)
          skel-types))

(defmethod %convert (in skel)
  (throw 'fail (format nil "Unhandled pattern ~A ~A" in skel)))

(defmethod %convert ((in in:function) (skel s:function))
  (destructuring-bind (in-args in-results)
      (in:split in)
    (destructuring-bind (skel-args skel-results)
        (s:split skel)
      (out:function (match-pattern-list in-args skel-args)
                    (match-pattern-list in-results skel-results)))))

(defmethod %convert ((in in:box) (skel s:box))
  (destructuring-bind (in-contents)
      (in:split in)
    (destructuring-bind (skel-contents)
        (s:split skel)
      (out:box (%convert in-contents skel-contents)))))

(defmethod %convert ((in in:base) (skel s:base))
  (apply #'out:base (in:split in)))

(defmethod %convert ((in in:var) (skel s:type))
  (apply #'out:var (in:split in)))

;; values introduced by spread expansion are already out:vars
(defmethod %convert ((out out:var) (skel s:type))
  out)

(defun convert (in results)
  (assert (every #'in:type-p in))
  (assert (every #'s:type-p results))
  (spread:with-table ()
    (format *error-output* "Failure in ~S: ~A~%"
            'convert
            (catch 'fail
              (return-from convert (match-pattern-list in results))))
    nil))
