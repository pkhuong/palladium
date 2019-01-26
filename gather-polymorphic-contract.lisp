;;; The polymorphic function type encodes pre- and post- conditions in
;;; the refinements.
;;;
;;; In positive polarity, we have post-conditions. If the polymorphic
;;; base type is set in the guarantee, the function is free to come up
;;; with, e.g., an arbitrary integer however it wants; that return
;;; type is fixed to exactly its monotype in the polymorphic type.
;;;
;;; A polymorphic function may also add guarantees to a polymorphic
;;; return type. In that case, the function is unable to manifest
;;; arbitrary values that inhabit this polymorphic type (unknown
;;; sort), but may be able to impose additional constraints on the
;;; return value (e.g., through partiality, or constraints on other
;;; arguments). This condition should be ANDed with the description of
;;; the set of values that might flow there.
;;;
;;; In negative polarity, we have pre-conditions. They are ANDed with
;;; the conditions that were already gathered for all negative bases.
(defpackage "GATHER-POLYMORPHIC-CONTRACT"
  (:export "CONTRACT" "SOLUTION-OR-NIL" "CONSTRAINT")
  (:use "CL")
  (:local-nicknames ("C" "CONDITION")
                    ("S" "SKELETON-TYPE")
                    ("P" "POLY-TYPE")))

(in-package "GATHER-POLYMORPHIC-CONTRACT")

;;; Public interface.
(defstruct contract
  ;; for positive s:base only, map from base name to globalised
  ;; mandatory return type, if any. the polymorphic function knows how
  ;; to generate that kind of values; if there are dataflow
  ;; annotations, it could do even more.
  (solutions (ordered:map) :type ordered:map :read-only t)
  ;; for any s:base, map from base name to globalised constraint that
  ;; should be ANDed in.
  (constraints (ordered:map) :type ordered:map :read-only t))

;; It only makes sense to ask for a solution for values that flow out.
(defun solution-or-nil (contract base)
  (declare (type contract contract)
           (type s:base base))
  (destructuring-bind (name polarity flow position)
      (s:split base)
    (declare (ignore flow position))
    (assert (eql polarity '+))
    (values (ordered:find (contract-solutions contract) name))))

(defun constraint (contract base)
  (declare (type contract contract)
           (type s:base base))
  (destructuring-bind (name polarity flow position)
      (s:split base)
    (declare (ignore polarity flow position))
    (or (ordered:find (contract-constraints contract) name)
        'c:true)))

(defvar *contract*)
(declaim (type contract *contract*))

(defgeneric gather (skel poly)
  (:documentation "Walks the (compatible) skeleton and polymorphic
  types to setup the scoping environment and store globalised
  constraints or solutions from the polymorphic type's conditions on
  its base types."))

(defun bind-skel-base (types)
  (map 'simple-vector (lambda (type)
                        (and (s:base-p type) type))
       types))

(defmethod gather ((skel s:function) (poly p:function))
  (destructuring-bind (skel-args skel-results)
      (s:split skel)
    (destructuring-bind (poly-args poly-results)
        (p:split poly)
      (scoping:with-function-scope ((bind-skel-base skel-args))
        (assert (alexandria:length= skel-args poly-args))
        (map nil #'gather skel-args poly-args)
        (scoping:with-function-return-values ((bind-skel-base skel-results))
          (assert (alexandria:length= skel-results poly-results))
          (map nil #'gather skel-results poly-results))))))

(defmethod gather ((skel s:box) (poly p:box))
  (destructuring-bind (skel)
      (s:split skel)
    (destructuring-bind (poly)
        (p:split poly)
      (gather skel poly))))

(defmethod gather ((skel s:base) (poly p:base))
  (destructuring-bind (flow sort condition)
      (p:split poly)
    (declare (ignore flow))
    (unless condition
      (if (not (eql sort '*))
          ;; if the sort is set, default the missing condition to
          ;; true, but the frontend should really handle this case.
          (setf condition 'c:true)
          ;; otherwise, nothing to do.
          (return-from gather)))
    (destructuring-bind (name polarity flow position)
        (s:split skel)
      (declare (ignore flow position))
      (let ((globalised-condition (scoping:to-global condition 'fail))
            (destination (if (and (eql polarity '+)
                                  (not (eql sort '*)))
                             ;; out value with a known sort. that's
                             ;; our full solution.
                             (contract-solutions *contract*)
                             ;; sort is unknown, that's a promise
                             ;; (+ve) or requirement (-ve).
                             (contract-constraints *contract*))))
        (ordered:record destination
                        (cons name globalised-condition))))))

(defmethod gather ((skel s:type) (poly p:var))
  ;; nothing to do.
  )

(defun contract (skel-function poly-function)
  (check-type skel-function s:function)
  (check-type poly-function p:function)
  (flet ((contract ()
           (let ((*contract* (make-contract)))
             (gather skel-function poly-function)
             *contract*)))
    (scoping:with-environment ()
      (format *error-output*
              "Failed to gather contract in ~S: ~A."
              'contract
              (catch 'fail (return-from contract (contract)))))
    nil))
