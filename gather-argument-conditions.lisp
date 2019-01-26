;;; The monotypes tell us exactly what the values flowing into the
;;; polymorphic function look like. We may also need help from the
;;; polymorphic return type, in order to pin down arguments to
;;; functions returned from the polymorphic call.
;;;
;;; In some cases, the latter polymorphic return type might not
;;; contain any constraint. In theory, this is a way for the caller to
;;; tell us they don't intend to use the function. In practice, it's
;;; friendlier to heuristically hallucinate a condition that should
;;; usually be tight enough to later type check.
;;;
;;; However, before doing that, we'll look for fully defined base
;;; types in the polymorphic function type.
(defpackage "GATHER-ARGUMENT-CONDITIONS"
  (:use "CL")
  (:export "CONDITIONS" "BASE-CONDITION" "ADJOIN-CONDITION")
  (:local-nicknames ("S" "SKELETON-TYPE")
                    ("M" "MONO-TYPE")
                    ("P" "POLY-TYPE")))

(in-package "GATHER-ARGUMENT-CONDITIONS")

(defstruct conditions
  ;; negative skeleton base name -> globalised constraint
  (arguments (ordered:map) :type ordered:map :read-only t)
  ;; positive skeleton base name -> globalised constraint
  ;; this is only populated from the argument monotypes, and used
  ;; heuristically to generate missing negative constraints.
  (sinks (ordered:map) :type ordered:map :read-only t))

(defun base-condition (conditions base)
  (declare (type conditions conditions)
           (type s:base base))
  (destructuring-bind (name polarity flow position)
      (s:split base)
    (declare (ignore flow position))
    (values (ordered:find (ecase polarity
                            (- (conditions-arguments conditions))
                            (+ (conditions-sinks conditions)))
                          name))))

(defun adjoin-condition (conditions base condition)
  (declare (type conditions conditions)
           (type s:base base)
           (type (or cons (and symbol (not (member nil *))))))
  (destructuring-bind (name polarity flow position)
      (s:split base)
    (declare (ignore flow position))
    (let ((destination (ecase polarity
                         (- (conditions-arguments conditions))
                         (+ (conditions-sinks conditions)))))
      (assert (not (ordered:find destination name)))
      (ordered:record destination (cons name condition))
      condition)))

;;; Gather conditions for monotypes passed in as arguments
(defvar *conditions*)
(declaim (type conditions *conditions*))

(defun bind-if-base-p (types)
  (map 'simple-vector (lambda (type)
                        (and (s:base-p type) type))
       types))

(defgeneric %gather-mono-conditions (skel mono)
  (:documentation "Saves conditions for all s:base that show up in
  arguments to the polymorphic function."))

(defmethod %gather-mono-conditions ((skel s:function) (mono m:function))
  (destructuring-bind (skel-args skel-results)
      (s:split skel)
    (destructuring-bind (mono-args mono-results)
        (m:split mono)
      (scoping:with-function-scope ((bind-if-base-p skel-args))
        (assert (alexandria:length= skel-args mono-args))
        (map nil #'%gather-mono-conditions skel-args mono-args)
        (scoping:with-function-return-values ((bind-if-base-p skel-results))
          (assert (alexandria:length= skel-results mono-results))
          (map nil #'%gather-mono-conditions skel-results mono-results))))))

(defmethod %gather-mono-conditions ((skel s:box) (mono m:box))
  (destructuring-bind (skel)
      (s:split skel)
    (destructuring-bind (mono)
        (m:split mono)
      (%gather-mono-conditions skel mono))))

(defmethod %gather-mono-conditions ((skel s:base) (mono m:base))
  (destructuring-bind (name polarity flow position)
      (s:split skel)
    (declare (ignore flow position))
    (destructuring-bind (sort condition)
        (m:split mono)
      (declare (ignore sort))
      (let ((destination (ecase polarity
                           (- (conditions-arguments *conditions*))
                           (+ (conditions-sinks *conditions*)))))
        (ordered:record destination
                        (cons name
                              (scoping:to-global condition 'fail)))))))

;;; Gather conditions for polytypes expected as results.
(defgeneric %gather-poly-conditions (skel poly)
  (:documentation "Saves conditions for all s:base in argument
  positions (negative polarity) that show up in the polymorphic
  results. We don't store positive base types (direct return values)
  because they probably don't impact the expected argument type in
  returned functions.

  The only way the return type in a returned function impacts the
  argument type in a function is if it's an argument type in that very
  same function. The programmer might as well annotate the returned
  function's argument type."))

;;; When the polymorphic types come from the polymorphic function type
;;; we're invoking, we heuristically use information from fully
;;; specified base types when necessary. I still think the programmer
;;; should specify those in the expected result types, but it seems
;;; preferable to use these than to try to elaborate a complicated
;;; type from dataflow information. It also means we can more easily
;;; drop dataflow information for fully specified base types.
;;;
;;; In negative polarity, we can pretend fully specified base types in
;;; the polymorphic function type go to a unique flow variable that
;;; isn't read: we don't need to elaborate types when they're already
;;; fully specified.
;;;
;;; In positive polarity, we always directly paste in the polymorphic
;;; function type's fully specified base types, so we don't need
;;; dataflow information. They can come from a unique flow variable
;;; that isn't written.
(defvar *only-store-fully-typed-conditions*)
(declaim (type boolean *only-store-fully-typed-conditions*))

(defvar *flip-toplevel*)
(declaim (type boolean *flip-toplevel*))

(defmethod %gather-poly-conditions ((skel s:function) (poly p:function))
  (destructuring-bind (skel-args skel-results)
      (s:split skel)
    (destructuring-bind (poly-args poly-results)
        (p:split poly)
      (scoping:with-function-scope ((map 'simple-vector
                                         (lambda (arg)
                                           (and (s:base-p arg) arg))
                                         skel-args))
        (assert (alexandria:length= skel-args poly-args))
        (map nil #'%gather-poly-conditions skel-args poly-args)
        (scoping:with-function-return-values
            ((map 'simple-vector
                  (lambda (result)
                    (and (s:base-p result) result))
                  skel-results))
          (assert (alexandria:length= skel-results poly-results))
          (map nil #'%gather-poly-conditions skel-results poly-results))))))

(defmethod %gather-poly-conditions ((skel s:box) (poly p:box))
  (destructuring-bind (skel)
      (s:split skel)
    (destructuring-bind (poly)
        (p:split poly)
      (%gather-poly-conditions skel poly))))

(defmethod %gather-poly-conditions ((skel s:base) (poly p:base))
  (destructuring-bind (flow sort condition)
      (p:split poly)
    (declare (ignore flow))
    (destructuring-bind (name polarity flow position)
        (s:split skel)
      (declare (ignore flow position))
      ;; we only store negative (argument) polarity,
      ;; and we obey *only-store-fully-typed-conditions*
      (when (or (eql polarity '+)
                (null condition)
                (and *only-store-fully-typed-conditions*
                     (eql sort '*)))
        (return-from %gather-poly-conditions))
      (assert (eql polarity '-))
      (let ((destination (conditions-arguments *conditions*)))
        (unless (ordered:find destination name)
          (ordered:record
           destination
           (cons name
                 (scoping:to-global condition 'fail
                                    :flip-toplevel *flip-toplevel*))))))))

(defmethod %gather-poly-conditions ((skel s:type) (poly p:var))
  ;; nothing to gather here.
  )

(defun conditions (poly skeleton arguments results)
  (check-type poly p:function)
  (check-type skeleton s:function)
  (assert (every #'m:type-p arguments))
  (assert (every #'p:type-p results))
  (flet ((conditions ()
           (let ((*conditions* (make-conditions)))
             (scoping:with-environment ()
               (destructuring-bind (skel-args skel-results)
                   (s:split skeleton)
                 ;; arguments are monotypes and should not refer to
                 ;; each other relationally. Make sure they can't by
                 ;; giving them an empty scope.
                 (scoping:with-function-scope (#())
                   (assert (alexandria:length= skel-args arguments))
                   ;; save information about values in argument position
                   (map nil #'%gather-mono-conditions skel-args arguments))
                 ;; do bind arguments for results.
                 (scoping:with-function-scope ((bind-if-base-p skel-args))
                   (scoping:with-function-return-values
                       ((bind-if-base-p skel-results))
                     (assert (alexandria:length= skel-results results))
                     ;; save information about return (function)
                     ;; values. We assume the poly result type is an
                     ;; argument list for a function that will receive
                     ;; the return values, so flip toplevel args.
                     (let ((*only-store-fully-typed-conditions* nil)
                           (*flip-toplevel* t))
                       (map nil #'%gather-poly-conditions skel-results results))

                     ;; try to steal from the polymorphic function type.
                     (let ((*only-store-fully-typed-conditions* t)
                           (*flip-toplevel* nil))
                       (multiple-value-bind (poly-args poly-results)
                           (p:split poly)
                         (declare (ignore poly-args))
                         (map nil #'%gather-poly-conditions
                              skel-results poly-results)))))))
             *conditions*)))
    (format *error-output*
            "Failed to gather conditions for ~A ~A ~A: ~A"
            skeleton arguments results
            (catch 'fail (return-from conditions (conditions))))
    nil))
