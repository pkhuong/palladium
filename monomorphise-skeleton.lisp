;;; Synthesise localised (with de bruijn indices) conditions for every
;;; s:base in the skeleton type.
;;;
;;; We already have known conditions for values flowing in the
;;; polymorphic function; we only have to AND them with any constraint
;;; from the function's preconditions, and save the localised version.
;;;
;;; We have some known conditions for values flowing out of the
;;; polymorphic function. We can use them directly.
;;;
;;; Otherwise, we must find the union of everything that could flow
;;; there, and AND with any post-condition on that outflowing value.
;;;
;;; We do the latter with dataflow information from
;;; `gather-skeleton-flow-info`. Given a positive s:base, we use
;;; `base-sources` to find all the (negative) bases that may flow
;;; there, and take the union of the set of values that may be bound
;;; to these s:base.
(defpackage "MONOMORPHISE-SKELETON"
  (:export "MONOMORPHISE")
  (:use "CL")
  (:local-nicknames ("C" "CONDITION")
                    ("S" "SKELETON-TYPE")
                    ("OUT" "MONO-TYPE")
                    ("FLOW-INFO" "GATHER-SKELETON-FLOW-INFO")
                    ("ARGUMENTS" "GATHER-ARGUMENT-CONDITIONS")
                    ("CONTRACT" "GATHER-POLYMORPHIC-CONTRACT")))

(in-package "MONOMORPHISE-SKELETON")

(defun polarity-base-p (skel expected-polarity)
  (check-type skel s:type)
  (and (s:base-p skel)
       (destructuring-bind (name actual-polarity flow position)
           (s:split skel)
         (declare (ignore name flow position))
         (eql actual-polarity expected-polarity))))

(defun positive-base-p (skel)
  (polarity-base-p skel '+))

(defun negative-base-p (skel)
  (polarity-base-p skel '-))

(defun binding-vector (skels)
  (map 'simple-vector (lambda (skel)
                        (and (s:base-p skel) skel))
       skels))

;;; Construct a map from base name -> global condition for values
;;; flowing in.
(defun collect-argument-global-conditions (flow conditions contract)
  (declare (type flow-info:info flow)
           (type arguments:conditions conditions)
           (type contract:contract contract))
  (let ((global-conditions (ordered:map)))  ;; base name -> joined condition
    (dolist (base (flow-info:all-bases flow) global-conditions)
      ;; only do this for values flowing in.
      (destructuring-bind (name polarity flow position)
          (s:split base)
        (declare (ignore flow position))
        (when (eql polarity '-)
          (let ((actual-condition (arguments:base-condition conditions base))
                (precondition (contract:constraint contract base)))
            (assert actual-condition)
            (assert precondition)
            (ordered:record global-conditions
                            (cons name
                                  (c:and-conditions (list actual-condition
                                                          precondition))))))))))

;;; Generate localised conditions for s:base. The global conditions
;;; for -ve values (arguments, values flowing in) are in
;;; *argument-global-conditions*.
;;;
;;; For positive values, we sometimes have a solution from the
;;; contract. In that case, we take it.
;;;
;;; Otherwise, we intersect the binding's constraint from the
;;; polymorphic contract with everything that could flow there.
;;;
;;; When we approximate everything that could flow somewhere, we must
;;; be care to only directly refer to bindings in scope with negative
;;; polarity: those are the bindings that are write-once in the
;;; context of the return value.
;;;
;;; However, we are allowed (and want to) refer to everything when
;;; localising conditions where they were defined. (XXX double check)
(defvar *flow-info*)
(declaim (type flow-info:info *flow-info*))

(defvar *argument-global-conditions*)
;;; base name -> globalised condition.
(declaim (type ordered:map *argument-global-conditions*))

(defun argument-global-condition (base)
  (declare (type s:base base))
  (destructuring-bind (name polarity flow position)
      (s:split base)
    (declare (ignore flow position))
    (assert (eql polarity '-))
    (let ((condition (ordered:find *argument-global-conditions* name)))
      (assert condition)
      condition)))

(defvar *contract*)
(declaim (type contract:contract *contract*))

;;; if *assume-causality* is true, only consider as sources of values
;;; skeleton:base that are causally available. This flag should only
;;; be set to true when generating a condition (for a positive base)
;;; in return position: that means we're generating a return value for
;;; the polymorphic function or one of the functions it returned.
(defvar *assume-causality*)
(declaim (type boolean *assume-causality*))

;;; if *assume-purity*, we're generating the type for a pure function.
;;; otherwise, assume that any function returned by the initial call
;;; may mutate shared state, and thus communicate across time and
;;; function bodies.
;;;
;;; In the impure/conservative case, we can only directly refer to
;;; toplevel bindings.
(defvar *assume-purity*)
(declaim (type boolean *assume-purity*))

;;; The logic here is very similar to generate-condition-for-base in
;;; `backfill-argument-conditions`: we're replacing bindings that aren't
;;; in scope, or not safe to directly refer to, by existentials that
;;; approximate the values that could go there.

;;; Rewrite a condition to replace arguments in scope with existentials.
(defun approximate-local-condition (condition)
  (assert condition)
  (let ((to-bind (ordered:map :test #'equalp)))
    (flet ((rewrite (expression reference)
             (cond ((and (typep reference '(cons (eql c:@-)))
                         (negative-base-p expression))
                    ;; Only safe to refer to negative polarity
                    ;; arguments. negative polarity return values
                    ;; could come from functions invoked repeatedly.
                    reference)
                   (t
                    ;; make up a new binding that will be made to look like
                    ;; something bound to this s:base.
                    (or (ordered:find to-bind expression)
                        (let ((fresh (c:fresh '#:a)))
                          (ordered:record to-bind (cons expression fresh))
                          fresh))))))
      (wrap-existentials to-bind
                         (scoping:to-local condition
                                           #'s:base-p
                                           #'rewrite
                                           :test #'equalp)))))

(defun wrap-existentials (to-bind condition)
  (check-type to-bind ordered:map)
  (let ((bindings '())
        (conditions (list condition)))
    (loop for (base . gensym) in (ordered:entries to-bind) do
       ;; nothing prevents a negative value from directly
       ;; referring to another negative value; perform type
       ;; dispatch in approximate-local-condition-for-base.
         (let ((condition (approximate-local-condition-for-base base)))
           (push `(,gensym ,(flow-info:base-sort *flow-info* base)) bindings)
           (push (if (c:mentions-v condition)
                     `(let ((c:v ,gensym))
                        ,condition)
                     condition)
                 conditions)))
    (if bindings
        `(c:exists ,(nreverse bindings)
                   ,(c:and-conditions (nreverse conditions)))
        condition)))

(defun approximate-local-condition-for-argument (base)
  (declare (type s:base base))
  (assert (negative-base-p base))
  (let ((location (scoping:reference base :test #'equalp
                                     ;; only safe to use non-toplevel bindings
                                     ;; in pure functions.
                                     :toplevel-only (not *assume-purity*))))
    ;; it's always safe to directly refer to negative arguments in scope.
    (when (typep location '(cons (eql c:@-)))
      (return-from approximate-local-condition-for-argument
        `(= c:v ,location))))
  (when (and *assume-causality*
             (not (causality:available-p base)))
    ;; this base type can't flow into the return type we're currently
    ;; generating.
    (return-from approximate-local-condition-for-argument 'c:false))
  (occur-check:with-occur-check ((base) (return 'c:true))
    ;; (over)approximate the set of values that may flow into the
    ;; polymorphic function.
    (approximate-local-condition (argument-global-condition base))))

(defun approximate-local-condition-for-base (base)
  (declare (type s:base base))
  (cond ((negative-base-p base)
         (approximate-local-condition-for-argument base))
        (t
         (assert (positive-base-p base))
         ;; the logic here implements the same thing as
         ;; generate-condition-for-result: if the polymorphic base
         ;; specified a sort, the condition describes values that it
         ;; may create de novo, otherwise it's an additional
         ;; constraint. At most one of solution or constraint is
         ;; populated for any base, but it's simpler to code as if
         ;; both could be present, with appropriate defaults.
         (let ((solution (approximate-local-condition
                          ;; only populated if the sort is known.
                          (or (contract:solution-or-nil *contract* base)
                              'c:false)))
               (constraint (approximate-local-condition
                            (contract:constraint *contract* base)))
               (flow-in (approximate-local-condition-for-result base)))
           (c:or-conditions (list solution
                                  (c:and-conditions (list constraint
                                                          flow-in))))))))

(defun approximate-local-condition-for-result (base)
  (declare (type s:base base))
  (assert (positive-base-p base))
  (occur-check:with-occur-check ((base) (return 'c:true))
    (c:or-conditions (mapcar #'approximate-local-condition-for-argument
                             (flow-info:base-sources *flow-info* base)))))

;;; The previous section was concerned with funny dataflow
;;; business. Here, we just take conditions where they were defined
;;; and re-localise them.
(defun relocalise-condition-in-place (condition)
  (assert condition)
  (scoping:to-local condition
                    #'s:base-p
                    (lambda (expression reference)
                      (assert reference (expression reference))
                      reference)
                    :test #'equalp))

(defun check-all-local (condition)
  "Errors out if the condition has any global reference left. Returns the condition as-is."
  (scoping:to-local condition
                    #'s:base-p
                    (lambda (expression reference)
                      (error "Unexpected global reference remaining ~S ~S."
                             expression reference)
                      expression)
                    :test #'equalp)
  condition)

;;; Argument, or value flowing in the callee.
(defun generate-condition-for-argument (base)
  (assert (negative-base-p base))
  (relocalise-condition-in-place (argument-global-condition base)))

;;; Result, or value flowing out of the callee.
(defun generate-condition-for-result (base)
  (destructuring-bind (name polarity flow position)
      (s:split base)
    (declare (ignore name flow))
    (assert (eql polarity '+))
    ;; if we have a fully specified solution, we know hat the
    ;; polymorphic function knows how to generate these values from
    ;; scratch and push them there. the spec also tells us what
    ;; polymorphic types might also flow there; OR both in.
    (let ((solution (or (contract:solution-or-nil *contract* base)
                        'c:false))
          (dataflow-type
           (occur-check:with-occur-check-environment (#'equalp)
             ;; we know we're describing values flowing out of the
             ;; polymorphic function. if these values flow out as a result,
             ;; and the function is pure, what's currently in scope describes
             ;; everything available to compute them. otherwise, the best we
             ;; can do is obey dataflow annotations.
             ;;
             ;; of course, that doesn't hold when computing a direct return
             ;; value for the toplevel function. In that case, we can always
             ;; assume causality, since any side effect happens after the
             ;; value has been generated.
             ;;
             ;; n.b., it's always safe to not assume causality: it only means
             ;; we are more conservative when determining all the ways we
             ;; could generate values of a given type.
             (let ((*assume-causality* (and (eql position :res)
                                            (or *assume-purity*
                                                (not
                                                 (null
                                                  (scoping:reference base
                                                                     :test #'equalp
                                                                     :toplevel-only t)))))))
               (check-all-local
                (c:and-conditions (list (relocalise-condition-in-place
                                         (contract:constraint *contract* base))
                                        (approximate-local-condition-for-result base))))))))
      (c:or-conditions (list (relocalise-condition-in-place solution)
                             dataflow-type)))))

(defun generate-condition (base)
  (if (positive-base-p base)
      (generate-condition-for-result base)
      (generate-condition-for-argument base)))

(defgeneric %monomorphise (skel)
  (:documentation "Monomorphise a skeleton by generating local conditions for every s:base."))

(defmethod %monomorphise ((skel s:function))
  (destructuring-bind (skel-args skel-results)
      (s:split skel)
    (scoping:with-function-scope ((binding-vector skel-args))
      (causality:with-causes (skel-args)
        (out:function (mapcar #'%monomorphise skel-args)
                      (scoping:with-function-return-values ((binding-vector skel-results))
                        (mapcar #'%monomorphise skel-results)))))))

(defmethod %monomorphise ((skel s:box))
  (destructuring-bind (skel)
      (s:split skel)
    (out:box (%monomorphise skel))))

(defmethod %monomorphise ((skel s:base))
  (let ((sort (flow-info:base-sort *flow-info* skel))
        (condition (generate-condition skel)))
    (assert sort)
    (assert condition)
    (out:base sort condition)))

(defun monomorphise (skeleton *flow-info* conditions *contract*
                     &key (assume-purity t))
  (declare (type s:function skeleton)
           (type flow-info:info *flow-info*)
           (type arguments:conditions conditions)
           (type contract:contract *contract*))
  (let ((*argument-global-conditions*
         (collect-argument-global-conditions *flow-info* conditions *contract*))
        (*assume-purity* (not (not assume-purity))))
    (scoping:with-environment ()
      (causality:with-causality-tracking ()
        (%monomorphise skeleton)))))
