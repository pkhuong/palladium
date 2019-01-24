(defpackage "ELABORATE-MONOMORPHIC-TYPE"
  (:export "ELABORATE")
  (:use "CL")
  (:local-nicknames ("PATTERN" "PATTERN-TYPE")
                    ("MONO" "MONO-TYPE")
                    ("S" "SKELETON-TYPE")))

(in-package "ELABORATE-MONOMORPHIC-TYPE")
(defun skeleton-function-results (skeleton)
  (check-type skeleton s:function)
  (destructuring-bind (args results)
      (s:split skeleton)
    (declare (ignore args))
    results))

(defun list-of-poly (x)
  (and (listp x)
       (every #'poly-type:type-p x)))

(defun elaborate (function-pattern mono-arguments result-pattern)
  (check-type function-pattern pattern:function)
  (check-type mono-arguments list)
  (assert (every #'mono:type-p mono-arguments))
  (check-type result-pattern list)
  (assert (every #'pattern:type-p result-pattern))
  (labels ((fail (message &rest arguments)
             (apply 'format *error-output* message arguments)
             (return-from elaborate nil))
           (check (value type message &rest arguments)
             (cond (value
                    (assert (typep value type))
                    value)
                   (t
                    (apply #'fail message arguments)))))
    (let*
        ((condition:*counter* 0)
         (poly-function
          (check (pattern-to-poly:convert function-pattern mono-arguments)
                 'poly-type:function
                 "Unable to match pattern ~S with arguments ~S."
                 function-pattern mono-arguments))
         (skel-function
          (check (poly-to-skeleton:convert poly-function mono-arguments)
                 's:function
                 "Unable to match polymorphic function ~S with arguments ~S."
                 poly-function mono-arguments))
         (poly-results
          (check (return-pattern-to-poly:convert result-pattern
                                                 (skeleton-function-results
                                                  skel-function))
                 '(satisfies list-of-poly)
                 "Unable to expand return pattern ~S with skeleton ~S."
                 result-pattern skel-function))
         (sort-solution
          (check (solve-sort-constraints:solution skel-function poly-function
                                                  mono-arguments poly-results)
                 'solve-sort-constraints:solution
                 "Unable to assign sorts for ~S with arguments ~S and result ~S."
                 poly-function mono-arguments poly-results))
         (flow-info
          (check (gather-skeleton-flow-info:info skel-function sort-solution)
                 'gather-skeleton-flow-info:info
                 "Failed to gather flow info for skeleton ~S / ~S."
                 skel-function sort-solution))
         (initial-arg-conditions
          (check (gather-argument-conditions:conditions poly-function
                                                        skel-function
                                                        mono-arguments
                                                        poly-results)
                 'gather-argument-conditions:conditions
                 "Failed to gather argument conditions for ~S ~
                  with arguments ~S and result ~S."
                 poly-function mono-arguments poly-results))
         (argument-conditions
          (check (backfill-argument-conditions:backfill skel-function
                                                        flow-info
                                                        initial-arg-conditions) 
                 'gather-argument-conditions:conditions
                 "Failed to backfill argument conditions for ~
                  ~S with flow info ~S and initial conditions ~S."
                 skel-function flow-info initial-arg-conditions))
         (function-contract
          (check (gather-polymorphic-contract:contract skel-function poly-function)
                 'gather-polymorphic-contract:contract
                 "Failed to extract polymorphic function contract for ~
                  ~S with skeleton ~S."
                 poly-function skel-function)))
      ;; And we're done!
      (check (monomorphise-skeleton:monomorphise skel-function
                                                 flow-info
                                                 argument-conditions
                                                 function-contract)
             'mono:function
             "Failed to monomorphise skeleton ~S with flow info ~S, ~
              argument conditions ~S, and contract ~S."
             skel-function flow-info argument-conditions function-contract))))
