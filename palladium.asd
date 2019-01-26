(asdf:defsystem palladium
  :version "0.0.1"
  :author "Paul Khuong"
  :mailto "pvk@pvk.ca"
  :license "Apache v2"
  :depends-on ("alexandria" "cl-smt" "cl-smt.z3")
  :components (;; callable -> function
               (:file "coerce-to-fun")
               ;; ordered hashed set / map
               (:file "ordered")
               ;; SMT side condition
               (:file "condition" :depends-on ("ordered"))
               ;; union find + value per equivalence class
               (:file "attributed-disjoint-set" :depends-on ("coerce-to-fun"
                                                             "ordered"))
               (:file "occur-check")
               ;; Tracks virtual values in scope and their de bruijn indices,
               ;; and maps between local expressions and globalised ones.
               (:file "scoping" :depends-on ("condition"))
               ;; monomorphic types
               (:file "mono" :depends-on ("condition"))
               ;; function call patterns
               (:file "pattern" :depends-on ("condition"))
               ;; polymorphic types
               (:file "poly" :depends-on ("condition"))
               ;; monomorphic type skeleton
               (:file "skeleton" :depends-on ("condition"))
               ;; Tracks causally available skeleton:base types in the
               ;; current scope.
               (:file "causality" :depends-on ("skeleton"))
               ;; utility to create and cache mappings from spread
               ;; variables to polymorphic variables.
               (:file "spread-to-poly" :depends-on ("condition"
                                                    "pattern" "poly"))
               ;; function call pattern to polymorphic type
               (:file "pattern-to-poly"
                      :depends-on ("ordered"
                                   "condition"
                                   "mono" "pattern" "poly"))
               ;; return value pattern to list of polymorphic types
               (:file "return-pattern-to-poly"
                      :depends-on ("ordered"
                                   "condition"
                                   "skeleton" "pattern" "poly"))
               ;; polymorphic function type to skeleton
               (:file "poly-to-skeleton"
                      :depends-on ("ordered"
                                   "condition"
                                   "mono" "skeleton" "poly"))
               ;; adjoin sort constraints for the skeleton to match
               ;; the monomorphic arguments, the polymorphic function
               ;; call, and the polymorphic return types.
               (:file "solve-sort-constraints"
                      :depends-on ("coerce-to-fun"
                                   "attributed-disjoint-set"
                                   "skeleton" "mono" "poly"))
               ;; convert what we know about flow variables to
               ;; knowledge about base type variables in the skeleton.
               (:file "gather-skeleton-flow-info"
                      :depends-on ("ordered"
                                   "skeleton"
                                   "solve-sort-constraints"))
               ;; save information about base types of negative
               ;; polarity (that flow into the polymorphic
               ;; solution). We always have that for direct arguments,
               ;; and we should usually have that from the poly result
               ;; type: if we don't, that's the same as the programmer
               ;; telling us they don't plan on calling the result
               ;; ever.
               ;;
               ;; Of course, that last bit isn't very ergonomic, so
               ;; we'll hallucinate a "probably good enough" type
               ;; later on.
               (:file "gather-argument-conditions"
                      :depends-on ("ordered"
                                   "scoping"
                                   "skeleton" "mono" "poly"))
               ;; Come up with a type for arguments without gathered
               ;; condition.
               (:file "backfill-argument-conditions"
                      :depends-on ("ordered"
                                   "occur-check"
                                   "scoping"
                                   "gather-skeleton-flow-info"
                                   "gather-argument-conditions"
                                   "skeleton"))
               ;; Globalise and save constraints or guarantees from
               ;; the polymorphic function type.
               (:file "gather-polymorphic-contract"
                      :depends-on ("ordered"
                                   "condition"
                                   "scoping"
                                   "skeleton" "poly"))
               ;; We have known conditions for all negative values,
               ;; and we can intersect them with the polymorphic
               ;; function's preconditions to then run a subtyping
               ;; check.
               ;;
               ;; We still have to come up with conditions for
               ;; positive values. In some cases, we have a known
               ;; solution from the polymorphic type, so we use that
               ;; directly. Otherwise, use dataflow with an occur
               ;; check to synthesise each return type.
               (:file "monomorphise-skeleton"
                      :depends-on ("ordered"
                                   "condition"
                                   "scoping"
                                   "causality"
                                   "gather-skeleton-flow-info"
                                   "gather-argument-conditions"
                                   "gather-polymorphic-contract"
                                   "skeleton"))
               ;; Bring it all together to eborate a tentative
               ;; monomorphic type for a polymorphic (pattern) callee,
               ;; monomorphic arguments, and polymorphic (pattern)
               ;; expected return types.
               (:file "elaborate-monomorphic-type"
                      :depends-on (;; function types are defined with
                                   ;; pattern types, same for expected
                                   ;; return values.
                                   "pattern"
                                   ;; arguments are specified with
                                   ;; monotypes.
                                   "mono"
                                   ;; take the pattern callee, convert
                                   ;; to polymorphic to match the
                                   ;; arguments
                                   "pattern-to-poly"
                                   ;; make a skeleton out of the
                                   ;; polymorphic callee and the
                                   ;; monomorphic arguments.
                                   "poly-to-skeleton"
                                   ;; take the return pattern, convert
                                   ;; to polymorphic to match the
                                   ;; skeleton
                                   "return-pattern-to-poly"
                                   ;; solve for sorts (base types)
                                   ;; with regular unification on data
                                   ;; flows.
                                   "solve-sort-constraints"
                                   ;; map flow info to the skeleton's
                                   ;; base types.
                                   "gather-skeleton-flow-info"
                                   ;; map constraints from monomorphic
                                   ;; arguments to the skeleton's base
                                   ;; types, and from the polymorphic
                                   ;; return type.
                                   "gather-argument-conditions"
                                   ;; infer constraints for arguments
                                   ;; to any returned function.
                                   "backfill-argument-conditions"
                                   ;; save pre/post conditions from
                                   ;; the polymorphic function type.
                                   "gather-polymorphic-contract"
                                   ;; generate the monomorphic
                                   ;; function type
                                   "monomorphise-skeleton")))
  :in-order-to ((asdf:test-op (asdf:test-op palladium.test))))

(asdf:defsystem palladium.test
  :depends-on (#:palladium #:prove)
  :defsystem-depends-on (#:prove-asdf)
  :components ((:test-file "test-attributed-disjoint-set")
               (:test-file "test-pattern-to-poly")
               (:test-file "test-return-pattern-to-poly")
               (:test-file "test-poly-to-skeleton")
               (:test-file "test-solve-sort-constraints")
               (:test-file "test-backfill-argument-conditions")
               (:test-file "test-gather-polymorphic-contract")
               (:test-file "test-monomorphise-skeleton")
               (:test-file "test-elaborate-monomorphic-type"))
  :perform (asdf:test-op :after (op c)
                         (funcall (intern "RUN" '#:prove) c)))
