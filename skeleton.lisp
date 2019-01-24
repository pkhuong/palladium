(defpackage "SKELETON-TYPE"
  (:use "CL")
  (:local-nicknames ("C" "CONDITION"))
  (:shadow "TYPE" "FUNCTION")
  (:export "*"
           "TYPE" "FUNCTION" "BOX" "BASE"
           "TYPE-P" "FUNCTION-P" "BOX-P" "BASE-P"
           "SPLIT" "PARSE"))

(in-package "SKELETON-TYPE")

(defgeneric split (value))

(defgeneric build (tag arguments))

(defun parse (expression)
  (if (typep expression '(cons symbol))
      (destructuring-bind (tag &rest args) expression
        (build tag args))
      (mapcar #'parse expression)))

(defstruct (type
             (:constructor nil)))

(defun list-of-skeleton (x)
  (and (listp x)
       (every #'type-p x)))

(defun list-of-symbol (x)
  (and (listp x)
       (every #'symbolp x)))

(defstruct (function
             (:include type)
             (:constructor function (arguments results)))
  (arguments nil
             :type (and list (satisfies list-of-skeleton))
             :read-only t)
  (results nil
           :type (and list (satisfies list-of-skeleton))
           :read-only t))

(defmethod split ((skeleton function))
  (list (function-arguments skeleton)
        (function-results skeleton)))

(defmethod build ((tag (eql 'function)) arguments)
  (destructuring-bind (arguments results) arguments
    (function (parse arguments)
              (parse results))))

(defstruct (box
             (:include type)
             (:constructor box (contents)))
  (contents nil :type type :read-only t))

(defmethod split ((skeleton box))
  (list (box-contents skeleton)))

(defmethod build ((tag (eql 'box)) arguments)
  (destructuring-bind (contents) arguments
    (box (parse contents))))

(defstruct (base
             (:include type)
             (:constructor base (name polarity flow)))
  (name nil :type symbol :read-only t)
  ;; + is covariant (values returned by the function), - is
  ;; contravariant (values accepted by the function).
  (polarity nil :type (member + -) :read-only t)
  ;; list of equivalence classes where this type flows into and out
  ;; of, depending on polarity.
  (flow nil :type (and cons (satisfies list-of-symbol)) :read-only t))

(defmethod split ((skeleton base))
  (list (base-name skeleton)
        (base-polarity skeleton)
        (base-flow skeleton)))

(defmethod build ((tag (eql 'base)) arguments)
  (apply #'base arguments))
