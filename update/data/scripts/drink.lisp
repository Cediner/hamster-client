(defpackage :drink
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :hafen-config)

(script
  (check-stam-and-drink :drink-at 100 :refill nil))
