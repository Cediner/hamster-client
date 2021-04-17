(defpackage :auto-drink
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :hafen-config)

(script
 (forever
  (check-stam-and-drink :drink-at 75 :refill nil)
  (sleep 1)))
