(defpackage :auto-drop-leech
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :hafen-config)
;; Auto drops any non-bloated leeches in your equipment slots while running.


(script
  (forever
    (doarr (itm (equipment-get-all-items))
      (when (string= "Leech" (item-name (equip-item itm)))
        (item-drop (equip-item itm))))
    (sleep 1)))
