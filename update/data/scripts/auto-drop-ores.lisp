(defpackage :auto-drop-ores
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :hafen-config)
;; Auto drops any known stones from your inventory for mining purposes

(script
 (let ((stones '("Black Ore" "Bloodstone" "Cassiterite" "Chalcopyrite"
                 "Direvein" "Galena" "Heavy Earth" "Iron Ochre" "Leaf Ore"
                 "Peacock Ore" "Silvershine" "Malachite" "Horn Silver"
                 "Black Coal" "Lead Glance")))
   (forever
    (dolist (stone stones)
      (inventories-drop-all-items-by-name stone))
    (sleep 1))))
