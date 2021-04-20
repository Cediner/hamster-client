(defpackage :auto-drop-stones
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :hafen-config)
;; Auto drops any known stones from your inventory for mining purposes

(script
 (let ((stones '("Orthoclase" "Mica" "Feldspar" "Cinnabar" "Olivine"
                 "Sandstone" "Apatite" "Basalt" "Alabaster" "Fluorospar"
                 "Schist" "Limestone" "Arkose" "Gabbro" "Kyanite"
                 "Korund" "Breccia" "Hornblende" "Quartz" "Zincspar"
                 "Gneiss" "Microlite" "Diabase" "Porphyry" "Sodalite"
                 "Marble" "Granite"  "Dolomite" "Flint" "Soapstone"
                 "Rhyolite" "Quarryartz" "Greenschist" "Diorite" "Dross"
                 "Jasper" "Schrifterz" "Slate" "Pegmatite")))
   (forever
    (dolist (stone stones)
      (inventories-drop-all-items-by-name stone))
    (sleep 1))))
