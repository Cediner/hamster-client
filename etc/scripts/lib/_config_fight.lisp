(in-package :hafen-config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Everything Combat
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant +fightview+ "haven.Fightview")
(defconstant +fightsess+ "haven.Fightsess")
(defconstant +relation+ "haven.Fightview$Relation")
(defconstant +card+ "hamster.ui.fight.Card")
(defconstant +attack+ "hamster.ui.fight.Attack")
(defconstant +maneuver+ "hamster.ui.fight.Maneuver")
(defconstant +restoration+ "hamster.ui.fight.Restoration")
(java-field gui-fv "fv")
(java-field gui-fs "fs")
(java-field fs-actions "actions")

(defun fv ()
  (gui-fv (ui-gui (ui))))
(defun fs ()
  (gui-fs (ui-gui (ui))))

(defun actions ()
  (let ((ret ()))
    (doarr (act (fs-actions (fs)))
      (when act
        (push act ret)))
    ret))

(java-field action-id "id")
(java-field action-card "card")
(java-field action-number-of-cards "cards")

(defmacro fight-use-action (action-id)
  `(wdgmsg (fs) "use" ,action-id 1 +mf-none+))

(java-field card-name "name")
;;Argument -> # of cards for this card
(java-func +card+ card-cooldown "cooldown" +int+)
(defun attackp (card)
  (jinstance-of-p card +attack+))
(defun maneuverp (card)
  (jinstance-of-p card +maneuver+))
(defun restorationp (card)
  (jinstance-of-p card +restoration+))

(export '(fs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Relation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(java-func +relation+ peace-toggle "peace")
(java-func +fightview+ get-relations1 "getrelations")

(java-field rel-gob-id "gobid")

(defmacro get-relations ()
	`(get-relations1 (fv))
)

(defun get-relation (id)
    (let ((rels (get-relations)))
        (doarr (rel rels)
            (when (= (rel-gob-id rel) id)
                (return-from get-relation rel))))
    nil)


(export '(get-relation get-relations rel-gob-id peace-toggle))
