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
;; Func: fv
;; Desc: Gets the Fightview widget
;; Inputs: N/A
;; Return: Fightview
(defun fv ()
  (gui-fv (ui-gui (ui))))
;; Func: fs
;; Desc: Gets the Fightsess widget
;; Inputs: N/A
;; Return: Fightsess
(defun fs ()
  (gui-fs (ui-gui (ui))))
;; Func: actions
;; Desc: Gets all of your actions (cards) during a combat session
;; Inputs: N/A
;; Return: List of Action
(defun actions ()
  (let ((ret ()))
    (doarr (act (fs-actions (fs)))
      (when act
        (push act ret)))
    ret))

;; Field: action-id
;; Desc: Get an action card's id
(java-field action-id "id")
;; Field: action-card
;; Desc: Get the card for a given action
(java-field action-card "card")
;; Field: action-number-of-cards
;; Desc: Gets how many  cards this specific action has applied to it [1,5]
(java-field action-number-of-cards "cards")

;; Func: fight-use-action
;; Desc: Use a given action by its id
;; Inputs:
;;   1) int - Action id
;; Return: N/A
(defmacro fight-use-action (action-id)
  `(wdgmsg (fs) "use" ,action-id 1 +mf-none+))

(export '(fs fv actions action-id action-card action-number-of-cards fight-use-action))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Card related fields / functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Field: card-name
;; Desc: Gets the name of a Card
(java-field card-name "name")
;; Func: card-cooldown
;; Desc: Gives you the cooldown of a particular card if used based on number of them
;; Inputs:
;;   1) int - Number of cards to apply
;; Return: double
(java-func +card+ card-cooldown "cooldown" +int+)
;; Func: attackp
;; Desc: Check if a Card is an attack type
;; Inputs:
;;   1) Card - The card to check
;; Return: boolean
(defun attackp (card)
  (jinstance-of-p card +attack+))
;; Func: maneuverp
;; Desc: Check if a Card is a maneuver type
;; Inputs:
;;   1) Card - The card to check
;; Return: boolean
(defun maneuverp (card)
  (jinstance-of-p card +maneuver+))
;; Func: restorationp
;; Desc: Check if a Card is a restoration type
;; Inputs:
;;   1) Card - The card to check
;; Return: boolean
(defun restorationp (card)
  (jinstance-of-p card +restoration+))

(export '(card-name card-cooldown attackp maneuverp restorationp))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Relation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Func: peace-toggle
;; Desc: Toggles the peace button on a given relation
;; Inputs:
;;   1) Relation - The relation
;; Return: N/A
(java-func +relation+ peace-toggle "peace")
(java-func +fightview+ get-relations1 "getrelations")
;; Field: rel-gob-id 
;; Desc: Gets the Gob ID of a given relation
(java-field rel-gob-id "gobid")
;; Func: get-relations
;; Desc: Gets all the relations currently registered in the client
;; Inputs: N/A
;; Return: Relation[]
(defmacro get-relations ()
	`(get-relations1 (fv)))
;; Func: get-relation
;; Desc: Gets a specific relation based off a gob-id
;; Inputs:
;;   1) long - The gob-id the relation is for
;; Return: Relation or nil if none found with given gob-id
(defun get-relation (id)
    (let ((rels (get-relations)))
        (doarr (rel rels)
            (when (= (rel-gob-id rel) id)
                (return-from get-relation rel))))
    nil)

(export '(get-relation get-relations rel-gob-id peace-toggle))
