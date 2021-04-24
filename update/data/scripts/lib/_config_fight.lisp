(in-package :hafen-config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Everything Combat
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant +fightview+ "haven.Fightview")
(defconstant +fightsess+ "haven.Fightsess")
(defconstant +relation+ "haven.Fightview$Relation")
(defconstant +def-type+ "hamster.ui.fight.DefenseType")
(defconstant +def-type-red+ (svar +def-type+ "RED"))
(defconstant +def-type-green+ (svar +def-type+ "GREEN"))
(defconstant +def-type-blue+ (svar +def-type+ "BLUE"))
(defconstant +def-type-yellow+ (svar +def-type+ "YELLOW"))
(defconstant +card+ "hamster.ui.fight.Card")
(defconstant +attack+ "hamster.ui.fight.Attack")
(defconstant +maneuver+ "hamster.ui.fight.Maneuver")
(defconstant +restoration+ "hamster.ui.fight.Restoration")
(defconstant +weight-type+ "hamster.ui.fight.WeightType")
(defconstant +weight-type-melee+ (svar +weight-type+ "MC"))
(defconstant +weight-type-unarmed+ (svar +weight-type+ "UA"))
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

;; Func: fv-get-weight
;; Desc: Gets your weight for a given defense type
;; Inputs:
;;   1) DefenseType - The type to check
;; Return: double [0, 1] (ex: 0.7 => 70%)
(java-func +fightview+ fv-get-weight "getWeight" +weight-type+)
;; Field: fv-maneuver
;; Desc: Gets your Maneuver being used
;; Type: Maneuver
(java-field fv-maneuver "maneuver")
;; Field: fv-maneuver-meter 
;; Desc: Gets your Maneuver's meter value (for like Bloodlust, etc)
;; Type: double [0, 1] (ex: 0.7 => 70%)
(java-field fv-maneuver-meter "maneuvermeter")

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
;; Type: long
(java-field action-id "id")
;; Field: action-card
;; Desc: Get the card for a given action
;; Type: Card
(java-field action-card "card")
;; Field: action-number-of-cards
;; Desc: Gets how many cards this specific action has applied to it [1,5]
;; Type: int
(java-field action-number-of-cards "cards")

;; Func: fight-use-action
;; Desc: Use a given action by its id
;; Inputs:
;;   1) int - Action id
;; Return: N/A
(defmacro fight-use-action (action-id)
  `(wdgmsg (fs) "use" ,action-id 1 +mf-none+))

(export '(fs fv actions action-id action-card action-number-of-cards fight-use-action fv-get-weight fv-maneuver fv-maneuver-meter))
(export '(+def-type-red+ +def-type-green+ +def-type-blue+ +def-type-yellow+))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Card related fields / functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Field: card-name
;; Desc: Gets the name of a Card
;; Type: String
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
;; Field: attack-type 
;; Desc: Get the WeightType that this attack uses (Unarmed or Melee)
;; Type: WeightType
(java-field attack-type "type")
;; Field: attack-ip-cost 
;; Desc: Gets the IP cost of this attack
;; Type: int
(java-field attack-ip-cost "cost")
;; Field: attack-base-damage 
;; Desc: Gets the base damage of this attack (Only for UA attack-type)
;; Type: int
(java-field attack-base-damage "damage")
;; Field: attack-damage-weight 
;; Desc: Gets the damage weight of this attackc [0,1] (ex: 125% => 1.25) (Only useful for MC attack-type)
;; Type: double
(java-field attack-damage-weight "damageweight")
;; Field: attack-grievous-weight 
;; Desc: Gets the grievous weight of this attack [0,1] (ex: 125% => 1.25)
;; Type: double
(java-field attack-grievous-weight "grievousDamageWeight")
;; Func: attack-get-opening-weight
;; Desc: Get the opening weight applied for a given WeightType
;; Inputs:
;;   1) WeightType -  The weight type to check
;; Return: double [0,1] (ex: 25% => 0.25, 0.0 means no opening applied)
(java-func +attack+ attack-get-opening-weight "getWeight" +weight-type+)
;; Func: attack-get-damage-types
;; Desc: Get the damage types this attack is based on
;; Inputs:
;;   1) Attack - The attack card
;; Return: DefenseType[]
(java-func +attack+ attack-get-damage-types "getDamageTypes")
;; Func: attack-weight
;; Desc: Get the attack weight of this card when applied given other stat/fight details
;; Inputs:
;;   1) Attack - The attack card to base off
;;   2) Maneuver - The maneuver card in use
;;   3) double - The maneuver's meter value
;;   4) int - UA value
;;   5) int - MC value
;;   6) int - Number of cards applied to this Attack [1,5]
;; Return: 
(java-func +attack+ attack-weight "getAttackweight" +maneuver+ +double+ +int+ +int+ +int+)

;; Func: maneuverp
;; Desc: Check if a Card is a maneuver type
;; Inputs:
;;   1) Card - The card to check
;; Return: boolean
(defun maneuverp (card)
  (jinstance-of-p card +maneuver+))
;; Field: maneuver-weight-type
;; Desc: The WeightType this Maneuver is based off
;; Type: WeightType
(java-field maneuver-weight-type "type")
;; Field: maneuver-weight
;; Desc: The weight value for this Maneuver (note: 125% => 1.25)
;; Type: double
(java-field maneuver-weight "weight")

;; Func: restorationp
;; Desc: Check if a Card is a restoration type
;; Inputs:
;;   1) Card - The card to check
;; Return: boolean
(defun restorationp (card)
  (jinstance-of-p card +restoration+))
;; Field: restoration-type
;; Desc: Get the WeightType for this Restoration
;; Type: WeightType
(java-field restoration-type "attacktype")
;; Field: restoration-weight
;; Desc: Get the weighting of the WeightType for this Restoration 
;; Type: double [0,1] (ex: 100% => 1.0)
(java-field restoration-weight "attackweight")
;; Field: restoration-ip-cost
;; Desc: Get the IP cost to use this restoration
;; Type: int
(java-field restoration-ip-cost "cost")
;; Func: restoration-get-opening-weight
;; Desc: Get the opening weight applied by this Restoration based off given WeightType
;; Inputs:
;;   1) Restoration - This card
;;   2) WeightType - The weight type to check for weight
;; Return: double [0,1] (ex: 15% => 0.15, 0.0 means no opening applied)
(java-func +restoration+ restoration-get-opening-weight "getOpening" +weight-type+)
;; Func: restoration-get-reduction-weight
;; Desc: Get the reduction weight applied by this Restoration based off given WeightType
;; Inputs:
;;   1) Restoration - This card
;;   2) WeightType - The weight type to check for reduction
;; Return: double [0,1] (ex: 10% => 0.1, 0.0 mens no reduction)
(java-func +restoration+ restoration-get-reduction-weight "getReduction" +weight-type+)


;; Func: meleep
;; Desc: Check if a WeightType is a Melee type
;; Inputs:
;;   1) WeightType - The weight type to check
;; Return: boolean
(defun meleep (weight)
  (jinstance-of-p weight +weight-type-melee+))
;; Func: unarmedp
;; Desc: Check if a WeightType is a Unarmed type
;; Inputs:
;;   1) WeightType - The weight type to check
;; Return: boolean
(defun unarmedp (weight)
  (jinstance-of-p weight +weight-type-unarmed+))

(export '(card-name card-cooldown
                    attackp attack-type attack-weight attack-ip-cost attack-base-damage
                    attack-damage-weight attack-grievous-weight attack-get-opening-weight attack-get-damage-types
                    maneuverp maneuver-weight-type maneuver-weight
                    restorationp restoration-type restoration-weight restoration-ip-cost restoration-get-opening-weight restoration-get-reduction-weight
                    +weight-type-melee+ +weight-type-unarmed+ meleep unarmedp))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Relation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Func: peace-toggle, rel-peace
;; Desc: Toggles the peace button on a given relation
;; Inputs:
;;   1) Relation - The relation
;; Return: N/A
(java-func +relation+ peace-toggle "peace")
(defmacro rel-peace (relation)
  `(peace-toggle ,relation))
(java-func +fightview+ get-relations1 "getrelations")
;; Func: rel-get-weight
;; Desc: Gets a specified Defensive Weight for the given relation
;; Inputs:
;;   1) DefenseType - The weight color (+def-type-<color>+)
;; Return: double [0, 1] (0.7 => 70%)
(java-func +relation+ rel-get-weight "getWeight" +weight-type+)
;; Field: rel-your-ip 
;; Desc: Get your IP on a given relation
;; Type: int
(java-field rel-your-ip "ip")
;; Field: rel-their-ip
;; Desc: Gets the relation's IP against you
;; Type: int
(java-field rel-their-ip "oip")
;; Field: rel-gob-id 
;; Desc: Gets the Gob ID of a given relation
;; Type: long
(java-field rel-gob-id "gobid")
;; Field: rel-maneuver
;; Desc: Gets the Maneuver the relation is using
;; Type: Maneuver
(java-field rel-maneuver "maneuver")
;; Field: rel-maneuver-meter 
;; Desc: Gets the Maneuver's meter value for a relation (for like Bloodlust, etc)
;; Type: double [0, 1] (ex: 0.7 => 70%)
(java-field rel-maneuver-meter "maneuvermeter")
;; Field: rel-estimated-block-weight
;; Desc: Gets the estimated block weight for a given relation
;; Type: double
(java-field rel-estimated-block-weight "estimatedBlockWeight")
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

(export '(get-relation get-relations rel-gob-id rel-peace peace-toggle rel-get-weight rel-your-ip rel-their-ip))
