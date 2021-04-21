(in-package :hafen-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Equipory
;;;; This is for getting equipment you have on your character to manipulate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant +equ+ "haven.Equipory")
(java-func +equ+ equipment-get-all-items-1 "getEquippedItems")

(java-field gui-equ "equ")

(defun equ ()
  (gui-equ (ui-gui (ui))))

(defmacro equipment-get-all-items ()
  `(equipment-get-all-items-1 (equ)))

;; Equips held item into given slot, See EquipmentType for allowed slots
(defmacro equip (slot)
  `(wdgmsg (equ) "drop" (equip-type-slot ,slot)))

(defun get-equip-slot (slot)
  (doarr (itm (equipment-get-all-items))
    (when (jeq slot (equip-type itm))
      (return-from get-equip-slot itm)))
  nil)

(defconstant +equ-itm+ "hamster.ui.equip.EquipmentItem")
(defconstant +equ-type+ "hamster.data.character.EquipmentType")
(java-func +equ-itm+ equip-type "getType")
(java-func +equ-itm+ equip-item "getItem")
(java-func +equ-type+ equip-type-name "name")
(java-field equip-type-slot "slot")

(java-sfield head-gear +equ-type+ "HeadGear")
(java-sfield accessory +equ-type+ "Accessory")
(java-sfield shirt +equ-type+ "Shirt")
(java-sfield torso-armor +equ-type+ "TorsoArmor")
(java-sfield gloves +equ-type+ "Gloves")
(java-sfield belt +equ-type+ "Belt")
(java-sfield left-hand +equ-type+ "LeftHand")
(java-sfield right-hand +equ-type+ "RightHand")
(java-sfield left-ring +equ-type+ "LeftRing")
(java-sfield right-ring +equ-type+ "RightRing")
(java-sfield robe +equ-type+ "Robe")
(java-sfield back +equ-type+ "Back")
(java-sfield pants +equ-type+ "Pants")
(java-sfield leg-armor +equ-type+ "LegArmor")
(java-sfield cape +equ-type+ "Cape")
(java-sfield shoes +equ-type+ "Shoes")
(java-sfield cosmetic-hat +equ-type+ "CosmeticHat")
(java-sfield eyes +equ-type+ "Eyes")
(java-sfield mouth +equ-type+ "Mouth")


(export '(equipment-get-all-items equip get-equip-slot
          equip-type equip-item
          equip-type-name
          head-gear accessory shirt torso-armor gloves belt left-hand right-hand
          left-ring right-ring robe back pants leg-armor cape shoes eyes mouth cosmetic-hat))
