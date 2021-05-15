(defpackage :farming-common
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :farming-common)
(defconstant +objdata+ "hamster.data.gob.ObjData")
(defconstant +cropdata+ "hamster.data.gob.CropData")
(defconstant +croptype+ "hamster.data.gob.CropData.CropType")
(defconstant +ground-crop+ (svar +croptype+ "GROUND"))
(defconstant +trellis-crop+ (svar +croptype+ "TRELLIS"))

;; Func: is-a-crop
;; Desc: Checks if a given res name is a crop
;; Inputs:
;;   1) String - Gob res name
;; Return: Boolean
(java-sfunc +objdata+ is-a-crop "isACrop" 1)
;; Func: get-crop-data
;; Desc: Get the crop data for a given res name that is a valid crop
;; Inputs:
;;   1) String - Gob res name
;; Return: Boolean or nil
(java-sfunc +objdata+ get-crop-data "getCropData" 1)

;; Func: crop-type
;; Desc: Get the CropType for this CropData
;; Inputs:
;;   1) CropData - the crop in question
;; Return: CropType
(java-func +cropdata+ crop-type "type")
;; Func: crop-min-stage
;; Desc: Get the stage value (sdt) for the crop at which it can be harvested
;; Inputs:
;;   1) CropData - the crop in question
;; Return: int
(java-func +cropdata+ crop-min-stage "min_stage")
;; Func: crop-final-stage
;; Desc: Gets the final stage value (sdt) for the crop
;; Inputs:
;;   1) CropData - the crop in question
;; Return: int
(java-func +cropdata+ crop-final-stage "final_stage")
;; Func: crop-is-multistage
;; Desc: Checks if this crop is multistage
;; Inputs:
;;   1) CropData - the crop in question
;; Return: Boolean
(java-func +cropdata+ crop-is-multistage "multistage")

;; Func: is-a-harvestable-crop
;; Desc: Checks if a gob is a crop that is ready to harvest and is of a specific CropType
;; Inputs:
;;   1) Gob - The gob in question
;;   2) CropType - The type of crop to target
;; Return: Boolean
(defun is-a-harvestable-crop (gob kind)
  (if (is-a-crop (gob-name gob))
      (let* ((name (gob-name gob))
             (stage (gob-sdt gob))
             (crop (get-crop-data name)))
        (if (and crop
                 (jeq (crop-type crop) kind)
                 (>= stage (crop-min-stage crop)))
            t
          nil))
    nil))

;; Func: is-a-seed
;; Desc: Checks if a WItem is a seed
;; Inputs:
;;   1) WItem - The item in question
;; Return: Boolean
(defun is-a-seed (itm)
  (let ((name (item-name itm)))
    (or (search "seeds" name)
        (string= "gfx/invobjs/beet" name)
        (string= "gfx/invobjs/carrot" name))))

(export '(+ground-crop+ +trellis-crop+ is-a-crop get-crop-data is-a-harvestable-crop is-a-seed
          crop-type crop-min-stage crop-final-stage crop-is-multistage))
