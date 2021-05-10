(in-package :hafen-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Gob API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Gob Tags
(defconstant +plant+ (svar +tag+ "PLANT"))
(defconstant +human+ (svar +tag+ "HUMAN"))
(defconstant +animal+ (svar +tag+ "ANIMAL"))
(defconstant +tamed-animal+ (svar +tag+ "TAMED_ANIMAL"))
(defconstant +water-vehicle+ (svar +tag+ "WATER_VEHICLE"))
(defconstant +vehicle+ (svar +tag+ "VEHICLE"))
(defconstant +tree+ (svar +tag+ "TREE"))
(defconstant +bush+ (svar +tag+ "BUSH"))
(defconstant +log+ (svar +tag+ "LOG"))
(defconstant +rock+ (svar +tag+ "ROCK"))
(defconstant +stump+ (svar +tag+ "STUMP"))
(defconstant +stockpile+ (svar +tag+ "STOCKPILE"))
;; Func: gob-c
;; Desc: Get the coordinate of this gob in the world
;; Inputs:
;;   1) Gob - The Gob
;; Return: Coord3f
(java-func +gob+ gob-c "getc")
;; Func: gob-name
;; Desc: Get the res name of the gob
;; Inputs:
;;   1) Gob - The Gob
;; Return: String
(java-func +gob+ gob-name "name")
;; Func: gob-v
;; Desc: Get the velocity of this gob
;; Inputs:
;;   1) Gob - The Gob
;; Return: double
(java-func +gob+ gob-v "getv")
;; Func: gob-sdt
;; Desc: Get the SDT number on this gob's Drawable
;; Inputs:
;;   1) Gob - The Gob
;; Return: int
(java-func +gob+ gob-sdt "sdt")
;; Func: gob-poses
;; Desc: Get an array of poses by res name attached to this gob
;; Inputs:
;;   1) Gob -  The Gob
;; Return: String[]
(java-func +gob+ gob-poses "poses")
;;;; Holding / Held by funcs
;; Func: gob-is-holding
;; Desc: Check if this gob is holding a given gob's id
;; Inputs:
;;   1) Gob - The gob
;;   2) long - Other gob's gob-id
;; Return: Boolean
(java-func +gob+ gob-is-holding "isHolding" +long+)
;; Func: gob-is-held-by-something
;; Desc: Check if this gob is held by anything
;; Inputs:
;;   1) Gob - The Gob
;; return: Boolean
(java-func +gob+ gob-is-held-by-something "isHeldBySomething")
;; Func: gob-is-held-by
;; Desc: Check if this gob is held by a given gob's id
;; Inputs:
;;   1) Gob - The Gob
;;   2) long - Other gob's gob-id
;; Return: Boolean
(java-func +gob+ gob-is-held-by "isHeldBy" +long+)
;; Func: gob-held-by
;; Desc: Gets the Gob ID of who is holding this gob, if any
;; Inputs:
;;   1) Gob - The Gob
;; Return: Long - Holding gob's id or -1 if none
(java-func +gob+ gob-held-by "whoIsHoldingMe")
;; Func: gob-how-many-gobs-held
;; Desc: Get a number of gods held by this gob
;; Inputs:
;;   1) Gob - The Gob
;; Return: Integer
(java-func +gob+ gob-how-many-gobs-held "howManyGobsHeld")
;; Func: gob-held-gobs
;; Desc: Gets an array of all gob ids that this gob is holidng
;; Inputs:
;;   1) Gob - The Gob
;; Return: long[] - Array of gob ids
(java-func +gob+ gob-held-gobs "heldGobs")
;;;; Tag system
;; Func: gob-has-tag
;; Desc: Check if this gob has a given Gob Tag
;; Inputs:
;;   1) Gob - The Gob
;;   2) Tag - Given Gob Tag (see: Gob Tags)
;; Return: Boolean
(java-func +gob+ gob-has-tag "hasTag" +tag+)
;; Func: is-gob-a
;; Desc: Alias for gob-has-tag
(defmacro is-gob-a (gob tag)
  `(gob-has-tag ,gob ,tag))

;;;; Misc
;; Func: is-gob-dead
;; Desc: Checks if the given gob is dead
;; Inputs:
;;   1) Gob - The Gob
;; Return: Boolean
(java-func +gob+ is-gob-dead "isDead")
;; Func: is-gob-friendly
;; Desc: Checks if the given gob is friendly
;; Inputs:
;;   1) Gob - The Gob
;; Return: Boolean
(java-func +gob+ is-gob-friendly "isFriendly")
;; Func: is-gob-dangerous
;; Desc: Checks if the given gob is dangerous
;; Inputs:
;;   1) Gob - The Gob
;; Return: Boolean
(java-func +gob+ is-gob-dangerous "isDangerous")
;; Func: gob-kinnmae
;; Desc: Gets the Kin name of a Human Tagged Gob
;; Inputs:
;;   1) Gob - The Gob
;; Return: String, or nil if it has none
(java-func +gob+ gob-kinname "kinname")
;; Func: gob-equipment
;; Desc: Get an array of equipment res names that this Human Tagged gob has on
;; Inputs:
;;   1) Gob - The Gob
;; Return: String[]
(java-func +gob+ gob-equipment "equipment")
;; Func: gob-sail
;; Desc: Get the image of a sail if this gob is a snekkja or knarr and it has a custom sail
;; Inputs:
;;   1) Gob - The Gob
;; Return: BufferedImage
(java-func +gob+ gob-sail "sail")

;;;; Gob Fields
;; Desc: The gob's id
(java-field gob-id "id")
;; Desc: The gob's Coord2d coordinate within the world
(java-field gob-rc "rc")
;; Desc: The gob's angle within the world
(java-field gob-angle "a")

;; Func: my-gob
;; Desc: Get the player gob
;; Return: Gob
(defun my-gob ()
  (oc-get-gob (mv-plgob (mv))))

(java-func +gob+ is-gob-moving-1 "moving")
;; Func: is-gob-moving
;; Desc: Checks if this gob is moving
;; Inputs:
;;   1) Gob - The gob
;; Return: Boolean
(defun is-gob-moving (gob)
  (or (is-gob-moving-1 gob)
      (and (= (mv-plgob (mv)) (gob-id gob))
           (mv-has-moves (mv)))))

;; Func: wait-for-movement
;; Desc: Waits for a given gob to begin moving and then finish moving
;; Inputs:
;;   1) Gob - The Gob
;;   2) Function - (Optional) F unction to test if gob is moving
(defun wait-for-movement (&key (gob (my-gob)) (test #'is-gob-moving))
  (progn
     (wait-for-movement-to-start :gob gob :test test)
     (wait-for-movement-to-finish :gob gob :test test)))

;; Func: wait-for-movement-to-start
;; Desc: Waits for a given gob to begin moving
;; Inputs:
;;   1) Gob - The Gob
;;   2) Function - (Optional) F unction to test if gob is moving
(defun wait-for-movement-to-start (&key (gob (my-gob)) (test #'is-gob-moving))
  (let ((start-c (gob-rc gob)))
    (wait-until (lambda () (or (funcall test gob)
                               (not (coord2d-eq start-c (gob-rc gob)))))
                :timeout 2000)))
;; Func: wait-for-movement-to-finish
;; Desc: Waits for a given gob to stop moving
;; Inputs:
;;   1) Gob - The Gob
;;   2) Function - (Optional) F unction to test if gob is moving
(defun wait-for-movement-to-finish (&key (gob (my-gob)) (test #'is-gob-moving))
  (sleep 0.5)
  (wait-until (lambda ()
                (not (funcall test gob)))))

;; Func: gob-get-by-name
;; Desc: Gets a gob by its res name
;; Inputs:
;;   1) String - The res name
;; Return: Gob, or nil if couldn't find
(defun gob-get-by-name (name)
  (let ((gobs (oc-get-all-gobs)))
    (doarr (gob gobs)
      (when (search name (gob-name gob))
        (return-from gob-get-by-name gob)))
    nil))

;; Func: gob-get-all-by-filter
;; Desc: Gets a list of gobs based off a filter function
;; Inputs:
;;   1) Function - The filter function to accept a gob or not
;; Return: List of Gob
(defun gob-get-all-by-filter (filter-fun)
  (let ((gobs (oc-get-all-gobs))
        (ret ()))
    (doarr (gob gobs)
      (when (funcall filter-fun gob)
        (push gob ret)))
    ret))

;; Func: gob-get-all-by-name
;; Desc: Gets a list of gobs based off a res name
;; Inputs:
;;   1) String - The res name
;; Return: List of Gob
(defmacro gob-get-all-by-name (name)
  `(gob-get-all-by-filter (lambda (gob) (string= ,name (gob-name gob)))))

;; Func: gob-get-closest-by-filter-and-path
;; Desc: Gets the closest gob based off a filter function and path distance from player Gob
;; Inputs:
;;   1) Function - The filter function to accept a gob or not
;; Return: Gob
(defun gob-get-closest-by-filter-and-path (filter-fun)
  (let ((me (my-gob)))
    (if me
        (let ((gobs (gob-get-all-by-filter filter-fun))
              (best nil)
              (bestdist 0)
              (mc (gob-rc me)))
          (dolist (gob gobs)
            (let* ((path (mv-find-path-to-gob gob))
                   (dist (if path (mv-path-distance path) 99999999)))
              (when (and path
                         (or (null best)
                             (< dist bestdist)))
                (setf best gob)
                (setf bestdist dist))))
          best)
        nil)))

;; Func: gob-get-closest-by-filter
;; Desc: Get the closest gob based off a filter function
;; Inputs:
;;   1) Function - The filter function to accept a gob or not
;; Return: Gob
(defun gob-get-closest-by-filter (filter-fun)
  (let ((me (my-gob)))
    (if me
        (let ((gobs (gob-get-all-by-filter filter-fun))
              (best nil)
              (bestdist 0)
              (mc (gob-rc me)))
          (dolist (gob gobs)
            (when (or (null best)
                      (< (coord2d-dist (gob-rc gob) mc) bestdist))
              (setf best gob)
              (setf bestdist (coord2d-dist (gob-rc gob) mc))))
          best)
        nil)))

;; Func: gob-get-closest-by-name-and-path
;; Desc: Gets the closest gob based off a res name and path distance from player
;; Inputs:
;;   1) String - The res name
;; Return: Gob
(defmacro gob-get-closest-by-name-and-path (name)
  `(gob-get-closest-by-filter-and-path (lambda (gob) (string= ,name (gob-name gob)))))

;; Func: gob-get-closest-by-name
;; Desc: Gets the closest gob based off a res name
;; Inputs:
;;   1) String - The res name
;; Return: Gob
(defmacro gob-get-closest-by-name (name)
  `(gob-get-closest-by-filter (lambda (gob) (string= ,name (gob-name gob)))))

(export '(+plant+ +human+ +animal+ +tamed-animal+ +water-vehicle+ +vehicle+ +tree+ +bush+ +log+ +rock+ +stump+ +stockpile+
          gob-c gob-name gob-has-tag gob-overlays gob-poses gob-sdt is-gob-dead
          gob-is-holding gob-is-held-by-something gob-is-held-by gob-held-by gob-how-many-gobs-held gob-held-gobs
          gob-kinname gob-equipment gob-sail
          gob-id gob-rc gob-v gob-angle
          is-gob-a my-gob
          is-gob-moving is-gob-friendly is-gob-dangerous
          wait-for-movement
          wait-for-movement-to-start
          wait-for-movement-to-finish
          gob-get-by-name gob-get-all-by-name gob-get-all-by-filter
          gob-get-closest-by-filter-and-path
          gob-get-closest-by-name-and-path
          gob-get-closest-by-filter gob-get-closest-by-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Gob Overlay
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant +gob-overlay+ "haven.Gob$Overlay")
;; Func: gob-overlays
;; Desc: Get an array of overlays attached to this gob
;; Inputs:
;;   1) Gob - The Gob
;; Return: Overlay[] 
(java-func +gob+ gob-overlays "overlays")
;; Func: gob-overlay-name
;; Desc: Get the name of the given overlay
;; Inputs:
;;   1) Overlay - The overlay
;; Return: String
(java-func +gob-overlay+ gob-overlay-name "name")
;; Func: gob-overlay-id
;; Desc: Get the id of the given overlay
;; Inputs:
;;   1) Overlay - The overlay
;; Return: int
(java-field gob-overlay-id "id")
;; Func: gob-overlay-has
;; Desc: Check if this gob has an overlay with the given res name
;; Inputs:
;;   1) Gob - The Gob
;;   2) String - overlay name
;; Return: Boolean
(defun gob-overlay-has (gob overlay-name)
  (doarr (ol (gob-overlays gob))
    (when (string= (gob-overlay-name ol) overlay-name)
      (return-from gob-overlay-has t)))
  nil)
;; Func: gob-get-overlay-by-name
;; Desc: Get an overlay, if it exists, by its res name from the given g ob
;; Inputs:
;;   1) Gob - The Gob
;;   2) String - overlay name
;; Return: Overlay or nil if no overlay by that name exists
(defun gob-get-overlay-by-name (gob overlay-name)
  (doarr (ol (gob-overlays gob))
    (when (string= (gob-overlay-name ol) overlay-name)
      (return-from gob-get-overlay-by-name ol)))
  nil)

(export '(gob-overlays gob-overlay-name gob-overlay-id gob-overlay-has gob-get-overlay-by-name))
