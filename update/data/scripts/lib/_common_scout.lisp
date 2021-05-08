(defpackage :scout-common
  (:use :common-lisp
        :cl-user
        :java
        :hafen-config))
(in-package :scout-common)

(defconstant +alert-refresh+ (* 3 60 1000))
(defconstant +targets+
  `("gfx/terobjs/vehicle/knarr"
    "gfx/terobjs/vehicle/snekkja"
    "gfx/terobjs/vehicle/rowboat"))

(defclass target ()
  ((id :initarg :id :accessor target-id)
   (name :initarg :name :accessor target-name)
   (init-rc :initarg :init-rc :accessor target-init-rc)
   (rc :initarg :rc :accessor target-rc)
   (angle :initarg :angle :accessor target-angle)
   (avg-speed :initarg :avg-speed :accessor target-avg-speed)
   (max-speed :initarg :max-speed  :accessor target-max-speed)
   (spotted-at :initarg :spotted-at  :accessor target-spotted-at)
   (last-alert :initarg :last-alert :accessor target-last-alert)
   (tick :initarg :tick :accessor target-tick)
   (changed :initarg :changed :accessor target-changed)))

(defgeneric describe (target))
(defgeneric update (target gob tick))
(defun update-target (target tick rc angle speed)
  (setf (target-rc target) rc)
  (setf (target-angle target) angle)
  (setf (target-avg-speed target)
        (/ (+ (target-avg-speed target) speed) 2))
  (setf (target-max-speed target) (max (target-max-speed target) speed))
  (setf (target-tick target) tick))

(defclass player (target)
  ((gear :initarg :gear :accessor player-gear)))

(defun make-player (gob tick)
  (make-instance 'player
                 :id (gob-id gob) :name (gob-kinname gob)
                 :init-rc (gob-rc gob) :rc (gob-rc gob) :angle (gob-angle gob)
                 :avg-speed (gob-v gob) :max-speed (gob-v gob)
                 :spotted-at (get-time) :last-alert (get-time)
                 :tick tick :changed nil :gear (gob-equipment gob)))

(defun update-player (player tick rc angle speed gear)
  (update-target player tick rc angle speed)
  (setf (target-changed player) (not (string= (describe-gear (player-gear player)) (describe-gear gear))))   
  (setf (player-gear player) gear))

(defmethod update ((target player) gob tick)
  (update-player target tick
                 (gob-rc gob) (gob-angle gob) (gob-v gob)
                 (gob-equipment gob)))

(defun describe-gear (gear)
  (if gear
      (apply #'concatenate 'string
             (loop
              for equ in (listify gear)
              collect (format nil "~A~%" equ)))
    ""))

(defmethod describe ((target player))
  (format nil "~A [~A] [Avg Speed ~A] [Max Speed ~A] [Duration ~A] Wearing:~%~A"
          (target-name target)
          (target-id target)
          (target-avg-speed target)
          (target-max-speed target)
          (- (get-time) (target-spotted-at target))
          (describe-gear (player-gear target))))

(defclass boat (target)
  ((sail :initarg :sail :accessor boat-sail)))

(defun make-boat (gob tick)
  (make-instance 'boat
                 :id (gob-id gob) :name (gob-name gob)
                 :init-rc (gob-rc gob) :rc (gob-rc gob) :angle (gob-angle gob)
                 :avg-speed (gob-v gob) :max-speed (gob-v gob)
                 :spotted-at (get-time) :last-alert (get-time)
                 :tick tick :changed nil :sail (gob-sail gob)))

(defmethod describe ((target boat))
  (format nil "~A [~A] [Avg Speed ~A] [Max Speed ~A] [Duration ~A]"
          (target-name target)
          (target-id target)
          (target-avg-speed target)
          (target-max-speed target)
          (- (get-time) (target-spotted-at target))))

(defun update-boat (boat tick rc angle speed sail)
  (update-target boat tick rc angle speed)
  (setf (target-changed boat) (or (and (not (boat-sail boat)) sail)))
  (setf (boat-sail boat) sail))

(defmethod update ((target boat) gob tick)
  (update-boat target tick
               (gob-rc gob) (gob-angle gob) (gob-v gob)
               (gob-sail gob)))

(defun make-target (gob tick)
  (if(gob-has-tag gob +human+)
    (make-player gob tick)
    (make-boat gob tick)))

(defun holding-bad-people (gob idmap)
  (when (plusp (gob-how-many-gobs-held gob))
    (let ((heldgobs (gob-held-gobs gob)))
      (doarr (held heldgobs)
        (when (gethash held idmap)
          (return-from holding-bad-people t))))
    nil))

(defun scan-for-targets (role spotted-gobs tick)
  (let ((targets (gob-get-all-by-filter
                  (lambda (gob)
                    (or (member (gob-name gob) +targets+ :test 'equal)
                        (and (gob-has-tag gob +human+)
                             (not (is-gob-friendly gob))
                             (not (= (gob-id gob) (mv-plgob (mv)))))))))
        (idmap (make-hash-table)))
    ;; Populate the idmap
    (dolist (target targets)
      (setf (gethash (gob-id target) idmap) target))
    ;; Scan targets
    (dolist (gob targets)
      (let ((target (gethash (gob-id gob) spotted-gobs)))
        (cond
         ((and target
               (> (- (get-time) (target-last-alert target)) +alert-refresh+))
          ;; Previously seen Gob that hasn't left our view by the +alert-refresh+ time
          (update target gob tick) 
          (send-discord-message-with-map-and-mark
           "player-alerts"
           (format nil "<@&~A> Stilled spotted a ~A" role (describe target))
           (target-rc target)
           (target-angle target)))
         (target
          ;; Previously seen Gob that hasn't reached the alert-refresh yet
          (update target gob tick)
          (when (target-changed target)
            (if (typep target 'player)
                (send-discord-message "player-alerts" (format nil "<@&~A> Target changed: ~A" role (describe target)))
              (when (and (typep target 'boat)
                         (boat-sail target))
                (send-discord-image "player-alerts" (format nil "<@&~A> Custom boat sail for ~A" role (target-id target))
                                    (boat-sail target))))))
                
         (t
          ;; Brand new target, don't add if they aren't human and being held or have only friendly passengers
          (when (or (gob-has-tag gob +human+)
                    (and (not (gob-is-held-by-something gob))
                         (holding-bad-people gob idmap)))
            (setf target (make-target gob tick))
            (setf (gethash (target-id target) spotted-gobs) target)
            (send-discord-message-with-map-and-mark
             "player-alerts"
             (format nil "<@&~A> Spotted a ~A" role (describe target))
             (target-rc target)
             (target-angle target))
            (when (and (typep target 'boat)
                       (boat-sail target))
              (send-discord-image "player-alerts" "Custom boat sail" (boat-sail target))))))))
    ;; Cleanup old targets that are no longer spotted
    (loop
     for id being the hash-keys in spotted-gobs
     when (/= tick (target-tick (gethash id spotted-gobs)))
     do (let ((gob (gethash id spotted-gobs)))
          (remhash id spotted-gobs)
          (send-discord-message-with-map-and-mark
           "player-alerts"
           (format nil "<@&~A> Last spotted a ~A" role (describe gob))
           (target-rc gob)
           (target-angle gob))))))

(export '(scan-for-targets))
