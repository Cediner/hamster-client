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


(defstruct spotted-gob
  id
  name
  first-spotted-at
  avg-speed
  max-speed
  gear
  first-rc-at
  last-rc-at
  last-spotted
  last-alert
  last-angle
  tick)


(defun describe-gear (gear)
  (if gear
      (apply #'concatenate 'string
             (loop
              for equ in (listify gear)
              collect (format nil "~A~%" equ)))
      ""))

(defun scan-for-targets (role spotted-gobs tick)
  (let ((targets (gob-get-all-by-filter
                  (lambda (gob)
                    (or (member (gob-name gob) +targets+ :test 'equal)
                        (and (gob-has-tag gob +human+)
                             (not (is-gob-friendly gob))
                             (not (= (gob-id gob) (mv-plgob (mv))))))))))
    (loop
       for target in targets
       do (let ((name (if (gob-has-tag target +human+) (gob-kinname target) (gob-name target)))
                (gear (if (gob-has-tag target +human+) (gob-equipment target) nil)))
            (cond
              ((and (gethash (gob-id target) spotted-gobs)
                    (> (- (get-time)
                          (spotted-gob-last-alert (gethash (gob-id target) spotted-gobs)))
                       +alert-refresh+))
               (let ((gob (gethash (gob-id target) spotted-gobs)))
                 (setf (spotted-gob-last-spotted gob) (gob-rc target))
                 (setf (spotted-gob-last-rc-at gob) (gob-rc target))
                 (setf (spotted-gob-last-angle gob) (gob-angle target))
                 (setf (spotted-gob-avg-speed gob)
                       (/ (+ (spotted-gob-avg-speed gob) (gob-v target)) 2))
                 (setf (spotted-gob-max-speed gob)
                       (max (spotted-gob-avg-speed gob) (gob-v target)))
                 (setf (spotted-gob-tick gob) tick)
                 (setf (spotted-gob-last-alert gob) (get-time))
                 (send-discord-message-with-map-and-mark
                  "player-alerts" 
                  (format nil "<@&~A> Still spotted a ~A [~A] [Avg Speed ~A] [Max Speed ~A]"
                          role name (gob-id target)
                          (spotted-gob-avg-speed gob) (spotted-gob-max-speed gob))
                  (gob-rc target)
                  (gob-angle target))))
              ((gethash (gob-id target) spotted-gobs)
               (let ((gob (gethash (gob-id target) spotted-gobs)))
                 (setf (spotted-gob-last-spotted gob) (gob-rc target))
                 (setf (spotted-gob-last-rc-at gob) (gob-rc target))
                 (setf (spotted-gob-last-angle gob) (gob-angle target))
                 (unless (equalp (spotted-gob-gear gob) gear)
                   (setf (spotted-gob-gear gob) gear)
                   (send-discord-message
                    "player-alerts"
                    (format nil "<@&~A> Gob ~A [~A] is now wearing ~A~%"
                            role
                            (spotted-gob-name gob)
                            (spotted-gob-id gob)
                            (describe-gear (spotted-gob-gear gob)))))
                 (setf (spotted-gob-avg-speed gob)
                       (/ (+ (spotted-gob-avg-speed gob) (gob-v target)) 2))
                 (setf (spotted-gob-max-speed gob)
                       (max (spotted-gob-avg-speed gob) (gob-v target)))
                 (setf (spotted-gob-tick gob) tick)))
              (t ;;new target
               (let ((gob (make-spotted-gob :id (gob-id target)
                                            :name name
                                            :avg-speed (gob-v target)
                                            :max-speed (gob-v target)
                                            :gear gear
                                            :first-spotted-at (get-time)
                                            :first-rc-at (gob-rc target)
                                            :last-spotted (gob-rc target)
                                            :last-angle (gob-angle target)
                                            :last-alert (get-time)
                                            :tick tick)))
                 (setf (gethash (spotted-gob-id gob) spotted-gobs) gob)
                 (send-discord-message-with-map-and-mark
                  "player-alerts"
                  (format nil
                          (if gear
                              "<@&~A> Spotted a ~A [~A] going ~A tiles/s Wearing:~%~A"
                            "<@&~A> Spotted a ~A [~A] going ~A tiles/s ~A")
                          role
                          name
                          (gob-id target)
                          (spotted-gob-max-speed gob)
                          (describe-gear (spotted-gob-gear gob)))
                  (gob-rc target)
                  (gob-angle target)))))))))

(defun describe-missing-gob (gob)
  (let* ((first-pos (spotted-gob-first-rc-at gob))
         (last-pos (spotted-gob-last-rc-at gob))
         (real-loc (get-real-location-on-map (spotted-gob-last-spotted gob))))
    (format nil "~A [~A] [Avg Speed ~A] [Max Speed ~A] [Duration ~A]"
            (spotted-gob-name gob)
            (spotted-gob-id gob)
            (spotted-gob-avg-speed gob)
            (spotted-gob-max-speed gob)
            (- (get-time) (spotted-gob-first-spotted-at gob)))))

(defun check-if-any-spotted-have-left (role spotted-gobs tick)
  (loop
     for id being the hash-keys in spotted-gobs
     when (/= tick (spotted-gob-tick (gethash id spotted-gobs)))
     do (let ((gob (gethash id spotted-gobs)))
          (remhash id spotted-gobs)
          (let ((msg (describe-missing-gob gob)))
            (send-discord-message-with-map-and-mark
             "player-alerts"
             (format nil "<@&~A> Last spotted a ~A" role msg)
             (spotted-gob-last-rc-at gob)
             (spotted-gob-last-angle gob))))))

(export '(scan-for-targets
          describe-missing-gob
          spotted-gob
          check-if-any-spotted-have-left))
