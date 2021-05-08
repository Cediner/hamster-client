(in-package :hafen-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Discord 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(java-func +script+ script-send-discord-message "sendDiscordMessage" +string+ +string+)
(java-func +script+ script-send-discord-image "sendDiscordImage" +string+ +string+ +bufferedimage+)
(java-func +script+ script-send-discord-message-with-map-and-mark "sendDiscordMessageWithMapAndMark" +string+ +string+ +coord2d+ +double+)
(java-func +script+ script-start-discord-session "startDiscord" +string+)
(java-func +script+ script-end-discord "endDiscord")
(with-script-define send-discord-message script-send-discord-message channel message)
(with-script-define send-discord-image script-send-discord-image channel message img)
(with-script-define send-discord-message-with-map-and-mark script-send-discord-message-with-map-and-mark channel message markc angle)
(with-script-define start-discord-session script-start-discord-session token)
(with-script-define end-discord-session script-end-discord)

(defconstant +discord-bot-channel+ "bot-alerts")
(defconstant +discord-angler-channel+ "angler-alerts")

(defun prompt-for-discord-info ()
  (msg-listen)
  (widget-add (gui) (jnew "hamster.ui.script.DiscordHelper") (coord 50 50))
  (let ((token nil)
        (role nil))
    (loop
       until (and token role)
       do (progn
            (sleep 1)
            (loop
               while (and (msg-has-message)
                          (null token)
                          (null role))
               do (let ((msg (msg-poll-message)))
                    (when (string= "discord" (msg-subject msg))
                      (setf token (aref (msg-args msg) 0)
                            role (aref (msg-args msg) 1)))))))
    (msg-stop-listening)
    (msg-clear-messages)
    (values token role)))

(export '(send-discord-message send-discord-message-with-map-and-mark start-discord-session end-discord-session
          send-discord-image
          prompt-for-discord-info
          +discord-bot-channel+
		  +discord-angler-channel+))
