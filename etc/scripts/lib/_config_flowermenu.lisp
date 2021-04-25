(in-package :hafen-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Flower Menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(java-func +session+ session-flowermenu "getFlowermenu")
;; Field: flowermenu-options
;; Desc: Get the Petal options from the flowermenu
;; Type: Petal[]
(java-field flowermenu-options "opts")
;; Field: flowermenu-petal-name
;; Desc: Get the name of the given Petal
;; Type: String
(java-field flowermenu-petal-name "name")
;; Field: flowermenu-petal-num
;; Desc: Get the id of the given Petal
;; Type: int
(java-field flowermenu-petal-num "num")
(with-session-define flowermenu session-flowermenu)
;; Func: wait-for-flowermenu
;; Desc: Waits for the flowermenu to be created up to 3s
;; Inputs: N/A
;; Return: N/A
(defmacro wait-for-flowermenu ()
  `(wait-until (lambda () (flowermenu)) :timeout 3000))
;; Func: flowermenu-select
;; Desc: Select a given flowermenu option by id
;; Inputs:
;;   1) int - Petal id
;; Return: N/A
(defmacro flowermenu-select (ind)
  `(wdgmsg (flowermenu) "cl" ,ind))
;; Func: flowermenu-select-by-name
;; Desc: Selects a flowermenu Petal based off name
;; Inputs:
;;   1) String - Petal name
;; Return: boolean (success / failure)
(defun flowermenu-select-by-name (name)
  (doarr (petal (flowermenu-options (flowermenu)))
    (when (string= (flowermenu-petal-name petal) name)
      (flowermenu-select (flowermenu-petal-num petal))
      (return-from flowermenu-select-by-name t)))
  nil)
;; Func: flowermenu-get-petal-name
;; Desc: Gets the name of the petal for the given id
;; Inputs:
;;   1) int - Petal id
;; Return: String
(defun flowermenu-get-petal-name (ind)
  (doarr (petal (flowermenu-options (flowermenu)))
    (when (= (flowermenu-petal-num petal) ind)
      (return-from flowermenu-get-petal-name (flowermenu-petal-name petal))))
  nil)

(java-func +gui+ gui-fm-auto-select-1 "setFmAutoSelectOpt" +string+)
(java-func +gui+ gui-fm-override-settings-1 "setFmOverrideSettings" +boolean+)
;; Func: flowermenu-set-auto-select-opt
;; Desc: Sets the flowermenu auto select opt which will automatically select once the flowermenu wdg is created
;; Inputs:
;;   1) String - Name of the option to auto select
;; Return: N/A
(defun flowermenu-set-auto-select-opt (name)
  (gui-fm-auto-select-1 (gui) name))
;; Func: flowermenu-override-settings
;; Desc: Sets the flowermenu override settings to ignore any settings set by the user on the next flowermenu wdg created
;; Inputs:
;;   1) boolean - Whether or not to turn this one
;; Return: N/A
(defun flowermenu-override-settings (val)
  (gui-fm-override-settings-1 (gui) val))

(export '(flowermenu flowermenu-set-auto-select-opt flowermenu-override-settings
          wait-for-flowermenu flowermenu-select flowermenu-select-by-name flowermenu-get-petal-name))
