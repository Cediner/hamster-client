(in-package :hafen-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Minimap API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant +minimap+ "haven.MapWnd")

(java-func +minimap+ minimap-mark-1 "mark" +string+ +color+ +coord2d+)
(java-func +minimap+ minimap-mark-2 "mark" +string+ +string+ +color+ +coord2d+)

(defmacro minimap-mark (name color position)
  `(minimap-mark-1 ,name ,color ,position))

(defmacro minimap-mark-with-icon (icon-name name color position)
  `(minimap-mark-2 ,icon-name ,name ,color ,position))

(export '(minimap-mark minimap-mark-with-icon))