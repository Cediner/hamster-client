

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Color API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant +color-black+ (svar +color+ "BLACK"))
(defconstant +color-blue+ (svar +color+ "BLUE"))
(defconstant +color-cyan+ (svar +color+ "CYAN"))
(defconstant +color-gray+ (svar +color+ "GRAY"))
(defconstant +color-green+ (svar +color+ "GREEN"))
(defconstant +color-magenta+ (svar +color+ "MAGENTA"))
(defconstant +color-orange+ (svar +color+ "ORANGE"))
(defconstant +color-pink+ (svar +color+ "PINK"))
(defconstant +color-red+ (svar +color+ "RED"))
(defconstant +color-white+ (svar +color+ "WHITE"))
(defconstant +color-yellow+ (svar +color+ "YELLOW"))
(java-func +int+ color-red "getRed")
(java-func +int+ color-green "getGreen")
(java-func +int+ color-blue "getBlue")
(java-func +int+ color-alpha "getAlpha")

(java-func +color+ color-darker "darker")
(java-func +color+ color-brighter "brighter")

(defmacro color (r g b a)
  `(jnew +coclor+ r g b a))

(export '(+color-black+ +color-blue+ +color-cyan+ +color-gray+ +color-green+
          +color-magenta+ +color-orange+ +color-pink+ +color-red+ +color-white+
          +color-yellow+
          color-red color-green color-blue color-alpha color-darker color-brighter
          color))