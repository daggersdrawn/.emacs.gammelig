;(require 'calfw-org)

(setq calendar-weekend-marker 'diary)
;(add-hook 'today-visible-calendar-hook 'calendar-mark-weekend)
;(add-hook 'today-invisible-calendar-hook 'calendar-mark-weekend)

(setq calendar-week-start-day 1
      calendar-day-name-array ["Domingo" "Lunes" "Martes" "Miércoles"
                               "Jueves" "Viernes" "Sábado"]
      calendar-month-name-array ["Enero" "Febrero" "Marzo" "Abril" "Mayo"
                                 "Junio" "Julio" "Agosto" "Septiembre"
                                     "Octubre" "Noviembre" "Diciembre"])

;; (cfw:open-calendar-buffer)
;; (cfw:contents-debug-data)

;; face
(custom-set-faces
 '(cfw:face-title ((t (:foreground "darkgoldenrod3" :weight bold :height 2.0 :inherit variable-pitch))))
 '(cfw:face-header ((t (:foreground "maroon2" :weight bold))))
 '(cfw:face-sunday ((t :foreground "red" :weight bold)))
 '(cfw:face-saturday ((t :foreground "blue" :weight bold)))
 '(cfw:face-holiday ((t :background "grey10" :foreground "purple" :weight bold)))
 '(cfw:face-default-content ((t :foreground "green2")))
 '(cfw:face-regions ((t :foreground "cyan")))
 '(cfw:face-day-title ((t :background "grey10")))
 '(cfw:face-today-title ((t :background "red4" :weight bold)))
 '(cfw:face-today ((t :foreground: "cyan" :weight bold)))
 '(cfw:face-select ((t :background "blue4")))
 '(cfw:face-grid ((t :foreground "DarkGrey")))
 '(cfw:face-default-day ((t :weight bold :inherit cfw:face-day-title)))
 '(cfw:face-annotation ((t :foreground "RosyBrown" :inherit cfw:face-day-title)))
 )
