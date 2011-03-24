;; Setting CLISP for SLIME. 
;; Don't forget to install 'clisp' binary on your system:
;; Ex: $ sudo yum install clisp
;; Instead of using the following line, you can execute in your
;; buffer:
;;      C-u M-x slime 
;; and then:
;;      clisp -K full
(setq inferior-lisp-program "clisp -K full")
