; Key bindings
(global-set-key "\C-l" 'goto-line)
(global-set-key [f2] 'save-buffer)
(global-set-key [f3] 'shell)
(global-set-key [f4] 'indent-region)
; lusty
(global-set-key "\C-x\C-f" 'lusty-file-explorer)
(global-set-key (kbd "C-x b") 'lusty-buffer-explorer)
(global-set-key [f5] 'lusty-file-explorer)
(global-set-key [f6] 'lusty-buffer-explorer)
;		Copy and paste current line
(global-set-key [f7] "\C-a\C- \C-n\M-w\C-y")
;		Copy current line
(global-set-key [f8] "\C-a\C- \C-n\M-w")
; Move between buffers
(global-set-key [C-right] 'next-buffer)
(global-set-key [C-left] 'previous-buffer)
; Replace string
(global-set-key "\M-r" 'replace-string)
