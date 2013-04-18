;;
;=======================================================================
; popwin.el
; - ヘルプバッファや補完バッファをポップアップで表示
;
; - Project page
; https://github.com/m2ym/popwin-el
;=======================================================================
(add-to-list 'load-path (concat siteinit-path "elisp/popwin") t)

(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

;; M-x anything
(setq anything-samewindow nil)
(push '("*anything*" :height 20) popwin:special-display-config)

;; M-x dired-jump-other-window
(push '(dired-mode :position top) popwin:special-display-config)

;; M-!
(push "*Shell Command Output*" popwin:special-display-config)

;; M-x compile
(push '(compilation-mode :noselect t) popwin:special-display-config)
