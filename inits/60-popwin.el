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
(push '(compilation-mode :width 0.5 :position right) popwin:special-display-config)

;; auto-async-byte-compile
(push '(" *auto-async-byte-compile*" :width 0.5 :position right) popwin:special-display-config)
; or (setq auto-async-byte-compile-display-function 'popwin:popup-buffer-tail)
