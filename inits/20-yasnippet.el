;;--------------------------------------------------
;; File name    :   20-yasnippet.el
;;              :   yasnippet 関連の設定
;;              :
;;--------------------------------------------------
;;
;=======================================================================
; yasnippet.el
; - 入力支援
;
; - Project homepage
; https://github.com/capitaomorte/yasnippet
;=======================================================================
(add-to-list 'load-path (concat siteinit-path "elisp/yasnippet") t)

(require 'yasnippet)
(yas-global-mode 1)
;(yas/initialize)
