;;--------------------------------------------------
;; File name    :   40-mmm-mode.el
;;              :   Emacs の基本的な設定
;;              :
;;--------------------------------------------------
;;
;=======================================================================
; mmm-mode
; バッファ内で、複数のメジャーモードを共存
;
; - Project page
; http://mmm-mode.sourceforge.net/
;=======================================================================
;; mmm-mode
(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
(set-face-background 'mmm-default-submode-face nil)
;(set-face-background 'mmm-default-submode-face "gainsboro")
