;;--------------------------------------------------
;; File name    :   cocoa-emacs-02-ime.el
;;              :   Mac OS X での日本語入力設定
;;              :
;;--------------------------------------------------
;;
;=======================================================================
; システムの IME を使う
;=======================================================================
(setq default-input-method "MacOSX")
(add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)
