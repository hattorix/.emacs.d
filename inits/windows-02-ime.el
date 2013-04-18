;;--------------------------------------------------
;; File name    :   windows-01-font.el
;;              :   NTEmacs の基本的な設定
;;              :
;;--------------------------------------------------
;;
;=======================================================================
; Windows IME
; - Windows IME で日本語入力
;=======================================================================
(setq default-input-method "W32-IME")
(w32-ime-initialize)

;; IMEのON/OFFでカーソルの色を変える
(set-cursor-color "black")
(add-hook 'w32-ime-on-hook
          (function (lambda () (set-cursor-color "#FF9696"))))
(add-hook 'w32-ime-off-hook
          (function (lambda () (set-cursor-color "cyan2"))))

;; バッファ切り替え時にIME状態を引き継ぐ
(setq w32-ime-buffer-switch-p nil)
