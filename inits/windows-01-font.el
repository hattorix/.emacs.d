;;--------------------------------------------------
;; File name    :   windows-01-font.el
;;              :   NTEmacs の font 設定
;;              :
;;--------------------------------------------------
;;
;=======================================================================
; フォント
;=======================================================================
(cond (window-system
       ;; フレームのデフォルトフォントを指定
       (set-frame-font "ARISAKA-等幅-12")
       (add-to-list 'default-frame-alist '(font . "ARISAKA-等幅-12"))
       ;; 日本語フォントを指定
       (set-fontset-font (frame-parameter nil 'font)
                         'katakana-jisx0201
                         (font-spec :family "あずきフォント"))
       (set-fontset-font (frame-parameter nil 'font)
                         'japanese-jisx0208
                         (font-spec :family "あずきフォント"))))
