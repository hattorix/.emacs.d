;;--------------------------------------------------
;; File name    :   linux-01-font.el
;;              :   emacs の font 設定
;;              :
;;--------------------------------------------------
;;
;=======================================================================
; フォント
;=======================================================================
(cond (window-system  ; emacs using GTK
       ;; フレームのデフォルトフォントを指定
       ;;(set-frame-font "Migu 1M-12:spacing=0")
       ;;(add-to-list 'default-frame-alist '(font . "Migu 1M-12:spacing=0"))
       ;;(set-fontset-font (frame-parameter nil 'font)
       ;;                  'katakana-jisx0201
       ;;                  (font-spec :family "あずきフォント"))
       ;;(set-fontset-font (frame-parameter nil 'font)
       ;;                  'japanese-jisx0208
       ;;                  (font-spec :family "あずきフォント"))

       ;; font-setを作成して、フレーム作成時のパラメータに設定する
       (let* ((size 16) ;フォントサイズ [9/10/12/14/15/17/19/20/...]
              (asciifont "DejaVu Sans Mono") ;fontset作成用のダミー
              (jpfont "Migu 1M")
              (xldf (format "%s-%d:weight=normal:slant=normal" asciifont size))
              (jp-fontspec (font-spec :family jpfont :size size :spacing 0))
              (fsn (create-fontset-from-ascii-font xldf nil "Migu")))
         ;; ASCII文字
         (set-fontset-font fsn 'ascii
                           jp-fontspec
                           nil 'prepend)
         (set-fontset-font fsn 'katakana-jisx0201
                           (font-spec :family "あずきフォント")
                           nil 'prepend)
         (set-fontset-font fsn 'japanese-jisx0208
                           (font-spec :family "あずきフォント")
                           nil 'prepend)

         ;; デフォルトのフレームパラメータでフォントセットを指定
         (add-to-list 'default-frame-alist '(font . "fontset-Migu"))

         ;; フォントサイズの比を設定
         ;;(add-to-list 'face-font-rescale-alist '(".*YukarryAA.*" . 1.12))

         ;; デフォルトフェイスにフォントセットを設定
         ;; # これは起動時に default-frame-alist に従ったフレームが
         ;; # 作成されない現象への対処
         ;;(set-face-font 'default "fontset-Migu")
         )))
