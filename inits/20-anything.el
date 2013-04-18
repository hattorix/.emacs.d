;;--------------------------------------------------
;; File name    :   20-anything.el
;;              :   anything 関連の設定
;;              :
;;--------------------------------------------------
;;
;=======================================================================
; anything.el
; - ファイルを開くを一元化
;
; - Project wiki
; http://www.emacswiki.org/emacs/Anything/
;=======================================================================
(add-to-list 'load-path (concat siteinit-path "elisp/anything") t)

(require 'anything-config)

;; 情報元を設定
(setq anything-sources (list anything-c-source-buffers
                             anything-c-source-bookmarks
                             anything-c-source-file-name-history
                             anything-c-source-man-pages
                             anything-c-source-info-pages
                             anything-c-source-complex-command-history))

;=======================================================================
; anything のキーマップ
;=======================================================================
(define-key anything-map "\C-p" 'anything-previous-line)
(define-key anything-map "\C-n" 'anything-next-line)
(define-key anything-map "\C-v" 'anything-next-page)
(define-key anything-map "\M-v" 'anything-previous-page)

;=======================================================================
; global key binding
;=======================================================================
;; anything の開始
(global-set-key "\C-xa" 'anything)
