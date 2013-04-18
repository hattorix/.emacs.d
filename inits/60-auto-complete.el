;;
;=======================================================================
; auto-complete.el
; - 自動保管
;
; - Project wiki
; http://www.emacswiki.org/emacs/AutoComplete
;=======================================================================
(add-to-list 'load-path (concat siteinit-path "elisp/auto-complete") t)
(add-to-list 'load-path (concat siteinit-path "elisp/popup") t)

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             (concat siteinit-path "elisp/auto-complete/dict"))
(ac-config-default)

;; 4 文字以上の単語の時に補完を開始する
(setq ac-auto-start 4)
;; 補完の情報源
(setq ac-sources '(ac-source-yasnippet
                   ac-source-dictionary
                   ac-source-gtags
                   ac-source-words-in-buffer))

;=======================================================================
; auto-complete のキーマップ
;=======================================================================
(define-key ac-completing-map "\M-/" 'ac-stop) ; 保管の停止

;=======================================================================
; global key binding
;=======================================================================
;(global-set-key "\C-i" 'auto-complete)         ;文字列保管
