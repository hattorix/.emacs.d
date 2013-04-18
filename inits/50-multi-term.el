;;
;=======================================================================
; multi-term.el
; - term の拡張
;
; - Project wiki
; http://www.emacswiki.org/emacs/MultiTerm/
;=======================================================================
(require 'multi-term)

(add-hook 'term-mode-hook
          '(lambda ()
             ;; 配色設定
             ;(setq term-default-fg-color "white")
             ;(setq term-default-bg-color "#222222")
             ;(setq ansi-term-color-vector [unspecified "#000000" "#FF0000" "#00FF00" "#AA5500" "#8080FF" "#AA00AA" "#00AAAA" "#AAAAAA"])

             ;; 実行する shell の設定
             (setq multi-term-program shell-file-name)

             ;; global-hl-line-mode を term-mode で無効にする
             (set (make-local-variable 'hl-line-range-function)
                  (lambda () '(0 . 0)))

             ;; キーバインド
             (setq term-bind-key-alist
                   '(
                     ("C-c C-c" . term-interrupt-subjob)
                     ("C-h" . term-send-backspace)
                     ("C-m" . term-send-raw)
                     ("C-r" . term-send-reverse-search-history)
                     ("C-y" . term-paste)
                     ("M-b" . term-send-backward-word)
                     ("M-d" . term-send-forward-kill-word)
                     ("M-f" . term-send-forward-word)
                     ("M-n" . next-line)
                     ("M-p" . previous-line)
                     ("M-r" . isearch-backward)
                     ("M-s" . isearch-forward)
                     ))
             ))
