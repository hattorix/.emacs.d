;;
;=======================================================================
; session.el
; - ミニバッファの履歴をファイルに保存し、次回起動時にも持ち越せる
;
; - Project page
; http://emacs-session.sourceforge.net/
;=======================================================================
(require 'session)
(setq session-initialize '(de-saveplace session keys menus places)
      session-globals-include '((kill-ring 50)
                                (session-file-alist 500 t)
                                (file-name-history 10000)))
;; これがないと file-name-history に500個保存する前に max-string に達する
(setq session-globals-max-string 100000000)
;; デフォルトでは30!
(setq history-length t)
(add-hook 'after-init-hook 'session-initialize)
