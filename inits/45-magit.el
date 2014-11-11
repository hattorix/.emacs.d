;;
;=======================================================================
; magit.el
; - Git
;
; - Project page
; http://philjackson.github.com/magit/
;=======================================================================
(add-to-list 'load-path (concat siteinit-path "elisp/git-modes") t)
(add-to-list 'load-path (concat siteinit-path "elisp/magit") t)

(eval-after-load 'info
  '(progn (info-initialize)
          (add-to-list 'Info-directory-list (concat siteinit-path "elisp/magit"))))
(autoload 'magit-status "magit" nil t)

;; vc-mode 無効化
;; - http://bit.ly/xPV6eW
(eval-after-load "vc" '(remove-hook 'find-file-hooks 'vc-find-file-hook))

;;
;=======================================================================
; key binding
;=======================================================================
(global-set-key "\C-cg" 'magit-status)         ;magit実行
