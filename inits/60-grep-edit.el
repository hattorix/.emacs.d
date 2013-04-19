;;
;=======================================================================
; grep-edit.el
; - grep の検索結果を直接編集する
;
; - Project homepage
; http://www.bookshelf.jp/soft/meadow_51.html#SEC782
;=======================================================================
;; M-x grep 後バッファを編集
;;   - C-c C-c で変更を適用 (or C-c C-e)
;;   - C-c C-u で変更の破棄
;;   - C-c C-r でリージョン内の変更の破棄
(require 'grep-edit)

(defun my-grep-edit-setup ()
  ;; (define-key grep-mode-map '[up] nil)
  (define-key grep-mode-map "\C-c\C-c" 'grep-edit-finish-edit)
  (message (substitute-command-keys "\\[grep-edit-finish-edit] to apply changes."))
  (set (make-local-variable 'inhibit-read-only) t)
  )
(add-hook 'grep-setup-hook 'my-grep-edit-setup t)
