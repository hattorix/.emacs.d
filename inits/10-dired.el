;=======================================================================
; dired
;=======================================================================

;; 二画面モード
(setq dired-dwim-target t)
;; コピー、削除は再帰的に
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
;; ディレクトリを先頭に表示させる
(setq dired-listing-switches "-lh --group-directories-first")
;; dired-find-alternate-fileを有効に
(put 'dired-find-alternate-file 'disabled nil)

;; r -> C-x C-s でファイル名の編集
(require 'wdired)

;; スペースでマークする (FD like)
(defun dired-toggle-mark (arg)
  "Toggle the current (or next ARG) files."
  ;; S.Namba Sat Aug 10 12:20:36 1996
  (interactive "P")
  (let ((dired-marker-char
         (if (save-excursion (beginning-of-line)
                             (looking-at " "))
             dired-marker-char ?\040)))
    (dired-mark arg)
    ;(dired-previous-line 1)
    ))

;; dired-find-alternate-file の親ディレクトリ移動版
(defun dired-discard-up-directory ()
  (interactive)
  (find-alternate-file ".."))

(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode) ;rで編集モードに
(define-key dired-mode-map " " 'dired-toggle-mark)            ;スペースでマークをトグル
;(define-key dired-mode-map [return] 'dired-find-alternate-file)
(define-key dired-mode-map "\C-h" 'dired-up-directory)
(define-key dired-mode-map [tab] 'other-window-or-split)
