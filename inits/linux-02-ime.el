;;--------------------------------------------------
;; File name    :   emacs23.el
;;              :   emacs23 の基本的な設定
;;              :
;;--------------------------------------------------
;;
;=======================================================================
; iiimecf
; - ATOK X3 for Linux
;
; - Project homepage
; http://www.meadowy.org/~kawabata/iiimecf/
;=======================================================================
;(add-to-list 'load-path (concat siteinit-path "iiimecf") t)

(setq iiimcf-server-control-hostlist (list (concat "/tmp/.iiim-" (user-login-name) "/:0.0")))
(setq use-atokx3 (if (= 0 (shell-command
                       (concat "netstat --unix -l | grep -q " (car iiimcf-server-control-hostlist))))
      (require 'iiimcf-sc nil t)))

(when use-atokx3
  (setq iiimcf-server-control-default-language "ja")
  (setq iiimcf-server-control-default-input-method "atokx3")
  (setq default-input-method 'iiim-server-control))

;;
;=======================================================================
; Mozc
; - Mozc で日本語入力
;
; - Project homepage
; # http://code.google.com/p/mozc/
;=======================================================================
(defvar use-mozc nil)
(when (not use-atokx3)
  (when (require 'mozc nil t)
    (setq default-input-method "japanese-mozc")
    (setq use-mozc t)

    ;; emacs24 だと、オーバーレイ表示が上手くいかない問題への対応
    (if (> emacs-major-version 23)
      (setq mozc-candidate-style 'echo-area))
    ))

;;
;=======================================================================
; Anthy
; - anthy で日本語入力
;
; - インストール
; # aptitude install anthy-el
;=======================================================================
(when (and (not use-atokx3) (not use-mozc))
  (require 'anthy)
  (setq default-input-method "japanese-anthy")

  ;; [shift]+[space]で半角スペースを入力
  (define-key anthy-mode-map [?\S-\ ]
    (lambda () (interactive)
      (let ((anthy-wide-space " "))
        (anthy-handle-key 32 32))))

  ;; Emacs23で、変換候補の表示が遅いことへの対処
  (if (>= emacs-major-version 23)
      (setq anthy-accept-timeout 1))

  ;; 入力のモード別にカーソルの色を変える
  (defvar anthy-cursor-color-alist
    '(("hiragana" . "#FF9696");"deep pink")
      ("katakana" . "goldenrod")
      ("alphabet" . "royalblue")
      ("walphabet" . "orchid")
      ("hankaku_kana" . "lime green")))

  (defun anthy-adjust-cursor-color ()
    (set-cursor-color
     (if anthy-minor-mode
         (cdr (assoc anthy-current-rkmap anthy-cursor-color-alist))
       ;;(frame-parameter nil 'foreground-color))))
       "cyan2")))

  (add-hook 'post-command-hook 'anthy-adjust-cursor-color)

  ;; 日本語でインクリメンタルサーチができない問題への対処
  (defvar anthy-preedit-empty-hook nil)

  (defadvice anthy-insert (after anthy-preedit-empty-hook () activate)
    (if (string= anthy-preedit "")
        (run-hooks 'anthy-preedit-empty-hook)))

  (defun anthy-isearch-process-commit-string ()
    (if preedit
        (setq unread-command-events
              (cons ?\C-g (cons last-command-event unread-command-events)))))

  (defadvice isearch-process-search-multibyte-characters
    (around anthy-isearch-process-search-characters () activate)
    (if (and (string= current-input-method "japanese-anthy")
             (eq this-command 'isearch-printing-char))
        (let ((overriding-terminal-local-map nil)
              (prompt (isearch-message-prefix))
              (minibuffer-local-map isearch-minibuffer-local-map)
              (anthy-preedit-empty-hook '(exit-minibuffer))
              (anthy-commit-hook '(anthy-isearch-process-commit-string))
              str junk-hist)
          (setq unread-command-events (cons last-char unread-command-events)
                str (substring
                     (read-string prompt isearch-string 'junk-hist nil t)
                     (length isearch-string)))
          (if (and str (> (length str) 0))
              (let ((unread-command-events nil))
                (isearch-process-search-string str str))
            (isearch-update)))
      ad-do-it)))
