;;--------------------------------------------------
;; File name    :   init.el
;;              :   Emacs の基本的な設定
;;              :
;;--------------------------------------------------
;;
;=======================================================================
; このファイルが置かれているディレクトリ
;=======================================================================
(add-to-list 'load-path "~/.emacs.d")
(defvar siteinit-path (file-name-directory (locate-library "init")))

;;
;=======================================================================
; elisp の追加読み込み PATH
;=======================================================================
(add-to-list 'load-path (concat siteinit-path "elisp") t)
(add-to-list 'load-path (concat siteinit-path "elisp/init-loader") t)
(add-to-list 'load-path (concat siteinit-path "virtualenv") t)

;;
;=======================================================================
; Emacs Lisp Package Archive
;=======================================================================
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;
;=======================================================================
; 設定ファイル分割フレームワーク
;
; - Project page
; https://github.com/emacs-jp/init-loader
;=======================================================================
(require 'init-loader)
;(init-loader-load "~/.emacs.d/inits")
(init-loader-load)

;;
;=======================================================================
; powershell-mode.el
; - Windows PowerShell 編集用のメジャーモード
;
; - Emacs wiki
; http://www.emacswiki.org/emacs/PowerShell
;=======================================================================
(require 'powershell-mode)
(add-to-list 'auto-mode-alist '("\\.ps1$" . powershell-mode))

;;
;=======================================================================
; kill-summary.el
; - kill-ring の内容を一覧表示してそこから yank する
;
; - Project page
; http://emacs-session.sourceforge.net/
;=======================================================================
(autoload 'kill-summary "kill-summary" nil t)

;;
;=======================================================================
; dabbrev-ja.el
; - 日本語で dabbrev(動的略語補完) を使う
;=======================================================================
;(load "dabbrev-ja")

;;
;=======================================================================
; redo+.el
; - undo, redo を行えるようにする
;
; http://www11.atwiki.jp/s-irie/pages/18.html
;=======================================================================
;; 24.3 で動かなくなったのでコメントアウト
;(require 'redo+)
;(setq undo-no-redo t)
(global-set-key [f12] 'redo)                   ;redo

;;
;=======================================================================
; mercurial.el
; - Mercurial 付属の elisp
;
; C-c h h でキーバインディング一覧が見れる
;
; - 分かりやすい help
; http://www.lares.dti.ne.jp/~foozy/fujiguruma/scm/mercurial-emacs.html
;=======================================================================
(when (require 'mercurial nil t)
  ;; MQ (Mercurial Queue) で patch 管理をする
  (require 'mq nil t))

;;
;=======================================================================
; kanji-code.el
; - 日本語と Escaped Unicode を変換する
;=======================================================================
(autoload 'kanji-to-unicode-buffer "kanji-code" nil t)
(autoload 'kanji-to-unicode-region "kanji-code" nil t)
(autoload 'unicode-to-kanji-buffer "kanji-code" nil t)
(autoload 'unicode-to-kanji-region "kanji-code" nil t)

;;
;=======================================================================
; twittering-mode
;
; - Project wiki
; http://www.emacswiki.org/emacs/TwitteringMode/
;=======================================================================
(require 'twittering-mode)
(setq twittering-icon-mode t)                 ; Show icons

;;
;=======================================================================
; schema-mode using gauche
;=======================================================================
(setq scheme-program-name "gosh -i")
(autoload 'schema-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-mode "cmuscheme" "Run an inferior Scheme process." t)

(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (split-window-horizontally)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name)
  (other-window -1))

(global-set-key "\C-cs" 'scheme-other-window)  ;scheme実行

;;
;=======================================================================
; Customizer
;=======================================================================
;(global-set-key [f12] 'setnu-mode)             ;行番号表示
