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
(add-to-list 'load-path (concat siteinit-path "elisp/init-loader") t)
(add-to-list 'load-path (concat siteinit-path "auto-complete") t)
(add-to-list 'load-path (concat siteinit-path "popwin") t)
(add-to-list 'load-path (concat siteinit-path "virtualenv") t)
(add-to-list 'load-path (concat siteinit-path "yasnippet") t)

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
; minibuf-isearch.el
; - ミニバッファで isearch 的な検索をする
;
; - Project page
; http://www.sodan.org/~knagano/emacs/minibuf-isearch/
;=======================================================================
(require 'minibuf-isearch)
;; minibuf-isearch 中は migemo を利用しない
(setq minibuf-isearch-use-migemo nil)

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
; tabbar.el
; - バッファリストのタブ表示
;
; - インストール
; # aptitude install emacs-goodies-el
;
; - Project wiki
; http://www.emacswiki.org/cgi-bin/wiki/TabBarMode
;=======================================================================
(when (require 'tabbar nil t)

  ;; タブのグループ化設定
  (setq tabbar-buffer-groups-function
        (lambda ()
          (cond
           ((eq major-mode 'dired-mode) '("Dired"))
           ((eq major-mode 'term-mode) '("Terminal"))
           (t (list "User Buffer")))))

  ;; `*scratch*' 以外の ` *'から始まるバッファーをリストしない
  (setq tabbar-buffer-list-function
        (lambda ()
          (remove-if
           (lambda (buffer)
             (unless (string-match-p "\\*\\(scratch\\|tmp\\|terminal\\)" (buffer-name buffer))
               (find (aref (buffer-name buffer) 0) " *")))
           (buffer-list))))

  ;; tabbar のバージョン違いごとの設定
  (cond ((string= tabbar-version "2.0")
         ;; Ubuntu 付属の tabbar
         ;;
         ;; 左端のボタンを無効化
         (setq tabbar-home-button nil)
         (setq tabbar-buffer-home-button nil)
         (setq tabbar-scroll-left-button nil)
         (setq tabbar-scroll-right-button nil)

         ;; 色設定
         (set-face-attribute 'tabbar-default nil :background "gray60")
         (set-face-attribute 'tabbar-unselected nil :background "gray85" :foreground "gray30" :box nil)
         (set-face-attribute 'tabbar-selected nil :background "#f2f2f6" :foreground "red" :box nil)
         (set-face-attribute 'tabbar-button nil :box '(:line-width 1 :color "gray72" :style released-button))

         ;; 幅設定
         (setq tabbar-separator (list 0.5)))

        (t
         ;; NTEmacs 用 (tabbar-version 1.3)
         ;;
         ;; 左端のボタンを無効化
         (setq tabbar-home-button-enabled "")
         (setq tabbar-scroll-right-button-enabled "")
         (setq tabbar-scroll-left-button-enabled "")
         (setq tabbar-scroll-right-button-disabled "")
         (setq tabbar-scroll-left-button-disabled "")

         ;; 色設定
         (set-face-attribute 'tabbar-default-face nil :background "gray60")
         (set-face-attribute 'tabbar-unselected-face nil :background "gray85" :foreground "gray30" :box nil)
         (set-face-attribute 'tabbar-selected-face nil :background "#f2f2f6" :foreground "red" :box nil)
         (set-face-attribute 'tabbar-button-face nil :box '(:line-width 1 :color "gray72" :style released-button))

         ;; 幅設定
         (set-face-attribute 'tabbar-separator-face nil :height 0.7)))

  ;; tabbar を有効にする
  (tabbar-mode))

;;
;=======================================================================
; yasnippet.el
; - 入力支援
;
; - Project homepage
; https://github.com/capitaomorte/yasnippet
;=======================================================================
(require 'yasnippet)
(yas/initialize)

;;
;;
;=======================================================================
; popwin.el
; - ヘルプバッファや補完バッファをポップアップで表示
;
; - Project page
; https://github.com/m2ym/popwin-el
;=======================================================================
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

;; M-x anything
(setq anything-samewindow nil)
(push '("*anything*" :height 20) popwin:special-display-config)

;; M-x dired-jump-other-window
(push '(dired-mode :position top) popwin:special-display-config)

;; M-!
(push "*Shell Command Output*" popwin:special-display-config)

;; M-x compile
(push '(compilation-mode :noselect t) popwin:special-display-config)

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

;;
;=======================================================================
; auto-complete.el
; - 自動保管
;
; - Project wiki
; http://www.emacswiki.org/emacs/AutoComplete
;=======================================================================
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             (concat siteinit-path "auto-complete/dict"))
(ac-config-default)

;; 4 文字以上の単語の時に補完を開始する
(setq ac-auto-start 4)
;; 補完の情報源
(setq ac-sources '(ac-source-yasnippet
                   ac-source-dictionary
                   ac-source-gtags
                   ac-source-words-in-buffer))

;; キーマップ
(define-key ac-completing-map "\M-/" 'ac-stop) ; 保管の停止

;;
;=======================================================================
; dsvn.el
; - Subversion
;
; - Source code
; http://svn.apache.org/repos/asf/subversion/trunk/contrib/client-side/emacs/
;=======================================================================
(autoload 'svn-status "dsvn" "Run `svn status'." t)
(autoload 'svn-update "dsvn" "Run `svn update'." t)

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
; スクロール設定
;=======================================================================
;; スクロールの基本設定
(setq scroll-conservatively 15                ;画面の下端 (上端) で移動したときのスクロール量
      ;; scroll-step 1                           ;(同上)
      scroll-preserve-screen-position t       ;カーソル位置を維持する (Page-Up, Page-Down とか)
      scroll-margin 0                         ;上下マージン
      )

;; ホイールマウスでスクロールを有効に
(mouse-wheel-mode t)

;; ホイールマウスのスクロール幅を設定（画面の８分の１）
(global-set-key [mouse-4] '(lambda () (interactive) (scroll-down (/ (window-height) 8))))
(global-set-key [mouse-5] '(lambda () (interactive) (scroll-up (/ (window-height) 8))))

;; スクロール時にカーソル位置を変えない
(setq scroll-preserve-screen-position t)

;; 画面スクロール時の重複行数
(setq next-screen-context-lines 1)

;; スクロールを加速させる
;(require 'scroll-speedup)

;;
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

;;
;=======================================================================
; function
;=======================================================================

;; カレントバッファを閉じる
(defun my-kill-buffer (all)
  (interactive "P")
  (if all
      ;prefix argument があれば全バッファを削除
      (loop for buffer being the buffers
            do (kill-buffer buffer))
    (kill-buffer nil)))

;; カレントバッファを閉じる
(defun my-revert-buffer (&optional coding-system)
  (interactive "zCoding system for visited file (default nil): \nP")
  (if coding-system
      ;prefix argument があればエンコード指定
      ;(revert-buffer-with-coding-system coding-system)
      t
    (revert-buffer t t)))

;; 作業用バッファを作る
(defvar tmp-buffer-count 0 "new bufferを作った数")
(defun create-tmp-buffer ()
  (interactive)
  (switch-to-buffer (if (= tmp-buffer-count 0)
                        "*tmp*"
                        ;TODO: 動的に数値を取得する
                        (concat "*tmp (" (number-to-string tmp-buffer-count) ")*")))
  (setq tmp-buffer-count (+ 1 tmp-buffer-count)))

;; カーソル位置の単語を検索
(defun search-word-cursor ()
  (interactive)
  (if (thing-at-point 'symbol)
      (occur (thing-at-point 'symbol))
    (call-interactively 'occur)))

;; カーソル位置の単語を削除
(defun kill-word-at-point ()
  (interactive)
  (let ((char (char-to-string (char-after (point)))))
    (cond
     ((string= " " char) (delete-horizontal-space))
     ((string-match "[\t\n -@\[-`{-~]" char) (kill-word 1))
     (t (forward-char) (backward-word) (kill-word 1)))))

;; 二分割されている画面を入れ替える
(defun swap-screen ()
  "Swap two screen,leaving cursor at current window."
  (interactive)
  (let ((thiswin (selected-window))
        (nextbuf (window-buffer (next-window))))
    (set-window-buffer (next-window) (window-buffer))
    (set-window-buffer thiswin nextbuf)))

;; 二分割されている画面、カーソルを入れ替える
(defun swap-screen-with-cursor ()
  "Swap two screen,with cursor in same buffer."
  (interactive)
  (let ((thiswin (selected-window))
        (thisbuf (window-buffer)))
    (other-window 1)
    (set-window-buffer thiswin (window-buffer))
    (set-window-buffer (selected-window) thisbuf)))

;; ウィンドウ二分割時に、縦分割<->横分割
(defun window-toggle-division ()
  "ウィンドウ 2 分割時に、縦分割<->横分割"
  (interactive)
  (unless (= (count-windows 1) 2)
    (error "ウィンドウが 2 分割されていません。"))
  (let (before-height (other-buf (window-buffer (next-window))))
    (setq before-height (window-height))
    (delete-other-windows)
    (if (= (window-height) before-height)
        (split-window-vertically)
      (split-window-horizontally)
      )
    (switch-to-buffer-other-window other-buf)
    (other-window -1)))

;; 対応する括弧に移動する
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
;        (t (self-insert-command (or arg 1)))))
        ))

;; タブ幅の設定
(defun set-tab-width (num)
  (interactive "nTab Width: ")
  (make-local-variable 'tab-stop-list)
  (setq tab-width num)
  (setq tab-stop-list ())
  (while (< num 128)
    (add-to-list 'tab-stop-list num t)
    (setq num (+ num tab-width))))

;; 縦横幅に応じて画面を分割
(defun split-window-conditional ()
  (interactive)
  (if (> (* (window-height) 2) (window-width))
      (split-window-vertically)
    (split-window-horizontally)))

;; ウィンドウが分割されていたら次のウィンドウに移動、そうでなければ分割する
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-conditional))
  (other-window 1))

;; 分割したウィンドウのリサイズ
(defun my-window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        action c)
    (catch 'end-flag
      (while t
        (setq action
              (read-key-sequence-vector (format "size[%dx%d]"
                                                (window-width)
                                                (window-height))))
        (setq c (aref action 0))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (let ((last-command-char (aref action 0))
                     (command (key-binding action)))
                 (when command
                   (call-interactively command)))
               (message "Quit")
               (throw 'end-flag t)))))))

;; 折り返し表示をトグルする
(defun toggle-truncate-lines ()
  "折り返し表示をトグルする."
  (interactive)
  (if truncate-lines
      (setq truncate-lines nil
            truncate-partial-width-windows nil)
    (setq truncate-lines t
          truncate-partial-width-windows t))
  (recenter))

;;----------------------------------------------
;; カーソル行を上下に移動
;;----------------------------------------------
; 下に移動
(defun transpose-lines-down (&optional n)
  "カーソル行を下に移動"
  (interactive "p")
  (if (not n)
      (setq n 1))
  (when (save-excursion (forward-line n))
    (let ((column (current-column))
          (beg (progn (move-beginning-of-line 1) (point)))
          (end (progn (forward-line 1) (point))))
      (insert (prog1
                  (buffer-substring beg end)
                (delete-region beg end)
                (forward-line n)))
      (forward-line -1)
      (move-to-column column))))

; 上に移動
(defun transpose-lines-up (&optional n)
  "カーソル行を上に移動"
  (interactive "p")
  (if (not n)
      (setq n 1))
  (transpose-lines-down (- n)))

;;
;=======================================================================
; キーカスタマイズ
;=======================================================================

(global-set-key "\C-cl" 'toggle-truncate-lines);折り返し表示のトグル
(global-set-key "\C-h" 'backward-delete-char)  ;バックスペース
;(global-set-key "\C-i" 'auto-complete)         ;文字列保管
(global-set-key [zenkaku-hankaku]
                'toggle-input-method)          ;日本語入力
(global-set-key "\C-o" 'toggle-input-method)   ;日本語入力
(global-set-key "\C-s" 'isearch-forward-regexp);正規表現で検索
(global-set-key "\C-r" 'query-replace-regexp)  ;正規表現で置換
(global-set-key "\C-x\C-g" 'my-revert-buffer)  ;カレントバッファを再読み込み
(global-set-key "\C-xk" 'my-kill-buffer)       ;カレントバッファを閉じる
(global-set-key "\C-xt" 'create-tmp-buffer)    ;作業用バッファを作る
(global-set-key "\C-x\C-b" 'buffer-menu)       ;ウィンドウ分割しないバッファメニュー
(global-set-key "\C-\\" 'undo)                 ;undo
(global-set-key [?\C-:] 'search-word-cursor)   ;カーソル下の単語で検索
(global-set-key "\C-]" 'match-paren)           ;対応する括弧に移動
(global-set-key "\C-\M-y" 'kill-summary)       ;kill-ring 一覧から yank
(global-set-key "\M-k" '(lambda () (interactive) (kill-line 0))) ;;行頭まで削除
;(global-set-key "\M-d" 'kill-word-at-point)    ;カーソル下の単語を削除
(global-set-key "\M-g" 'goto-line)             ;指定行へ移動
;; 半ページ/スクロール
(global-set-key "\M-]" '(lambda () (interactive) (scroll-up (/ (window-height) 2))))
(global-set-key "\M-[" '(lambda () (interactive) (scroll-down (/ (window-height) 2))))

(global-set-key [M-up] 'transpose-lines-up)    ;カーソル行を上に移動
(global-set-key [M-down] 'transpose-lines-down);カーソル行を下に移動
(global-set-key [f1] 'help-for-help)           ;ヘルプ
(global-set-key [f2] 'tabbar-backward-tab)     ;前のタブへ
(global-set-key [S-f2] 'tabbar-backward-group) ;前のタブグループへ
(global-set-key [f3] 'tabbar-forward-tab)      ;次のタブへ
(global-set-key [S-f3] 'tabbar-forward-group)  ;次のタブグループへ
(global-set-key [f7] 'next-error)              ;次のエラーを検索
(global-set-key [S-f7] 'previous-error)        ;前のエラーを検索
(global-set-key [f10] 'gtags-find-tag-from-here);タグジャンプ
(global-set-key [S-f10] 'gtags-pop-stack)      ;バックタグジャンプ
(global-set-key [f12] 'redo)                   ;redo
;(global-set-key [f12] 'setnu-mode)             ;行番号表示
(global-set-key [C-prior] 'tabbar-backward-tab);前のタブへ
(global-set-key [C-next] 'tabbar-forward-tab)  ;次のタブへ
(global-set-key [C-tab] 'other-window-or-split);次のウィンドウか分割か

(global-set-key "\C-cs" 'scheme-other-window)  ;scheme実行
(global-set-key "\C-ct" 'multi-term)           ;ターミナルを開く
(global-set-key "\C-cr" 'my-window-resizer)    ;ウィンドウのリサイズ

(put 'set-goal-column 'disabled nil)

;;
;=======================================================================
; Customizer
;=======================================================================
