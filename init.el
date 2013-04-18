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
(add-to-list 'load-path (concat siteinit-path "anything") t)
(add-to-list 'load-path (concat siteinit-path "auto-complete") t)
(add-to-list 'load-path (concat siteinit-path "coffee-mode") t)
(add-to-list 'load-path (concat siteinit-path "magit") t)
(add-to-list 'load-path (concat siteinit-path "mmm-mode") t)
(add-to-list 'load-path (concat siteinit-path "popwin") t)
(add-to-list 'load-path (concat siteinit-path "scala-mode") t)
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
;=======================================================================
; elisp の自動バイトコンパイル
;=======================================================================
(require 'auto-async-byte-compile)
;; 無視リスト
(setq auto-async-byte-compile-exclude-files-regexp "/junk/")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

;;
;=======================================================================
; mmm-mode
; バッファ内で、複数のメジャーモードを共存
;
; - Project page
; http://mmm-mode.sourceforge.net/
;=======================================================================
;; mmm-mode
(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
(set-face-background 'mmm-default-submode-face nil)
;(set-face-background 'mmm-default-submode-face "gainsboro")


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
; flymake.el
; - 構文チェック
;=======================================================================
(require 'flymake)
(require 'rfringe)

;; GUI の警告表示を無効
(setq flymake-gui-warnings-enabled nil)

;; 色設定
(set-face-background 'flymake-errline "red4")
(set-face-background 'flymake-warnline "dark slate blue")

;; flymake を有効にするファイル名のマスク
;; - コマンド無い時にサブプロセスを kill する確認がうざいから、
;;   自分で設定するようにする
(setq flymake-allowed-file-name-masks '(()))

;; ファイル名が有効の時に flymake-mode にする関数
(defun flymake-mode-if-enable-buffer ()
  (if (not (null buffer-file-name)) (flymake-mode)))

;; 汎用 flymake ルール
(defun flymake-simple-generic-init (cmd &optional opts)
  (let* ((temp-file  (flymake-init-create-temp-buffer-copy
                      'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list cmd (append opts (list local-file)))))

;; flymake のエラー/警告をミニバッファに表示
(defun credmp/flymake-display-err-minibuf ()
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count               (length line-err-info-list)))
    (while (> count 0)
      (when line-err-info-list
        (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)))
      (setq count (1- count)))))

;; flymake の一時ファイルに、システムの tmp ディレクトリを指定する関数
(defun flymake-create-temp-intemp (file-name prefix)
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((name (concat
                (file-name-nondirectory
                 (file-name-sans-extension file-name))
                "_" prefix))
         (ext  (concat "." (file-name-extension file-name)))
         (temp-name (make-temp-file name nil ext))
         )
    (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
    temp-name))

;-----------------------------------------------
; C, C++ で flymake を使う
;-----------------------------------------------

;; make 用の設定
(defun flymake-get-make-cmdline (source base-dir)
  (list "make"
        (list "-s"
              "-C" base-dir
              "LANG=C"               ; 警告を英語表示 (warning) させる
              (concat "CHK_SOURCES=" source)
              "SYNTAX_CHECK_MODE=1"
              "check-syntax")))

(defun flymake-simple-make-or-generic-init (cmd &optional opts)
  (if (file-exists-p "Makefile")
      (flymake-simple-make-init)
    (flymake-simple-generic-init cmd opts)))

;; C 用の設定
(defun flymake-c-init ()
  (flymake-simple-make-or-generic-init
   "gcc" '("-Wall" "-Wextra" "-pedantic" "-fsyntax-only")))

;; C++ 用の設定
(defun flymake-cc-init ()
  (flymake-simple-make-or-generic-init
   "g++" '("-Wall" "-Wextra" "-fsyntax-only")))

(add-hook 'c-mode-common-hook 'flymake-mode-if-enable-buffer)
(push '("\\.c$" flymake-c-init) flymake-allowed-file-name-masks)
(push '("\\.\\([ch]pp\\|cc\\|h\\|cxx\\)$" flymake-cc-init) flymake-allowed-file-name-masks)
(push '("\\(.+\\):\\([0-9]+\\):\\([0-9]+\\): \\(\\(エラー\\|警告\\):[ \t\n]*\\(.+\\)\\)" 1 2 nil 4) flymake-err-line-patterns)

;-----------------------------------------------
; python で flymake を使う
;-----------------------------------------------
(when (executable-find "pyflakes")
  (defun flymake-python-init ()
    (flymake-simple-generic-init
     "pyflakes"))

  (add-hook 'python-mode-hook 'flymake-mode-if-enable-buffer)
  (push '("\\.py$" flymake-python-init) flymake-allowed-file-name-masks)
  (push '("^\\([^:]+\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)
  )

;-----------------------------------------------
; ruby で flymake を使う
;-----------------------------------------------
(when (executable-find "ruby")
  (defun flymake-ruby-init ()
    (flymake-simple-generic-init
     "ruby" '("-c")))

  (add-hook 'ruby-mode-hook 'flymake-mode-if-enable-buffer)
  (push '("\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
  (push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
  ;; flymake-err-line-patterns は python の設定で吸収
  )

;-----------------------------------------------
; Scala で flymake を使う
;-----------------------------------------------
(when (executable-find "scalac")
  (defun flymake-scala-init ()
    (flymake-simple-generic-init
     "scalac" (list "-d" temporary-file-directory)))

  (push '("\\.scala$" flymake-scala-init) flymake-allowed-file-name-masks)
  ;; flymake-err-line-patterns は python の設定で吸収
  )

;-----------------------------------------------
; PHP で flymake を使う
;-----------------------------------------------
(when (executable-find "php")
  (add-hook 'php-mode-hook 'flymake-mode-if-enable-buffer)
  (push '("\\.php[345]?$" flymake-php-init) flymake-allowed-file-name-masks)
  )

;-----------------------------------------------
; CoffeeScript で flymake を使う
;-----------------------------------------------
(when (executable-find "coffeelint")
  (defun flymake-coffeescript-init ()
    (flymake-simple-generic-init
     ;; なぜか動かない
     ;;"coffeelint" (append (if (file-exists-p "~/.coffeelint.conf")
     ;;                       '("-f" "~/.coffeelint.conf"))
     ;;                     (list "--csv"))))
     "coffeelint" '("--csv")))

  (add-hook 'coffee-mode-hook 'flymake-mode-if-enable-buffer)
  (push '("\\.coffee$" flymake-coffeescript-init) flymake-allowed-file-name-masks)
  (push '("^\\([^,]+\\),\\([0-9]+\\),\\(\\(warn\\|error\\),\\(.*\\)\\)$" 1 2 nil 3) flymake-err-line-patterns)
  )

;;
;=======================================================================
; gdb-mode.el
; - デバッガ
;=======================================================================
;; いろんな情報を表示するバッファを開く
(setq gdb-many-windows t)

;; I/O バッファを開くかどうか
(setq gdb-use-separate-io-buffer t)

;;
;=======================================================================
; scala-mode.el
; - Scala プログラミング用のメジャーモード
;=======================================================================
(require 'scala-mode-auto)

;;
;=======================================================================
; csharp-mode.el
; - C# プログラミング用のメジャーモード
;
; - Emacs wiki
; http://www.emacswiki.org/emacs/CSharpMode
;=======================================================================
(require 'csharp-mode)

;=======================================================================
; python-mode.el
; - Python プログラミング用のメジャーモード
;=======================================================================
(add-hook 'python-mode-hook
          '(lambda()
             ;; インデント サイズの設定
             (setq tab-width 2
                   python-indent 2
                   indent-level 2
                   indent-tabs-mode nil)))

;; virtualenv に対応させる
(require 'virtualenv)
;; virtualenv-minor-mode 開始時に、python shell を起動するかどうか
(setq virtualenv-workon-starts-python nil)

;;
;=======================================================================
; ruby-mode.el
; - ruby プログラミング用のメジャーモード
;
; - インストール
; # aptitude install ruby-elisp
;
; - Project homepage
; http://www.ruby-lang.org/ja/
;=======================================================================
(when (locate-library "ruby-mode")
  ;; ruby-mode
  (autoload 'ruby-mode "ruby-mode" nil t)

  (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))

  ;; ruby-electric - 閉じ括弧とか end を保管する
  (when (locate-library "ruby-electric")
    (require 'ruby-electric)
    (add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t))))

  ;; inf-ruby - irb 実行環境
  (when (locate-library "inf-ruby")
    (autoload 'run-ruby "inf-ruby" nil)
    (autoload 'inf-ruby-keys "inf-ruby" nil)
    (add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys))))

  ;; rubydb - ruby デバッガ
  (when (locate-library "rubydb3x")
    (autoload 'rubydb "rubydb3x" nil t))
  )

;;
;=======================================================================
; php-mode.el
; - php プログラミング用のメジャーモード
;
; - Project homepage
; http://sourceforge.net/projects/php-mode/
;=======================================================================
(require 'php-mode)

(add-hook 'php-mode-hook
          '(lambda ()
             ;; インデントスタイルの設定(gnu,bsd,k&r,stroustrup,linux,java)
             (c-set-style "stroustrup")
             ;; 連続する空白の一括削除
             (c-toggle-hungry-state t)
             ;; コメント行のインデント
             (setq c-comment-only-line-offset 0)
             ;; コメントのスタイル
             (setq comment-start "// "
                   comment-end   ""
                   comment-start-skip "// *")
             ;; `;' を入力したら、自動改行+インデント
             (c-toggle-auto-hungry-state 1)
             ;; インデント サイズの設定
             (setq tab-width 4
                   c-basic-offset 4
                   c-hanging-comment-ender-p nil
                   indent-tabs-mode nil)))

(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;;
;=======================================================================
; js2-mode.el
; - java-script プログラミング用のメジャーモード
;
; - Project homepage
; http://code.google.com/p/js2-mode/
;=======================================================================
(autoload 'js2-mode "js2" nil t)

;; インデントの関数の再設定
(add-hook 'js2-mode-hook
          #'(lambda ()
              (require 'js)

              ;; インデントスタイルを Google JavaScript Style に合わせる
              (set (make-local-variable 'indent-line-function) 'js-indent-line)

              ;; インデント サイズの設定
              (setq js-indent-level 2
                    js-expr-indent-offset 2
                    indent-tabs-mode nil)))

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;
;=======================================================================
; coffee-mode.el
; - coffee-script プログラミング用のメジャーモード
;
; - Project homepage
; https://github.com/defunkt/coffee-mode
;=======================================================================
(when (require 'coffee-mode nil t)
  ;; coffee-mode
  (add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
  (add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

  (defun coffee-custom ()
    "coffee-mode-hook"

    ;; CoffeeScript uses two spaces.
    (make-local-variable 'tab-width)
    (set 'tab-width 2)

    ;; If you don't have js2-mode
    ;(setq coffee-js-mode 'javascript-mode)

    ;; If you don't want your compiled files to be wrapped
    (setq coffee-args-compile '("-c" "--bare"))

    ;; *Messages* spam
    (setq coffee-debug-mode t)

    ;; Emacs key binding
    (define-key coffee-mode-map "\M-r" 'coffee-compile-buffer)

    ;; Riding edge.
    (setq coffee-command "coffee")

    ;; Compile '.coffee' files on every save
    (and (file-exists-p (buffer-file-name))
         (file-exists-p (coffee-compiled-file-name))
         (coffee-cos-mode t)))

  (add-hook 'coffee-mode-hook 'coffee-custom)
  )

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
; cmake-mode.el
; - CMake ファイル編集用のメジャーモード
;=======================================================================
(when (require 'cmake-mode nil t)

   (add-to-list 'auto-mode-alist '("CMakeLists\\.txt$" . cmake-mode))
   (add-to-list 'auto-mode-alist '("\\.cmake$" . cmake-mode))
  )

;;
;=======================================================================
; markdown-mode.el
; - Markdown ファイル編集用のメジャーモード
;
; - Project page
; http://jblevins.org/projects/markdown-mode/
;=======================================================================
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;;
;=======================================================================
; csv-mode
; - CSV ファイル編集用のメジャーモード
;
; - Emacs wiki
; http://emacswiki.org/emacs/CsvMode
;=======================================================================
(require 'csv-mode)
(add-to-list 'auto-mode-alist '("\\.csv$" . csv-mode))

;;
;=======================================================================
; pkgbuild-mode.el
; - PKGBUILD ファイル編集用のメジャーモード
;
; - Project page
; https://github.com/juergenhoetzel/pkgbuild-mode
;=======================================================================
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode))
                              auto-mode-alist))

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
             (setq term-default-fg-color "white")
             (setq term-default-bg-color "#222222")
             (setq ansi-term-color-vector [unspecified "#000000" "#FF0000" "#00FF00" "#AA5500" "#8080FF" "#AA00AA" "#00AAAA" "#AAAAAA"])

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

;;
;=======================================================================
; gtags.el
; - GNU Global を使ったタグジャンプ
;
; - Project page
; http://www.gnu.org/software/global/
;=======================================================================
(when (locate-library "gtags")
  (require 'gtags))

;; *GTAGS SELECT* バッファを一つしか生成しないようにする
(setq gtags-select-buffer-single t)

;; gtags を使用するモード
(add-hook 'java-mode-hook (lambda () (gtags-mode 1)))
(add-hook 'c-mode-hook (lambda () (gtags-mode 1)))
(add-hook 'c++-mode-hook (lambda () (gtags-mode 1)))

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
;=======================================================================
; anything.el
; - ファイルを開くを一元化
;
; - Project wiki
; http://www.emacswiki.org/emacs/Anything/
;=======================================================================
(require 'anything-config)

;; 情報元を設定
(setq anything-sources (list anything-c-source-buffers
                             anything-c-source-bookmarks
                             anything-c-source-file-name-history
                             anything-c-source-man-pages
                             anything-c-source-info-pages
                             anything-c-source-complex-command-history))

;; anything のキーマップ
(define-key anything-map "\C-p" 'anything-previous-line)
(define-key anything-map "\C-n" 'anything-next-line)
(define-key anything-map "\C-v" 'anything-next-page)
(define-key anything-map "\M-v" 'anything-previous-page)

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
; test-case-mode.el
; - テスト駆動開発支援
;
; - Project Homepage
; http://nschum.de/src/emacs/test-case-mode/
;=======================================================================
(require 'test-case-mode)
(add-hook 'find-file-hook 'enable-test-case-mode-if-test)

;; コンパイル後に、テストの自動実行
(add-hook 'compilation-finish-functions 'test-case-compilation-finish-run-all)

;; test-case-mode のキーマップ
(define-key test-case-mode-map "\C-ct" 'test-case-run)

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
; magit.el
; - Git
;
; - Project page
; http://philjackson.github.com/magit/
;=======================================================================
(autoload 'magit-status "magit" nil t)

;; vc-mode 無効化
;; - http://bit.ly/xPV6eW
(eval-after-load "vc" '(remove-hook 'find-file-hooks 'vc-find-file-hook))

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
; c-mode, c++-mode
;
; - 参考
; http://d.hatena.ne.jp/i_s/20091026/1256557730
;=======================================================================
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

;; インデント設定
(add-hook 'c-mode-common-hook
          '(lambda ()
             ;; インデントスタイルの設定(gnu,bsd,k&r,stroustrup,linux,java)
             (c-set-style "stroustrup")
             ;; インデントに使う文字設定(t: タブ, nil: スペース)
             (setq indent-tabs-mode nil)
             ;; インデント幅
             (setq c-basic-offset 4)
             ;; namespace {}の中はインデントしない
             (c-set-offset 'innamespace 0)
             ;; 関数の引数リストの閉じ括弧はインデントしない
             (c-set-offset 'arglist-close 0)
             ;; インライン関数の開始括弧はインデントしない
             (c-set-offset 'inline-open 0)
             ;; メンバ初期化リストの開始 `:' のインデント量
             (c-set-offset 'member-init-intro 2)
             ;; extern "C" 内のブロックのインデント量
             (c-set-offset 'inextern-lang 0)
             ;; `;' を入力したら、自動改行+インデント
             (c-toggle-auto-hungry-state 1)
             ;; Enterで改行とインデント
             (define-key c-mode-base-map "\C-m" 'newline-and-indent)
             ;; ソースとヘッダの切り替え
             (define-key c-mode-base-map [f4] 'ff-find-other-file)
            ))

;; ff-find-other-file でヘッダを探すパス
(defcustom cc-search-directories
  '("." "/usr/include" "/usr/local/include/*")
  "*See the description of the `ff-search-directories' variable."
  :type '(repeat directory)
  :group 'ff)

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
; actionscript-mode
;
; - Project wiki
; http://www.emacswiki.org/emacs/ActionScriptMode/
;=======================================================================
(autoload 'actionscript-mode "actionscript-mode" "actionscript" t)
(add-to-list 'auto-mode-alist '("\\.as\\'" . actionscript-mode))
(add-to-list 'auto-mode-alist '("\\.mxml\\'" . xml-mode))

;; mxml + Action Script
(mmm-add-classes
 '((embedded-as
    :submode actionscript-mode
    :face mmm-code-submode-face
    ;:front "<mx:Script[^>]*>\\n?\\s-*<!\\[CDATA\\["
    ;:back "]]>\\n?\\s-*</mx:Script>")))
    :front "<mx:Script>"
    :back "</mx:Script>")))
(mmm-add-mode-ext-class nil "\\.mxml\\'" 'embedded-as)

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
(global-set-key "\C-xa" 'anything)             ;anything の開始
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

(global-set-key "\C-cg" 'magit-status)         ;magit実行
(global-set-key "\C-cs" 'scheme-other-window)  ;scheme実行
(global-set-key "\C-ct" 'multi-term)           ;ターミナルを開く
(global-set-key "\C-cr" 'my-window-resizer)    ;ウィンドウのリサイズ

(put 'set-goal-column 'disabled nil)

;;
;=======================================================================
; Customizer
;=======================================================================
