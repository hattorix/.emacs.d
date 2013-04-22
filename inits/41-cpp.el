;;--------------------------------------------------
;; File name    :   41-cpp.el
;;              :   C, C++ のソースコード編集
;;              :
;;--------------------------------------------------
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
  '("." "/usr/include" "/usr/local/include/*" "/opt/java/include")
  "*See the description of the `ff-search-directories' variable."
  :type '(repeat directory)
  :group 'ff)

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
