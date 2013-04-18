;;--------------------------------------------------
;; File name    :   42-ruby.el
;;              :   Ruby のソースコード編集
;;              :
;;--------------------------------------------------
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

