;;--------------------------------------------------
;; File name    :   42-coffeescript.el
;;              :   Java Script のソースコード編集
;;              :
;;--------------------------------------------------
;;
;=======================================================================
; coffee-mode.el
; - coffee-script プログラミング用のメジャーモード
;
; - Project homepage
; https://github.com/defunkt/coffee-mode
;=======================================================================
(add-to-list 'load-path (concat siteinit-path "coffee-mode") t)

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
