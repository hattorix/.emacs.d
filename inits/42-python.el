;;--------------------------------------------------
;; File name    :   42-python.el
;;              :   Python のソースコード編集
;;              :
;;--------------------------------------------------
;;
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

