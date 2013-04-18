;;
;=======================================================================
; elisp の自動バイトコンパイル
;=======================================================================
(require 'auto-async-byte-compile)
;; 無視リスト
(setq auto-async-byte-compile-exclude-files-regexp "/junk/")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
