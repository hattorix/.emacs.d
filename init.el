;;--------------------------------------------------
;; File name    :   init.el
;;              :   Emacs の基本的な設定
;;              :
;;--------------------------------------------------
;;
;=======================================================================
; このファイルが置かれているディレクトリ
;=======================================================================
(defvar siteinit-path (expand-file-name "~/.emacs.d/"))

;;
;=======================================================================
; elisp の追加読み込み PATH
;=======================================================================
(add-to-list 'load-path (concat siteinit-path "elisp") t)
(add-to-list 'load-path (concat siteinit-path "elisp/init-loader") t)

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
