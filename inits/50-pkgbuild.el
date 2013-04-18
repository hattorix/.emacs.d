;;
;=======================================================================
; pkgbuild-mode.el
; - PKGBUILD ファイル編集用のメジャーモード
;
; - Project page
; https://github.com/juergenhoetzel/pkgbuild-mode
;=======================================================================
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode))
                              auto-mode-alist))
