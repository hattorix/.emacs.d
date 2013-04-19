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

;=======================================================================
; global key binding
;=======================================================================
(global-set-key [f2] 'tabbar-backward-tab)     ;前のタブへ
(global-set-key [S-f2] 'tabbar-backward-group) ;前のタブグループへ
(global-set-key [f3] 'tabbar-forward-tab)      ;次のタブへ
(global-set-key [S-f3] 'tabbar-forward-group)  ;次のタブグループへ
(global-set-key [C-prior] 'tabbar-backward-tab);前のタブへ
(global-set-key [C-next] 'tabbar-forward-tab)  ;次のタブへ
