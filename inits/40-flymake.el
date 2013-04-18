;;--------------------------------------------------
;; File name    :   40-flymake.el
;;              :   flymake の基本設定
;;              :
;;--------------------------------------------------
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

