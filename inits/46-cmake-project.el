;;--------------------------------------------------
;; File name    :   46-cmake-project.el
;;              :   CMake のプロジェクト
;;              :
;;--------------------------------------------------
;;
;=======================================================================
; cmake-project.el
;
; - GitHub
; https://github.com/alamaison/emacs-cmake-project
;=======================================================================
(add-to-list 'load-path (concat siteinit-path "elisp/cmake-project") t)
(autoload 'cmake-project-mode "cmake-project")

(defun maybe-cmake-project-hook ()
  (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
(add-hook 'c-mode-hook 'maybe-cmake-project-hook)
(add-hook 'c++-mode-hook 'maybe-cmake-project-hook)
