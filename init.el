(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

(defun font-existsp (font)
  (interactive)
  (if (string-equal (describe-font font)
                    "No matching font being used")
    nil
    t))
(set-face-attribute 'default nil :family "FiraCode Nerd Font" :height 103)
