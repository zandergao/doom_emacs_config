;;; init-ui.el -*- lexical-binding: t; -*-

;;; Status line
(setq display-time-24hr-format t
      display-time-day-and-date t
      display-time-default-load-average nil)
(display-time-mode 1)

;;; Startup frame
(add-hook! 'window-setup-hook #'toggle-frame-maximized)

;;; Nyan Cat
(use-package! nyan-mode
  :if (locate-library "nyan-mode")
  :init
  (setq nyan-animate-nyancat t
        nyan-wavy-trail t)
  :config
  (nyan-mode 1))

;;; Chinese fonts
(use-package! cnfonts
  :if (locate-library "cnfonts")
  :config
  (cnfonts-enable))

(when (require 'holo-layer nil t)
  ;; MacOS 请使用窗口模式
  (setq holo-layer-enable-cursor-animation t)
  (setq holo-layer-enable-place-info t)
  (holo-layer-enable))
