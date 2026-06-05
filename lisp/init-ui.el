;;; init-ui.el -*- lexical-binding: t; -*-

;;; Status line
(setq display-time-24hr-format t
      display-time-day-and-date t
      display-time-default-load-average nil)
(display-time-mode 1)

;;; Startup frame
(add-to-list 'initial-frame-alist '(fullscreen . fullboth))
(add-to-list 'default-frame-alist '(fullscreen . fullboth))

(add-hook! 'window-setup-hook
  (set-frame-parameter nil 'fullscreen 'fullboth))

;;; Nyan Cat
(when (require 'nyan-mode nil t)
  (setq nyan-animate-nyancat t
        nyan-wavy-trail t)
  (nyan-mode 1))

;;; Chinese fonts
(when (require 'cnfonts nil t)
  (cnfonts-enable))

;;; HoloLayer cursor effect
(when (require 'holo-layer nil t)
  (setq holo-layer-enable-cursor-animation t
        holo-layer-enable-type-animation nil
        holo-layer-enable-indent-rainbow nil
        holo-layer-enable-window-border nil)
  (holo-layer-enable))
