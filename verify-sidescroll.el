#!/usr/bin/env emacs --script
;;; verify-sidescroll.el --- Manual verification script for sidescroll

;;; Commentary:
;; This script loads sidescroll and verifies basic functionality works

;;; Code:

(add-to-list 'load-path (file-name-directory load-file-name))
(require 'sidescroll)

(message "Testing sidescroll-mode...")

;; Test 1: Mode can be loaded
(message "✓ sidescroll-mode loaded successfully")

;; Test 2: Check customization variables
(unless (custom-variable-p 'sidescroll-window-side)
  (error "sidescroll-window-side not defined"))
(message "✓ Customization variables defined")

;; Test 3: Check that functions are defined
(unless (fboundp 'sidescroll-mode)
  (error "sidescroll-mode function not defined"))
(message "✓ sidescroll-mode function defined")

;; Test 4: Check helper functions
(let ((helpers '(sidescroll--get-minimap-buffer
                 sidescroll--setup-minimap-window
                 sidescroll--sync-to-minimap
                 sidescroll--sync-from-minimap
                 sidescroll--update-minimap-content
                 sidescroll--cleanup
                 sidescroll--setup)))
  (dolist (helper helpers)
    (unless (fboundp helper)
      (error "Function %s not defined" helper)))
  (message "✓ All helper functions defined"))

;; Test 5: Test mode activation in a buffer
(with-temp-buffer
  (insert "Test content\n")
  (insert "Line 2\n")
  (insert "Line 3\n")
  (sidescroll-mode 1)
  (unless sidescroll-mode
    (error "sidescroll-mode not activated"))
  (unless sidescroll--minimap-window
    (error "Minimap window not created"))
  (unless (window-live-p sidescroll--minimap-window)
    (error "Minimap window not live"))
  (message "✓ Mode activation works")
  
  ;; Test deactivation
  (sidescroll-mode -1)
  (when (and sidescroll--minimap-window 
             (window-live-p sidescroll--minimap-window))
    (error "Minimap window not cleaned up"))
  (message "✓ Mode deactivation works"))

(message "\nAll manual verification tests passed!")
(message "sidescroll-mode is ready to use.")

;;; verify-sidescroll.el ends here
