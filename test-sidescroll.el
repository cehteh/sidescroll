;;; test-sidescroll.el --- Tests for sidescroll -*- lexical-binding: t; -*-

;;; Commentary:
;; Basic tests for sidescroll-mode functionality

;;; Code:

(require 'ert)
(require 'sidescroll)

(ert-deftest sidescroll-test-mode-activation ()
  "Test that sidescroll-mode can be activated."
  (with-temp-buffer
    (insert "Test content\n")
    (insert "Line 2\n")
    (insert "Line 3\n")
    (sidescroll-mode 1)
    (should sidescroll-mode)
    (should sidescroll--minimap-window)
    (should (window-live-p sidescroll--minimap-window))
    (sidescroll-mode -1)
    (should-not sidescroll-mode)
    (should-not (and sidescroll--minimap-window
                     (window-live-p sidescroll--minimap-window)))))

(ert-deftest sidescroll-test-customization ()
  "Test customization variables exist."
  (should (custom-variable-p 'sidescroll-window-side))
  (should (custom-variable-p 'sidescroll-font-size))
  (should (custom-variable-p 'sidescroll-window-width)))

(ert-deftest sidescroll-test-minimap-buffer-creation ()
  "Test that minimap buffer is created correctly."
  (with-temp-buffer
    (let ((main-buffer (current-buffer)))
      (insert "Test content for minimap\n")
      (sidescroll-mode 1)
      (should sidescroll--minimap-window)
      (let ((minimap-buffer (window-buffer sidescroll--minimap-window)))
        (should minimap-buffer)
        (should (buffer-live-p minimap-buffer))
        (with-current-buffer minimap-buffer
          (should sidescroll--main-buffer)
          (should (eq sidescroll--main-buffer main-buffer))))
      (sidescroll-mode -1))))

(ert-deftest sidescroll-test-content-sync ()
  "Test that content is synchronized to minimap."
  (with-temp-buffer
    (insert "Initial content\n")
    (sidescroll-mode 1)
    (should sidescroll--minimap-window)
    (let ((minimap-buffer (window-buffer sidescroll--minimap-window)))
      (should minimap-buffer)
      (with-current-buffer minimap-buffer
        (should (string-match-p "Initial content" (buffer-string))))
      ;; Add more content
      (insert "New line\n")
      (sidescroll--update-minimap-content)
      (with-current-buffer minimap-buffer
        (should (string-match-p "New line" (buffer-string)))))
    (sidescroll-mode -1)))

;;; test-sidescroll.el ends here
