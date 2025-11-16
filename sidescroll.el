;;; sidescroll.el --- A minimap mode for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Christian Thaeter
;; Version: 0.1.0
;; Maintainer: Christian Thaeter
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience, frames
;; URL: https://github.com/cehteh/sidescroll

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Sidescroll provides a minimap-style view of your buffer.
;; It creates a side window showing the same buffer with a much smaller font,
;; allowing you to see an overview of the entire file while editing.
;;
;; Features:
;; - Customizable position (left or right side)
;; - Adjustable font size for the minimap
;; - Automatic cursor synchronization
;; - Clean interface (no line numbers, no modeline)
;;
;; Usage:
;; Enable sidescroll-mode in a buffer:
;;   M-x sidescroll-mode
;;
;; Customization:
;; - `sidescroll-window-side': Choose 'left or 'right (default: 'right)
;; - `sidescroll-font-size': Font size in points for minimap (default: 2)
;; - `sidescroll-window-width': Width of minimap window in columns (default: 30)

;;; Code:

(defgroup sidescroll nil
  "Minimap-style buffer view in a side window."
  :group 'convenience
  :prefix "sidescroll-")

(defcustom sidescroll-window-side 'right
  "Which side to display the minimap window.
Can be either \\='left or \\='right."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right))
  :group 'sidescroll)

(defcustom sidescroll-window-width 30
  "Width of the minimap window in columns."
  :type 'integer
  :group 'sidescroll)

(defface sidescroll-face
  '((t (:weight ultra-light :height 80 :width condensed)))
  "Face used for text in the sidescroll minimap window."
  :group 'sidescroll)

(defface sidescroll-current-line-face
  '((t (:inherit sidescroll-face :box (:line-width 1 :color "gray") :extend t)))
  "Face used to highlight the current line in the sidescroll minimap window."
  :group 'sidescroll)

(defvar-local sidescroll--minimap-window nil
  "The minimap window associated with the current buffer.")

(defvar-local sidescroll--main-buffer nil
  "The main buffer that this minimap window is displaying.")

(defvar-local sidescroll--updating nil
  "Flag to prevent recursive updates during synchronization.")

(defvar-local sidescroll--current-line-overlay nil
  "Overlay for highlighting the current line in the minimap.")

(defvar-local sidescroll--visible-region-overlay nil
  "Overlay for highlighting the visible region in the minimap.")

(defun sidescroll--get-minimap-buffer (main-buffer)
  "Get or create the minimap buffer for MAIN-BUFFER."
  (let ((minimap-name (format " *sidescroll-%s*" (buffer-name main-buffer))))
    (or (get-buffer minimap-name)
        (with-current-buffer (get-buffer-create minimap-name)
          (setq sidescroll--main-buffer main-buffer)
          (current-buffer)))))

(defun sidescroll--setup-minimap-window (minimap-buffer)
  "Setup the minimap window displaying MINIMAP-BUFFER."
  (let ((window (display-buffer-in-side-window
                 minimap-buffer
                 `((side . ,sidescroll-window-side)
                   (slot . 0)
                   (window-width . ,sidescroll-window-width)
                   (preserve-size . (t . nil))))))
    (with-selected-window window
      ;; Apply the sidescroll face to buffer text
      (buffer-face-set 'sidescroll-face)
      
      ;; Disable line numbers
      (when (fboundp 'display-line-numbers-mode)
        (display-line-numbers-mode -1))
      
      ;; Disable modeline
      (setq mode-line-format nil)
      
      ;; Make buffer read-only
      (setq buffer-read-only t)
      
      ;; Set line spacing to zero (buffer-local)
      (setq-local line-spacing 0)
      
      ;; Set other visual properties
      (setq cursor-type nil)
      (setq truncate-lines t)
      
      ;; Disable fringes
      (set-window-fringes window 0 0)
      
      ;; Remove truncation glyph ('$' character)
      (let ((table (make-display-table)))
        (set-display-table-slot table 'truncation ?\s)
        (setq-local buffer-display-table table))
      
      ;; Setup mouse bindings for dragging
      (let ((map (make-sparse-keymap)))
        (define-key map [down-mouse-1] 'sidescroll--mouse-drag)
        (define-key map [drag-mouse-1] 'sidescroll--mouse-drag)
        (define-key map [mouse-1] 'sidescroll--mouse-drag)
        ;; Mouse wheel bindings
        (define-key map [wheel-up] 'sidescroll--mouse-wheel)
        (define-key map [wheel-down] 'sidescroll--mouse-wheel)
        (define-key map [mouse-4] 'sidescroll--mouse-wheel)
        (define-key map [mouse-5] 'sidescroll--mouse-wheel)
        (use-local-map map)))
    window))

(defun sidescroll--update-highlights ()
  "Update the current line and visible region highlights in the minimap."
  (when (and sidescroll--minimap-window
             (window-live-p sidescroll--minimap-window))
    (with-selected-window sidescroll--minimap-window
      (let ((main-window (get-buffer-window sidescroll--main-buffer))
            (minimap-buffer (current-buffer)))
        (when main-window
          ;; Update current line overlay
          (let ((main-line-start (with-selected-window main-window
                                   (line-beginning-position)))
                (main-line-end (with-selected-window main-window
                                 (line-end-position))))
            (if sidescroll--current-line-overlay
                (move-overlay sidescroll--current-line-overlay 
                             main-line-start (1+ main-line-end) minimap-buffer)
              (setq sidescroll--current-line-overlay 
                    (make-overlay main-line-start (1+ main-line-end) minimap-buffer))
              (overlay-put sidescroll--current-line-overlay 'face 'sidescroll-current-line-face)))
          
          ;; Update visible region overlay
          (let ((win-start (window-start main-window))
                (win-end (window-end main-window t)))
            (if sidescroll--visible-region-overlay
                (move-overlay sidescroll--visible-region-overlay 
                             win-start win-end minimap-buffer)
              (setq sidescroll--visible-region-overlay 
                    (make-overlay win-start win-end minimap-buffer))
              (overlay-put sidescroll--visible-region-overlay 'face 
                          '(:background "gray20")))))))))

(defun sidescroll--sync-to-minimap ()
  "Synchronize the main buffer's position to the minimap."
  (when (and sidescroll-mode
             sidescroll--minimap-window
             (window-live-p sidescroll--minimap-window)
             (not sidescroll--updating))
    (let* ((main-point (point))
           (main-buffer (current-buffer))
           (main-window (selected-window))
           (win-start (window-start main-window))
           (win-end (window-end main-window t))
           (win-middle (/ (+ win-start win-end) 2)))
      (setq sidescroll--updating t)
      (with-selected-window sidescroll--minimap-window
        (when (eq sidescroll--main-buffer main-buffer)
          ;; Center the visible region in the minimap
          (goto-char win-middle)
          (recenter)
          ;; Update highlights
          (sidescroll--update-highlights)))
      (setq sidescroll--updating nil))))

(defun sidescroll--sync-from-minimap ()
  "Synchronize the minimap's position to the main buffer."
  (when (and sidescroll--main-buffer
             (buffer-live-p sidescroll--main-buffer)
             (not sidescroll--updating))
    (let* ((minimap-point (point))
           (main-window (get-buffer-window sidescroll--main-buffer)))
      (when main-window
        (setq sidescroll--updating t)
        (with-selected-window main-window
          (goto-char minimap-point)
          (recenter))
        ;; Update highlights after syncing
        (sidescroll--update-highlights)
        (setq sidescroll--updating nil)))))

(defun sidescroll--mouse-drag (event)
  "Handle mouse drag events in the minimap.
Synchronize the main buffer position when clicking or dragging in the minimap."
  (interactive "e")
  (mouse-set-point event)
  (when sidescroll--main-buffer
    (sidescroll--sync-from-minimap)))

(defun sidescroll--mouse-wheel (event)
  "Handle mouse wheel scrolling in the minimap.
Synchronize scrolling with the main buffer."
  (interactive "e")
  (let ((minimap-window (selected-window)))
    ;; Perform the scroll in the minimap
    (mwheel-scroll event)
    ;; Sync the position to the main buffer
    (when (and sidescroll--main-buffer
               (eq (selected-window) minimap-window))
      (sidescroll--sync-from-minimap))))

(defun sidescroll--update-minimap-content ()
  "Update the minimap buffer to reflect the main buffer's content."
  (when (and sidescroll-mode
             sidescroll--minimap-window
             (window-live-p sidescroll--minimap-window))
    (let ((main-buffer (current-buffer))
          (minimap-buffer (window-buffer sidescroll--minimap-window)))
      (with-current-buffer minimap-buffer
        (let ((inhibit-read-only t)
              (old-point (point)))
          (erase-buffer)
          (insert-buffer-substring main-buffer)
          (goto-char old-point))))))

(defun sidescroll--cleanup ()
  "Clean up the minimap window and buffer."
  ;; Clean up overlays
  (when sidescroll--current-line-overlay
    (delete-overlay sidescroll--current-line-overlay)
    (setq sidescroll--current-line-overlay nil))
  (when sidescroll--visible-region-overlay
    (delete-overlay sidescroll--visible-region-overlay)
    (setq sidescroll--visible-region-overlay nil))
  ;; Clean up window and buffer
  (when (and sidescroll--minimap-window
             (window-live-p sidescroll--minimap-window))
    (let ((minimap-buffer (window-buffer sidescroll--minimap-window)))
      (delete-window sidescroll--minimap-window)
      (when (buffer-live-p minimap-buffer)
        (kill-buffer minimap-buffer))))
  (setq sidescroll--minimap-window nil))

(defun sidescroll--post-command-hook ()
  "Hook function to sync after commands in main buffer."
  (when sidescroll-mode
    (sidescroll--sync-to-minimap)))

(defun sidescroll--after-change-hook (&rest _args)
  "Hook function to update minimap content after buffer change."
  (when sidescroll-mode
    (sidescroll--update-minimap-content)))

(defun sidescroll--setup ()
  "Set up the sidescroll minimap."
  (let* ((main-buffer (current-buffer))
         (minimap-buffer (sidescroll--get-minimap-buffer main-buffer)))
    
    ;; Initial content sync
    (with-current-buffer minimap-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-buffer-substring main-buffer)))
    
    ;; Create and setup the minimap window
    (setq sidescroll--minimap-window
          (sidescroll--setup-minimap-window minimap-buffer))
    
    ;; Add hooks for synchronization
    (add-hook 'post-command-hook #'sidescroll--post-command-hook nil t)
    (add-hook 'after-change-functions #'sidescroll--after-change-hook nil t)
    
    ;; Initial position sync
    (sidescroll--sync-to-minimap)))

;;;###autoload
(define-minor-mode sidescroll-mode
  "Toggle sidescroll minimap mode.

When enabled, displays a minimap of the current buffer in a side window.
The minimap shows the same buffer content with a much smaller font,
providing an overview of the entire file.

The minimap automatically synchronizes with the main buffer:
- Cursor position is kept in sync
- Content changes are reflected immediately
- The minimap has no line numbers or modeline for a clean view"
  :lighter " Sidescroll"
  :group 'sidescroll
  (if sidescroll-mode
      (sidescroll--setup)
    (progn
      (remove-hook 'post-command-hook #'sidescroll--post-command-hook t)
      (remove-hook 'after-change-functions #'sidescroll--after-change-hook t)
      (sidescroll--cleanup))))

(provide 'sidescroll)

;;; sidescroll.el ends here
