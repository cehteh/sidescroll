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

(defcustom sidescroll-line-spacing 0
  "Line spacing for the minimap window.
Can be a positive integer, zero, or negative (if supported by Emacs)."
  :type 'integer
  :group 'sidescroll)

(defface sidescroll-face
  '((t (:weight ultra-light :height 80 :width condensed)))
  "Face used for text in the sidescroll minimap window."
  :group 'sidescroll)

(defvar-local sidescroll--minimap-window nil
  "The minimap window associated with the current buffer.")

(defvar-local sidescroll--main-buffer nil
  "The main buffer that this minimap window is displaying.")

(defvar-local sidescroll--updating nil
  "Flag to prevent recursive updates during synchronization.")

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
      
      ;; Set line spacing (buffer-local)
      (setq-local line-spacing sidescroll-line-spacing)
      
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
        (use-local-map map)))
    window))

(defun sidescroll--sync-to-minimap ()
  "Synchronize the main buffer's position to the minimap."
  (when (and sidescroll-mode
             sidescroll--minimap-window
             (window-live-p sidescroll--minimap-window)
             (not sidescroll--updating))
    (let ((main-point (point))
          (main-buffer (current-buffer)))
      (setq sidescroll--updating t)
      (with-selected-window sidescroll--minimap-window
        (when (eq sidescroll--main-buffer main-buffer)
          (goto-char main-point)
          (recenter)))
      (setq sidescroll--updating nil))))

(defun sidescroll--sync-from-minimap ()
  "Synchronize the minimap's position to the main buffer."
  (when (and sidescroll--main-buffer
             (buffer-live-p sidescroll--main-buffer)
             (not sidescroll--updating))
    (let ((minimap-point (point)))
      (setq sidescroll--updating t)
      (with-current-buffer sidescroll--main-buffer
        (goto-char minimap-point)
        (recenter))
      (setq sidescroll--updating nil))))

(defun sidescroll--mouse-drag (event)
  "Handle mouse drag events in the minimap.
Synchronize the main buffer position when clicking or dragging in the minimap."
  (interactive "e")
  (let* ((start (event-start event))
         (window (posn-window start))
         (pos (posn-point start)))
    (when (and (windowp window) pos)
      (with-selected-window window
        (goto-char pos)
        (when sidescroll--main-buffer
          (sidescroll--sync-from-minimap))))))

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
