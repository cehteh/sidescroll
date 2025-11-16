;; Example Emacs Lisp code for testing sidescroll-mode
;; This file contains enough content to make the minimap useful

(defun example-function-1 ()
  "An example function."
  (interactive)
  (message "This is example function 1"))

(defun example-function-2 ()
  "Another example function."
  (interactive)
  (let ((result (+ 1 2 3 4 5)))
    (message "Sum is: %d" result)))

(defun example-function-3 (arg)
  "Function with an argument ARG."
  (interactive "P")
  (if arg
      (message "Called with prefix arg: %s" arg)
    (message "Called without prefix arg")))

(defun example-function-4 ()
  "A longer function to test scrolling."
  (interactive)
  (let ((items '(1 2 3 4 5 6 7 8 9 10)))
    (dolist (item items)
      (message "Processing item: %d" item)
      (sit-for 0.1))))

(defun example-function-5 ()
  "Yet another example function."
  (interactive)
  (with-temp-buffer
    (insert "Temporary content")
    (buffer-string)))

(defun example-function-6 ()
  "Function demonstrating some Emacs Lisp features."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (forward-line 1))))

(defun example-function-7 ()
  "More example code for testing."
  (interactive)
  (when (yes-or-no-p "Continue? ")
    (message "User said yes")))

(defun example-function-8 ()
  "Function with conditionals."
  (interactive)
  (cond
   ((> 5 3) (message "5 is greater than 3"))
   ((< 5 3) (message "5 is less than 3"))
   (t (message "This should not happen"))))

(defun example-function-9 ()
  "Function with a loop."
  (interactive)
  (dotimes (i 10)
    (message "Iteration: %d" i)))

(defun example-function-10 ()
  "Final example function."
  (interactive)
  (let ((name (read-string "Enter your name: ")))
    (message "Hello, %s!" name)))

;; End of example file
