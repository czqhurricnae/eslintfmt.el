;;; ESLINTFMT --- Format the javascript file in js2-mode.
;;
;; Author: c <c@cdeMacBook-Air.local>
;; Copyright © 2019, c, all rights reserved.
;; Created: 19 June 2019
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(defgroup eslintfmt nil
  "Minor mode for formatting JavaScript buffers with eslint."
  :link '(url-link "https://github.com/czqhurricane/eslintfmt.el")
  :group 'js)

(defcustom eslintfmt-command "eslint"
  "Specify which formatter to use."
  :type '(choice
          (string :tag "eslint" "eslint")
          (string :tag "jshint" "jshint"))
  :group 'eslintfmt)

(defcustom eslintfmt-command-args nil
  "Arguments passed to 'format.'."
  :type '(repeat string)
  :group 'eslintfmt)

(defcustom eslintfmt-custom-command nil
  "Specifty a custom formatter command."
  :type 'string
  :group 'eslintfmt)

(defun eslintfmt ()
  "Format the current buffer with eslint/format.."
  (interactive)
  (let ((tmpfile (make-temp-file "eslintfmt" nil ".js"))
        (patchbuf (get-buffer-create "*eslintfmt patch*"))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        our-command-args)

    (unwind-protect
        (save-restriction
          (widen)
          (with-current-buffer patchbuf (erase-buffer))

          (write-region nil nil tmpfile)

          (setq our-command-args (append our-command-args
                                         eslintfmt-command-args
                                         (list "--fix" tmpfile)))

          (message "Calling eslint: %s %s"
                   eslintfmt-command our-command-args)

          (apply #'call-process-region (point-min) (point-max)
                 eslintfmt-command nil nil nil our-command-args)

          (if (zerop (call-process-region (point-min) (point-max)
                                          "diff" nil patchbuf nil "-n" "-"
                                          tmpfile))
              (message "Buffer is already format.")
            (eslintfmt--apply-rcs-patch patchbuf)
            (message "Applied format."))))

    (kill-buffer patchbuf)
    (delete-file tmpfile)))

(defun eslintfmt--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "invalid rcs patch or internal error in eslintfmt--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (cl-decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (eslintfmt--goto-line (- from line-offset))
                (cl-incf line-offset len)
                (eslintfmt--delete-whole-line len)))
             (t
              (error "invalid rcs patch or internal error in eslintfmt--apply-rcs-patch")))))))))

(defun eslintfmt--delete-whole-line (&optional arg)
  "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function."
  (setq arg (or arg 1))
  (if (and (> arg 0)
           (eobp)
           (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0)
           (bobp)
           (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (cond ((zerop arg)
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (end-of-visible-line) (point))))
        ((< arg 0)
         (delete-region (progn (end-of-visible-line) (point))
                        (progn (forward-visible-line (1+ arg))
                               (unless (bobp)
                                 (backward-char))
                               (point))))
        (t
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (forward-visible-line arg) (point))))))

(defun eslintfmt--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

;;;###autoload
(define-minor-mode eslintfmt-mode
  "Enable format-on-save for javascript mode buffers via eslintfmt."
  :lighter " fmt"
  (if eslintfmt-mode
      (add-hook 'before-save-hook 'eslintfmt-before-save t t)
    (remove-hook 'before-save-hook 'eslintfmt-before-save t)))

(defun eslintfmt-before-save ()
  "Format buffer via eslintfmt if major mode is a javascript mode."
  (interactive)
  (eslintfmt))

(provide 'eslintfmt)
;;; eslintfmt.el ends here
