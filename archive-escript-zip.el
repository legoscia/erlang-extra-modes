;;; archive-escript-zip.el --- Open escript zip archives in archive-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Magnus Henoch

;; Author: Magnus Henoch <magnus.henoch@gmail.com>
;; Package-Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; escript (Erlang script) files come in many shapes, one of which is
;; a zip file containing compiled Erlang code with a shebang line
;; prepended.  This module lets you open such files in archive-mode as
;; if they were a plain zip archive.

;;; Code:

(defun archive-escript-zip--narrow ()
  "Narrow to the part of the buffer that contains the zip archive."
  (widen)
  (goto-char (point-min))
  ;; borrowed this regexp from `archive-find-type'
  (search-forward-regexp "^\\(PK00\\)?[P]K\003\004")
  (narrow-to-region (match-beginning 0) (point-max)))

(defun archive-escript-zip-summarize ()
  "'Summarize' callback for escript zip archives.
See `archive-summarize'."
  (archive-escript-zip--narrow)
  (archive-zip-summarize))

(defun archive-escript-zip-extract (_archive name)
  "'Extract' callback for escript zip archives.
See `archive-extract'."
  (let ((temp-file (make-temp-file "escript-archive" nil ".zip"))
        (coding-system-for-write 'binary))
    (unwind-protect
        (progn
          (with-current-buffer archive-superior-buffer
            (save-restriction
              (widen)
              (write-region archive-proper-file-start
                            (point-max)
                            temp-file nil :silent)))
          (archive-zip-extract temp-file name))
      (delete-file temp-file))))

;;;###autoload
(defun archive-escript-zip-find-type (old-fun)
  "Return 'escript-zip if this seems to be an escript zip archive.
This function is meant to be used as :around advice for
`archive-find-type'."
  (widen)
  (goto-char (point-min))
  (let (case-fold-search)
    (cond
     ((and (looking-at "#!.*escript")
           (search-forward-regexp "^\\(PK00\\)?[P]K\003\004" nil t))
      'escript-zip)
     (t
      (funcall old-fun)))))

;;;###autoload
(with-eval-after-load "arc-mode"
  (advice-add 'archive-find-type :around #'archive-escript-zip-find-type))

;;;###autoload
(defun archive-escript-zip--maybe-turn-on ()
  "Turn on `archive-mode' if this looks like an escript zip archive.
This function is meant to be called from `erlang-mode-hook'.
An escript zip archive normally has a shebang line that causes
Erlang mode to be activated (thanks to `interpreter-mode-alist').
We need to override the mode here."
  (require 'arc-mode)
  (save-excursion
    (save-restriction
      (when (eq (ignore-errors (archive-find-type)) 'escript-zip)
        (run-with-idle-timer
         0.1 nil
         (lambda (buffer)
           (with-current-buffer buffer
             (archive-mode)))
         (current-buffer))))))

;;;###autoload
(with-eval-after-load "erlang"
  (add-hook 'erlang-mode-hook 'archive-escript-zip--maybe-turn-on))

(provide 'archive-escript-zip)
;;; archive-escript-zip.el ends here
