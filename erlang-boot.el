;;; erlang-boot.el --- make Erlang boot files human-readable  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Magnus Henoch

;; Author: Magnus Henoch <magnus.henoch@gmail.com>

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

;; Erlang boot files are just the corresponding script file converted to
;; the external term format.  Seeing the binary format isn't very
;; interesting, so let's decode it automatically whenever we happen to
;; open a boot file!
;;
;; This function replaces the binary contents of the boot file with the
;; text representation.  That means that you can't modify the binary
;; file, but why would you do that...

;;; Code:

;;;###autoload
(define-derived-mode erlang-boot-file-mode special-mode "Erlang-Boot"
  "Major mode for binary Erlang boot files.
It invokes Erlang to decode the file."
  (let ((file-buffer (current-buffer))
        (temp-buffer (generate-new-buffer " *erlang-boot*")))
    (unwind-protect
        (let* ((coding-system-for-write 'binary)
               (exit-status
                (call-process-region
                 (point-min) (point-max)
                 "erl" nil temp-buffer nil
                 "-noshell"
                 "-eval"
                 (format "{ok, X} = file:read(standard_io, %d), io:format(\"~p~n\", [binary_to_term(list_to_binary(X))])"
                         (buffer-size))
                 "-s" "erlang" "halt")))
          (if (eq exit-status 0)
              (with-silent-modifications
                ;; We're replacing the buffer contents with the text
                ;; representation, so try to make sure we don't save
                ;; the file.
                (add-hook 'write-contents-functions
                          (apply-partially 'user-error "Cannot modify files in `erlang-boot-file-mode'")
                          nil t)
                (with-current-buffer temp-buffer
                  (copy-to-buffer file-buffer (point-min) (point-max))))
            (error "Term conversion failed with %S; %s"
                   exit-status (with-current-buffer temp-buffer (buffer-string)))))
      (kill-buffer temp-buffer))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.boot\\'" . erlang-boot-file-mode))

(provide 'erlang-boot)
;;; erlang-boot.el ends here
