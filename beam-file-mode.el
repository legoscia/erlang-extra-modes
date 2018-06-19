;;; beam-file-mode.el --- view some information when opening BEAM files  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Magnus Henoch

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

;; BEAM files are binary files containing byte code for the Erlang
;; virtual machine.  Let's try to display something more friendly when
;; opening such a file in Emacs (accidentally or deliberately).

;; http://www.erlang.se/~bjorn/beam_file_format.html
;; http://beam-wisdoms.clau.se/en/latest/indepth-beam-file.html

;;; Code:

(require 'bindat)

;;;###autoload
(add-to-list 'auto-mode-alist
	     '("\\.beam\\'" . beam-file-mode))

;;;###autoload
(define-derived-mode beam-file-mode special-mode "BEAM"
  "Major mode for BEAM files."
  (delete-all-overlays)
  (set-buffer-multibyte nil)
  ;; The file starts with "FOR1", a 32-bit big-endian length (which we
  ;; ignore), and "BEAM".
  (goto-char (point-min))
  ;; "." doesn't match newlines, which might be a legitimate byte in
  ;; the length.  Is there a nicer way to match an arbitrary byte than
  ;; \(?:.\|\n\)?...
  (unless (looking-at "FOR1\\(?:.\\|\n\\)\\{4\\}BEAM")
    (error "Not a BEAM file"))
  (goto-char (match-end 0))
  (overlay-put
   (make-overlay (point-min) (point))
   'display "BEAM file\n\n")
  ;; The rest of the file is made up of chunks, with a four-character
  ;; identifier followed by a 32-bit big-endian length.  Chunks are
  ;; aligned to 4 bytes.
  (while (not (eobp))
    (unless (looking-at "\\([A-Za-z0-9]\\{4\\}\\)\\(\\(?:.\\|\n\\)\\{4\\}\\)")
      (error "Expected chunk header"))
    (let* ((id (match-string 1))
	   (length (cdar (bindat-unpack '((:len u32)) (match-string 2))))
	   (padded-length (let ((rem (% length 4)))
			    (if (zerop rem)
				length
			      (+ length (- 4 rem)))))
	   (extra
	    (funcall (or (intern-soft (concat "beam-file-mode--handle-chunk-" id))
			 'beam-file-mode--handle-unknown-chunk)
		     (match-end 0) (+ (match-end 0) length)))
	   )
      (goto-char (min (+ (match-end 0) padded-length) (point-max)))
      (overlay-put
       (make-overlay (match-beginning 0) (point))
       'display (format "%s chunk, %s bytes\n" id length))
      (with-silent-modifications
	(insert extra "\n"))))
  (goto-char (point-min))

  )

(defun beam-file-mode--handle-unknown-chunk (_beg _end)
  "Handle unrecognised BEAM file chunks."
  ;; Anything sensible to do here?
  "")

(defun beam-file-mode--binary-to-term (beg end)
  ;; checkdoc-params: (beg end)
  "Handle BEAM file chunks that are just an Erlang term."
  (beam-file-mode--erlang-output-to-string
   "io:format(\"~p~n\", [binary_to_term(list_to_binary(X))])"
   beg end))

(fset 'beam-file-mode--handle-chunk-Attr 'beam-file-mode--binary-to-term)
(fset 'beam-file-mode--handle-chunk-CInf 'beam-file-mode--binary-to-term)

(defun beam-file-mode--handle-chunk-Dbgi (beg end)
  ;; checkdoc-params: (beg end)
  "Handle Dbgi chunks (abstract code since Erlang 20)."
  (beam-file-mode--erlang-output-to-string
   "case catch binary_to_term(list_to_binary(X)) of
    {debug_info_v1, erl_abstract_code, Metadata} ->
        try erl_abstract_code:debug_info(erlang_v1, module_name, Metadata, []) of
            {ok, [_ | Forms]} ->
                io:format(\"~s~n\", [erl_prettypr:format(erl_syntax:form_list(Forms))]);
            {error, _} = Error ->
                Error
        catch
            error:undef ->
                io:format(\"~p~n\", [{module, erl_abstract_code, was_introduced_in, 20, but_you_have, erlang:system_info(otp_release)}])
        end;
    {debug_info_v1, Module, _Metadata} ->
        io:format(\"~p~n\", [{unexpected_debug_info_module, Module, expected, erl_abstract_code}]);
    Other ->
        io:format(\"~p~n\", [Other])
end"
   beg end))

(defun beam-file-mode--handle-chunk-Abst (beg end)
  ;; checkdoc-params: (beg end)
  "Handle Abst chunks (abstract code up to Erlang 19)."
  (beam-file-mode--erlang-output-to-string
   "case catch binary_to_term(list_to_binary(X)) of
    {raw_abstract_v1, Forms} ->
        Src = erl_prettypr:format(erl_syntax:form_list(tl(Forms))),
        io:format(\"~s~n\", [Src]);
    Other ->
        io:format(\"This doesn't look like abstract code:~n~p~n\", [Other])
end"
   beg end))

(defun beam-file-mode--erlang-output-to-string (script beg end)
  "Run SCRIPT on the region between BEG and END, and return the output.
SCRIPT is a piece of Erlang code.  It can access the region as a string
in a variable called X."
  (let ((temp-buffer (generate-new-buffer " *erlang-output*"))
	(default-directory temporary-file-directory))
    (unwind-protect
	(let* ((coding-system-for-write 'binary)
	       (exit-status
		(call-process-region
		 beg end
		 "erl" nil temp-buffer nil
		 "-noshell"
		 "-eval"
		 (concat
		  (format "{ok, X} = file:read(standard_io, %d),\n" (- end beg))
		  script)
		 "-s" "erlang" "halt")))
	  (if (eq exit-status 0)
	      (with-current-buffer temp-buffer (buffer-string))
	    (error "Erlang script failed with %S; %s"
		   exit-status (with-current-buffer temp-buffer (buffer-string)))))
      (kill-buffer temp-buffer))))

(provide 'beam-file-mode)
;;; beam-file-mode.el ends here
