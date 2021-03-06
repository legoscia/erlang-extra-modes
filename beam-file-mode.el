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

(defvar-local beam-file-mode--atom-table nil
  "Vector containing atom table of beam file.")

;;;###autoload
(add-to-list 'auto-mode-alist
	     '("\\.beam\\'" . beam-file-mode))

;;;###autoload
(define-derived-mode beam-file-mode special-mode "BEAM"
  "Major mode for BEAM files."
  (delete-all-overlays)
  (set-buffer-multibyte nil)
  (goto-char (point-min))
  ;; BEAM files can be gzip compressed, when compiled with the 'compressed' option.
  ;; The code loader handles such files transparently.
  ;; Let's check if the file is gzipped, with a 1F 8B magic number.
  (when (looking-at "\x1f\x8b")
    (unless (and (fboundp 'zlib-available-p) (zlib-available-p))
      (error "This BEAM file is compressed, but Emacs is not built with zlib"))
    (with-silent-modifications
      (zlib-decompress-region (point-min) (point-max))))
  ;; The file starts with "FOR1", a 32-bit big-endian length (which we
  ;; ignore), and "BEAM".
  ;;
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

(defun beam-file-mode--handle-chunk-Atom (beg end)
  ;; checkdoc-params: (beg end)
  "Handle Latin-1 encoded atom table."
  (beam-file-mode--handle-atom-table 'latin-1 beg end))

(defun beam-file-mode--handle-chunk-AtU8 (beg end)
  ;; checkdoc-params: (beg end)
  "Handle UTF-8 encoded atom table."
  (beam-file-mode--handle-atom-table 'utf-8 beg end))

(defun beam-file-mode--handle-atom-table (coding beg end)
  ;; checkdoc-params: (beg end)
  "Handle atom table in coding system CODING."
  (catch 'atom-chunk-error
    (when (< (- end beg) 4)
      (throw 'atom-chunk-error
	     (format "Atom table chunk %d bytes long, expected at least 4" (- end beg))))
    (let ((count (cdar (bindat-unpack '((:count u32)) (buffer-substring beg (+ 4 beg))))))
      (setq beg (+ 4 beg))
      (setq beam-file-mode--atom-table (make-vector count nil))
      (dotimes (n count)
	(when (>= beg end)
	  (throw 'atom-chunk-error
		 (format "Atom table chunk too short, after %d entries\n" (1- n))))
	(let ((len (cdar (bindat-unpack '((:len u8)) (buffer-substring beg (1+ beg))))))
	  (setq beg (1+ beg))
	  (when (< (- end beg) len)
	    (throw 'atom-chunk-error
		   (format "Atom number %d goes outside atom table chunk\n" n)))
	  (aset beam-file-mode--atom-table n
		(decode-coding-string (buffer-substring beg (+ beg len)) coding))
	  (setq beg (+ beg len))))
      (format "%d %s encoded atoms\nModule name: %s\n"
	      count coding (aref beam-file-mode--atom-table 0)))))

(defun beam-file-mode--handle-chunk-ExpT (beg end)
  ;; checkdoc-params: (beg end)
  "Handle exported functions chunk."
  (catch 'export-chunk-error
    (when (< (- end beg) 4)
      (throw 'export-chunk-error
	     (format "Export table chunk %d bytes long, expected at least 4" (- end beg))))
    (let ((count (cdar (bindat-unpack '((:count u32)) (buffer-substring beg (+ 4 beg)))))
	  exported)
      (setq beg (+ 4 beg))
      (dotimes (n count)
	(when (> (+ beg 12) end)
	  (throw 'export-chunk-error
		 (format "Export table chunk too short, after %d entries\n" (1- n))))
	(let* ((data (bindat-unpack '((:atom-index u32) (:arity u32) (:label :u32))
				    (buffer-substring beg (+ beg 12))))
	       (atom-index (cdr (assq :atom-index data)))
	       (arity (cdr (assq :arity data))))
	  (setq beg (+ beg 12))
	  (push (format "%s/%d" (aref beam-file-mode--atom-table (1- atom-index)) arity)
		exported)))
      (format "%d exported functions:\n%s\n" count
	      (mapconcat 'identity (nreverse exported) "\n")))))

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
  (if (= beg end)
      "Empty abstract code section\n"
    (beam-file-mode--erlang-output-to-string
     "case catch binary_to_term(list_to_binary(X)) of
    {raw_abstract_v1, Forms} ->
        Src = erl_prettypr:format(erl_syntax:form_list(tl(Forms))),
        io:format(\"~s~n\", [Src]);
    Other ->
        io:format(\"This doesn't look like abstract code:~n~p~n\", [Other])
end"
     beg end)))

(defun beam-file-mode--handle-chunk-Docs (beg end)
  ;; checkdoc-params: (beg end)
  "Handle documentation chunk as per EEP 48."
  (beam-file-mode--erlang-output-to-string "
case binary_to_term(list_to_binary(X)) of
    {docs_v1, Anno, BeamLanguage, Format, ModuleDoc, Metadata, Docs} ->
        io:format(\"Documentation for ~s module written in ~s format.~n~n\", [BeamLanguage, Format]),
        if is_atom(ModuleDoc) ->
            io:format(\"Module documentation: ~s~n~n\", [ModuleDoc]);
           is_map(ModuleDoc) ->
            maps:map(fun(Language, Doc) -> io:format(\"In language '~s': ~ts~n~n\", [Language, Doc]) end, ModuleDoc)
        end,
        maps:map(fun(Key, Value) -> io:format(\"~p: ~p~n\", [Key, Value]) end, Metadata),
        io:format(\"~n\"),
        lists:foreach(
          fun({{Kind, Name, Arity}, _Anno, Signature, EntryDoc, EntryMetadata}) ->
              io:format(\"~s ~s/~b: ~ts~n~n\", [Kind, Name, Arity, << <<Sig/binary, \" \">> || Sig <- Signature >>]),
              maps:map(fun(Key, Value) -> io:format(\"~p: ~p~n\", [Key, Value]) end, EntryMetadata),
              io:format(\"~n\"),
              if is_atom(EntryDoc) ->
                  io:format(\"Entry documentation: ~s~n~n\", [EntryDoc]);
                 is_map(EntryDoc) ->
                  maps:map(fun(Language, Doc) -> io:format(\"In language '~s': ~ts~n~n\", [Language, Doc]) end, EntryDoc)
              end,
              io:format(\"~n\")
          end, Docs)
end"
     beg end))

(defun beam-file-mode--handle-hipe (beg end chunk-name)
  "Handle HiPE chunk."
  (let ((chunk-name-archs
         ;; from hipe_unified_loader.erl
         '(("HA64" . "x86_64")
           ("HARM" . "arm, v5, 32-bit")
           ("HPPC" . "PowerPC, 32-bit")
           ("HP64" . "ppc64")
           ("HS8P" . "SPARC, V8+, 32-bit")
           ("HX86" . "x86"))))
    (format "HiPE compiled code for %s\n%s"
            (or (cdr (assoc chunk-name chunk-name-archs)) "unknown architecture")
            (beam-file-mode--erlang-output-to-string "
[{Version, CheckSum} | _] = binary_to_term(list_to_binary(X)),
io:format(\"Version: ~s~nCheckSum: ~w~n\", [Version, CheckSum])" beg end))))

(dolist (chunk-name '("HA64" "HARM" "HPPC" "HP64" "HS8P" "HX86"))
  (fset (intern (concat "beam-file-mode--handle-chunk-" chunk-name))
        (lambda (beg end)
          (beam-file-mode--handle-hipe beg end chunk-name))))

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
