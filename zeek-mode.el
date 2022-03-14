;; An Emacs mode for the Zeek scripting language.

(defvar zeek-mode-hook nil)

;; For access to string-trim below.
(eval-when-compile (require 'subr-x))

;; (defvar zeek-mode-map
;;   (let ((zeek-mode-map (make-keymap)))
;;     (define-key zeek-mode-map "\C-j" 'newline-and-indent)
;;     zeek-mode-map)
;;   "Keymap for ZEEK major mode")

(add-to-list 'auto-mode-alist '("\\.bro\\'" . zeek-mode))
(add-to-list 'auto-mode-alist '("\\.zeek\\'" . zeek-mode))
(add-to-list 'interpreter-mode-alist '("zeek" . zeek-mode))

;; Crude way to match "env (args) zeek (args) --", i.e. the construct
;; to allow Zeek use in a shebang line.
(add-to-list 'magic-mode-alist '("#![ \t]*.*/bin/env.+[ \t]zeek.+--[ \t]*$" . zeek-mode))

;; ---- Syntax Highlighting --------------------------------------------

(defvar zeek-mode-keywords
  `(("\\(@[^#\n]+\\)" (0 font-lock-doc-face))
    (,(concat "\\<"
	      (regexp-opt '("const" "option" "redef") t)
	      "\\>") (0 font-lock-constant-face))
    (,(concat "\\<"
	      (regexp-opt '("addr" "any" "bool" "count" "counter" "double"
                            "enum" "file" "int" "interval" "list" "net"
                            "opaque" "paraglob" "pattern" "port" "record"
                            "set" "string" "subnet" "table" "timer" "time"
                            "union" "vector") t)
	      "\\>") (0 font-lock-type-face))
    (,(concat "\\<"
	      (regexp-opt '("add" "alarm" "break" "case" "default"
			    "delete" "else" "event" "export" "fmt" "for"
			    "function" "global" "global_attr" "hook" "if" "in"
			    "local" "match" "module" "next" "of" "print"
			    "return" "schedule" "switch" "this" "type"
			    "using" "when") t)
	      "\\>") (0 font-lock-keyword-face))
    (,(concat "\\<"
	      (regexp-opt '("day" "days" "hr" "hrs" "min" "mins" "sec" "secs"
			    "msec" "msecs" "usec" "usecs") t)
	      "\\>") (0 font-lock-function-name-face))
    ("\\(&[a-zA-Z_0-9]+\\)" (0 font-lock-builtin-face))
    )
  "Keyword highlighting spec for Zeek mode")

(font-lock-add-keywords 'zeek-mode zeek-mode-keywords)

;; ---- The Syntax Table -----------------------------------------------

(defvar zeek-mode-syntax-table
  (let ((zeek-mode-syntax-table (make-syntax-table)))

    ;; Additional valid token characters
    (modify-syntax-entry ?_ "w" zeek-mode-syntax-table)
    (modify-syntax-entry ?. "w" zeek-mode-syntax-table)
    (modify-syntax-entry ?& "w" zeek-mode-syntax-table)

    ;; Make $ a punctuation character
    (modify-syntax-entry ?$ "."  zeek-mode-syntax-table)

    ;; Comment starts/ends in the Zeek language.
    ;; Does not distinguish Zeekygen comments.
    (modify-syntax-entry ?# "<" zeek-mode-syntax-table)
    (modify-syntax-entry ?\n ">" zeek-mode-syntax-table)

    zeek-mode-syntax-table)
  "Syntax table for Zeek mode")

;; ---- Zeek script formatting and parsing via "zeek-script" -----------
;;
;; This requires the zeekscript Python package, see here for details:
;; https://github.com/ckreibich/zeekscript

(defvar zeek-script
  (executable-find "zeek-script")
  "Full path to the zeek-script command.

If nil, please install the zeekscript Python package and ensure its
zeek-script command is in your PATH.")

(when zeek-script

  (defun zeek-script-cmd (&rest args)
    "Returns full zeek-script invocation string for the given arguments."
    (mapconcat 'identity (cons zeek-script args) " "))

  (defun zeek-command-on-buffer (command destination &optional replace keep-errbuf)
    "Run a command on the buffer and report errors to echo area.

This is a wrapper around shell-command-on-region, with a subset
of its arguments. The stderr stream of the command ends up in
buffer *zeek command errors* and its first line gets reported
to the echo area. By default the buffer is temporary and killed
upon return, but the keep-errbuf argument, when t, preserves it."
    (let ((errbuf "*zeek-script errors*"))

      ;; Run the given command on the buffer, plugging in stdout and stderr
      ;; destination buffers.
      (shell-command-on-region (point-min) (point-max)
                               command destination replace errbuf)

      (when (get-buffer errbuf)
        (with-current-buffer errbuf
          (goto-char (point-min))
          (message (string-trim (thing-at-point 'line))))
        (unless keep-errbuf (kill-buffer errbuf)))))

  (defun zeek-format-buffer ()
    "Format the current buffer using `zeek-script format'."
    (interactive)
    (let (;; Count non-whitespace characters up to point. Formatting only
          ;; modifies whitespace, so we can use this count to place point at the
          ;; "same" location after formatting.
          (numchars (how-many "[^ \t\r\n]" (point-min) (point)))

          ;; Count number of lines that point is currently down from the top of
          ;; the visible window.
          (toplines (count-lines (window-start) (point))))

      ;; Format the whole buffer, replacing its content.
      (zeek-command-on-buffer (zeek-script-cmd "format" "-") (current-buffer) t t)

      ;; Put point back to "same" spot: search for "numchars" instances of
      ;; optional whitespace, a single non-whitespace, plus optional
      ;; whitespace. (The latter is necessary to make point land correctly right
      ;; after any whitespace.) The search leaves point at the end of the search
      ;; result, i.e. where we want to be.
      (goto-char (point-min))
      (re-search-forward "[ \t\r\n]*[^ \t\r\n][ \t\r\n]*" nil nil numchars)

      ;; Point is now correct, but we may have scrolled the window.  Re-scroll
      ;; so point is back on the same number of lines down from the top of the
      ;; window that it was before formatting.
      (recenter-top-bottom toplines)))

  (defun zeek-parse-buffer ()
    "Report parse tree via `zeek-script parse' in a new buffer.

Potential parsing problems appear in the echo area and are
reflected in the parse tree."
    (interactive)
    (zeek-command-on-buffer (zeek-script-cmd "parse" "-") "*zeek-script parse tree*"))

  (add-hook 'zeek-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-f") 'zeek-format-buffer)
              (local-set-key (kbd "C-c C-p") 'zeek-parse-buffer))))

;; ---- Main definitions -----------------------------------------------

;; We add whitespace minor mode by default and configure it to only show us
;; spaces after tabs or right from the start of a line. The face is called
;; whitespace-space-after-tab.
(add-hook 'zeek-mode-hook 'whitespace-mode)
(add-hook 'zeek-mode-hook
          (lambda ()
            (setq whitespace-space-after-tab-regexp '("^\t*\\( +\\)"))
            (setq whitespace-style '(face space-after-tab))
            ))

(defun zeek-mode ()
  "Major mode for editing Zeek scripts"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table zeek-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(zeek-mode-keywords))

  (setq indent-tabs-mode t)
  (setq c-basic-offset 8)
  (setq tab-width 8)
  (local-set-key (kbd "TAB") 'self-insert-command)

  (setq major-mode 'zeek-mode)
  (setq mode-name "Zeek")
  (run-hooks 'zeek-mode-hook))

(provide 'zeek-mode)
