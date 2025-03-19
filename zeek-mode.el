;;; zeek-mode.el --- A major-mode for the Zeek scripting language -*- lexical-binding: t -*-

;; Copyright (c) 2022-now, The Regents of the University of California.
;; Author: Christian Kreibich (christian@zeek.org)
;; URL: https://github.com/zeek/emacs-zeek-mode
;; Package-Requires: ((emacs "24.4"))
;; Version 1.0.0-25

;;; Commentary:
;; This is an Emacs major-mode for Zeek scripts.  Supported features:
;;
;; - Syntax highlighting
;;
;; - Script formatting and parsing via zeekscript, when available: C-c C-f
;;   formats the current buffer; C-c C-p parses it and renders the parse tree
;;   into a new buffer.
;;
;; - Whitespace configuration: TAB always inserts tab character.  The mode also
;;   highlights trailing whitespace as well as spaces used after tabs.
;;
;; Automatic indentation isn't yet supported, but use of 'zeek-script'
;; for formatting approximates it.

;;; Code:

(defvar zeek-mode-hook nil)

;; For access to string-trim below.
(eval-when-compile (require 'subr-x))

(add-to-list 'auto-mode-alist '("\\.bro\\'" . zeek-mode))
(add-to-list 'auto-mode-alist '("\\.zeek\\'" . zeek-mode))
(add-to-list 'interpreter-mode-alist '("zeek" . zeek-mode))

;; Crude way to match "env (args) zeek (args) --", i.e. the construct
;; to allow Zeek use in a shebang line.
(add-to-list 'magic-mode-alist '("#![ \t]*.*/bin/env.+[ \t]zeek.+--[ \t]*$" . zeek-mode))

;; ---- Syntax Highlighting --------------------------------------------

(defvar zeek-mode-keywords
  `(("\\(@[^#\n]+\\)" (0 font-lock-preprocessor-face t))
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
    ("\\(&[a-zA-Z_0-9]+\\)" (0 font-lock-builtin-face)))
  "Keyword highlighting spec for Zeek mode.")

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
  "Syntax table for Zeek mode.")

;; ---- Zeek script formatting and parsing via "zeek-script" -----------
;;
;; This requires the zeekscript Python package, see here for details:
;; https://github.com/zeek/zeekscript

(defvar zeek-script
  (executable-find "zeek-script")
  "Full path to the `zeek-script' command.

If nil, please install the zeekscript Python package and ensure its
`zeek-script' command is in your PATH.")

(defun zeek--script-cmd (&rest args)
  "Return the full `zeek-script' invocation string for the given ARGS."
  (when zeek-script
    (mapconcat 'identity (cons zeek-script args) " ")))

(defun zeek--command-on-buffer (command destination &optional replace keep-errbuf)
  "Run a COMMAND on the buffer and report errors to buffer in DESTINATION.

This is a wrapper around `shell-command-on-region', with a subset
of its arguments.  The stderr stream of the command ends up in
buffer *zeek command errors* and its first line gets reported
to the echo area.  By default the buffer is temporary and killed
upon return, but the KEEP-ERRBUF argument, when t, preserves it.  REPLACE
causes the buffer to be removed and replaced if it already exists."
  (when zeek-script
    (let ((errbuf "*zeek-script errors*"))

      (when (get-buffer errbuf)
        (kill-buffer errbuf))

      ;; Run the given command on the buffer, plugging in stdout and stderr
      ;; destination buffers.
      (shell-command-on-region (point-min) (point-max)
                               command destination replace errbuf)

      (when (get-buffer errbuf)
        (with-current-buffer errbuf
          (goto-char (point-min))
          (message (string-trim (thing-at-point 'line))))
        (unless keep-errbuf (kill-buffer errbuf))))))

;;;###autoload
(defun zeek-format-buffer ()
  "Format the current buffer using `zeek-script format'."
  (interactive)
  (when zeek-script
    (let (;; Count non-whitespace characters up to point. Formatting only
          ;; modifies whitespace, so we can use this count to place point at the
          ;; "same" location after formatting.
          (numchars (how-many "[^ \t\r\n]" (point-min) (point)))

          ;; Count number of lines that point is currently down from the top of
          ;; the visible window.
          (toplines (count-lines (window-start) (point))))

      ;; Format the whole buffer, replacing its content.
      (zeek--command-on-buffer (zeek--script-cmd "format" "-") (current-buffer) t t)

      ;; Put point back to "same" spot: search for "numchars" instances of
      ;; optional whitespace, a single non-whitespace, plus optional
      ;; whitespace. (The latter is necessary to make point land correctly right
      ;; after any whitespace.) The search leaves point at the end of the search
      ;; result, i.e. where we want to be.
      (goto-char (point-min))
      (re-search-forward "[ \t\r\n]*[^ \t\r\n][ \t\r\n]*" nil t numchars)

      ;; Point is now correct, but we may have scrolled the window.  Re-scroll
      ;; so point is back on the same number of lines down from the top of the
      ;; window that it was before formatting.
      (recenter-top-bottom toplines))))

;;;###autoload
(defun zeek-parse-buffer ()
  "Report parse tree via `zeek-script parse' in a new buffer.

Potential parsing problems appear in the echo area and are
reflected in the parse tree."
  (interactive)
  (when zeek-script
    (let ((outbuf "*zeek-script parse tree*"))
      (zeek--command-on-buffer (zeek--script-cmd "parse" "-") outbuf)
      (switch-to-buffer-other-window outbuf)
      (special-mode))))

;;;###autoload
(defun zeek-format-before-save ()
  "Add this to .emacs to run zeek-format on the current buffer when saving:
\(add-hook \\='before-save-hook #\\='zeek-format-before-save)"
  (interactive)
  (when zeek-script
    (when (eq major-mode 'zeek-mode) (zeek-format-buffer))))

(when zeek-script
  (add-hook 'zeek-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-f") #'zeek-format-buffer)
              (local-set-key (kbd "C-c C-p") #'zeek-parse-buffer))))

;; ---- Main definitions -----------------------------------------------

;; We add whitespace minor mode by default and configure it to only show us
;; spaces after tabs or right from the start of a line. The face is called
;; whitespace-space-after-tab.
(require 'whitespace)
(add-hook 'zeek-mode-hook 'whitespace-mode)
(add-hook 'zeek-mode-hook
          (lambda ()
            (setq whitespace-space-after-tab-regexp '("^\t*\\( +\\)"))
            (setq whitespace-style '(face space-after-tab))))

;; The update-changes script maintains this version number; do not edit.
(defconst zeek-mode-version "1.0.0-25"
  "The current version of Zeek mode.")

(defun zeek-mode ()
  "Major mode for editing Zeek scripts."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table zeek-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(zeek-mode-keywords))
  (set (make-local-variable 'comment-start) "#")

  (setq indent-tabs-mode t)
  (setq tab-width 8)
  (local-set-key (kbd "TAB") 'self-insert-command)

  (setq major-mode 'zeek-mode)
  (setq mode-name "Zeek")
  (run-hooks 'zeek-mode-hook))

(provide 'zeek-mode)

;;; zeek-mode.el ends here
