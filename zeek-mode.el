;;; zeek-mode.el --- A major-mode for the Zeek scripting language -*- lexical-binding: t -*-

;; Copyright (c) 2022-now, The Regents of the University of California.
;; Author: Christian Kreibich (christian@zeek.org)
;; URL: https://github.com/zeek/emacs-zeek-mode
;; Package-Requires: ((emacs "24.4"))
;; Version 1.0.0-29

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

;; ---- Syntax Highlighting --------------------------------------------

(defconst zeek--keywords-constants
  '("const" "option" "redef"))

(defconst zeek--font-lock-constants
  (list
   (rx-to-string
    `(: symbol-start
        (or ,@zeek--keywords-constants)
        symbol-end))
   '(0 font-lock-constant-face))
  "Zeek constant keywords.")

(defconst zeek--keywords-types
  '("addr" "any" "bool" "count" "counter" "double" "enum" "file" "int" "interval"
    "list" "net" "opaque" "paraglob" "pattern" "port" "record" "set" "string"
    "subnet" "table" "timer" "time" "union" "vector"))

(defconst zeek--font-lock-types
  (list
   (rx-to-string
    `(: symbol-start
        (or ,@zeek--keywords-types)
        symbol-end))
   '(0 font-lock-type-face))
  "Zeek constant keywords.")

(defconst zeek--keywords-keywords
  '("add" "alarm" "break" "case" "default" "delete" "else" "event" "export"
    "fmt" "for" "function" "global" "global_attr" "hook" "if" "in" "local"
    "match" "module" "next" "of" "print" "return" "schedule" "switch" "this"
    "type" "using" "when"))

(defconst zeek--font-lock-keywords
  (list
   (rx-to-string
    `(: symbol-start
        (or ,@zeek--keywords-keywords)
        symbol-end))
   '(0 font-lock-keyword-face))
  "Zeek constant keywords.")

(defconst zeek--keywords-functions
  '("day" "days" "hr" "hrs" "min" "mins" "sec" "secs" "msec" "msecs" "usec" "usecs"))

(defconst zeek--font-lock-functions
  (list
   (rx-to-string
    `(: symbol-start
        (or ,@zeek--keywords-functions)
        symbol-end))
   '(0 font-lock-function-name-face))
  "Zeek constant keywords.")

(defconst zeek--font-lock-preprocessor
  (list
   (rx-to-string
    `(: "@" (1+ (any "a-z" "A-Z"))))
   '(0 font-lock-preprocessor-face))
  "Zeek for script preprocessor commands starting with @.")

(defconst zeek--font-lock-builtin
  (list
   (rx-to-string
    `(: "&" (1+ (any "a-z" "A-Z" "0-9"))))
   '(0 font-lock-builtin-face))
  "Zeek face for attributes starting with &.")

(defconst zeek-font-lock-defaults
  (list zeek--font-lock-constants
        zeek--font-lock-keywords
        zeek--font-lock-types
        zeek--font-lock-functions
        zeek--font-lock-preprocessor
        zeek--font-lock-builtin)
  "Zeek font lock definitons.")

(defun zeek--setup-font-lock ()
  "Set up `font-lock-defaults' for `zeek-mode'."
  (setq font-lock-defaults '(zeek-font-lock-defaults)))

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
(defconst zeek-mode-version "1.0.0-29"
  "The current version of Zeek mode.")

;;;###autoload
(define-derived-mode zeek-mode prog-mode "Zeek"
  "Major mode for editing Zeek scripts."
  :syntax-table zeek-mode-syntax-table
  (setq-local comment-start "#")
  (setq-local indent-tabs-mode t)
  (setq-local tab-width 8)
  (local-set-key (kbd "TAB") 'self-insert-command)
  (zeek--setup-font-lock)
  (run-hooks 'zeek-mode-hook))

;; Map the file extensions over to zeek-mode.
(add-to-list 'auto-mode-alist '("\\.bro\\'" . zeek-mode))
(add-to-list 'auto-mode-alist '("\\.zeek\\'" . zeek-mode))
(add-to-list 'interpreter-mode-alist '("zeek" . zeek-mode))

;; Crude way to match "env (args) zeek (args) --", i.e. the construct
;; to allow Zeek use in a shebang line.
(add-to-list 'magic-mode-alist '("#![ \t]*.*/bin/env.+[ \t]zeek.+--[ \t]*$" . zeek-mode))

(provide 'zeek-mode)

;;; zeek-mode.el ends here
