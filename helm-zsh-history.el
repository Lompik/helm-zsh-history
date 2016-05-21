;;; helm-zsh-history.el --- Helm interface for zsh command history  -*- lexical-binding: t; -*-

;; Copyright (C) 2015

;; Author:  <lompik@ArchOrion>
;; Keywords: convenience, terminals

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

;; browse your zsh history with helm using '(helm-zsh-history)'

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm-shell-history (ZSH)

(require 'helm)

(defvar helm-zsh-history-file "~/.zsh_history"
  "Specify your the history filepath of zsh.")

(defvar helm-zsh-history-separator " :: ")

(defvar helm-zsh-history-command
  (lambda (pattern)
    (let* ((patterns (split-string pattern))
           (grep (when (string< "" pattern)
                   (helm-zsh-history-make-grep-command patterns))))
      (mapconcat 'identity (delq nil
                                 `(,(concat "\\tac " helm-zsh-history-file)
				   ,(concat "\\perl -lne 'use POSIX qw(strftime);m#: (\\d+):\\d+;(.+)# && printf \"%s" helm-zsh-history-separator "%s\\n\",strftime(\"%a_%d_%b_%Y_%H:%M:%S\",localtime \$1),\$2'" )
                                   ,grep
                                   ))
                 " | "))))

(defun helm-zsh-history-make-grep-command (patterns)
  "Return grep command form PATTERNS."
  (cl-loop with cmd = "\\grep --color=always -i -a -E"
           for search-word in patterns
           collect (concat cmd " \"" search-word "\" ") into result
           finally return (mapconcat 'identity result " | ")))

;(defvar helm-zsh-history-limit 1000)

(defun helm-zsh-history--filter-candidate-1 (candidate)
  (let* ((ansi-p (string-match-p helm--ansi-color-regexp candidate))
         (line   (if ansi-p (helm--ansi-color-apply candidate) candidate))
         (split  (split-string line helm-zsh-history-separator))
         (date (nth 0 split))
         (str    (nth 1 split))
         )
    (if (and date str)
        (cons (concat (propertize date 'face 'helm-grep-file )
                      helm-zsh-history-separator
                      str
                      )
              line)
      "")))

(defun helm-zsh-history-filter-one-by-one (candidate)
  "`filter-one-by-one' transformer function for `helm-do-grep'."
  (if (consp candidate)
      ;; Already computed do nothing (default as input).
      candidate
    (and (stringp candidate)
         (helm-zsh-history--filter-candidate-1 candidate))))

(defun helm-zsh-history-init ()
  (let ((cmd-line (funcall helm-zsh-history-command
                           helm-pattern)))
    (prog1
        (start-file-process-shell-command
         "helm-zsh-history-process" helm-buffer cmd-line)
      (set-process-sentinel
       (get-buffer-process helm-buffer)
       (lambda (_process event)
         (when (string= event "finished\n")
           (with-helm-window
             (setq mode-line-format
                   '(" " mode-line-buffer-identification " "
                     (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                     (:eval (propertize
                             (format
                              "[%s process finished - (no results)] "
                              "x-path-helper")
                             'face 'helm-grep-finish))))
             (force-mode-line-update))))))))

(defvar helm-c-zsh-history
  (helm-build-async-source "helm-zsh-history"

    :candidates-process (lambda () (helm-zsh-history-init))
    :filter-one-by-one 'helm-zsh-history-filter-one-by-one
    :candidate-number-limit 9999
    :action (helm-make-actions
             "Insert line(s)"  (lambda (lines)
                         (mapc 'helm-zsh-history-action-function  (let ((marked (helm-marked-candidates)))
                                                                    (if (> (length marked) 1)
                                                                        (mapcar #'(lambda (line)
                                                                                   (concat line "\n"))
                                                                                marked)
                                                                      marked)))))))

(defun helm-zsh-history-action-function (line)
  (cl-case major-mode
    (term-mode (term-send-raw-string (concat (cadr (split-string line helm-zsh-history-separator)) "; ")))
    (t         (insert  (cadr (split-string line helm-zsh-history-separator)) ))))

(defun helm-zsh-history ()
  "Display command line history from history file.
You can specify at `helm-zsh-history-file'."
  (interactive)
  (if (and (executable-find "tac") (executable-find "perl"))
      (helm :sources helm-c-zsh-history
	    :prompt "shell command: "
            :map helm-map
	    :buffer "*helm zsh history*")
    (message "Cannot find perl and tac executables")))

(provide 'helm-zsh-history)
;;; helm-zsh-history.el ends here
