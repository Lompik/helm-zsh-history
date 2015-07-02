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

;; browse your zsh history with helm usung '(helm-zsh-history)'

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; helm-shell-history (ZSH)

(defvar helm-shell-history-file
  (shell-command-to-string "~/.zsh_history" )
  "Specify your the history filepath of zsh.")



(setq helm-shell-history-command
  (lambda (pattern)
    (let* ((patterns (split-string pattern))
           (grep (when (string< "" pattern)
                   (helm-shell-history-make-grep-command patterns))))
      (mapconcat 'identity (delq nil
                                 `(,(concat "\\tac " helm-shell-history-file)
				   ,(concat "\\perl -lne 'use POSIX qw(strftime);m#: (\\d+):\\d+;(.+)# && printf \"%s :: %s\\n\",strftime(\"%a_%d_%b_%Y_%H:%M:%S\",localtime \$1),\$2'" )
                                   ,grep
                                   ))
                 " | "))))


(defvar helm-shell-history-command
  (lambda (pattern)
    (let* ((patterns (split-string pattern))
           (grep (when (string< "" pattern)
                   (helm-shell-history-make-grep-command patterns))))
      (mapconcat 'identity (delq nil
                                 `(,(concat "\\tac " helm-shell-history-file)
                                   ,grep
                                   "\\sed 's/^: [0-9]*:[0-9];//'"))
                 " | "))))


(defun helm-shell-history-make-grep-command (patterns)
  "Return grep command form PATTERNS."
  (cl-loop with cmd = "\\grep -E -e "
           for search-word in patterns
           collect (concat cmd " \"" search-word "\" ") into result
           finally return (mapconcat 'identity result " | ")))

(defvar helm-c-shell-history
  '((name . "helm-shell-history")
    (candidates-process . (lambda ()
                            (start-process
                             "helm-shell-history-process" nil "/bin/zsh" "-c"
                             (funcall helm-shell-history-command
                                      helm-pattern))))
    (nohighlight)
    (candidates-in-buffer)
    (action . (lambda (line)
                (funcall helm-shell-history-action-function line)))
    (delayed)))

(setq helm-shell-history-action-function
  (lambda (line)
    (cl-case major-mode
      (term-mode (term-send-raw-string line))
      (t         (insert (cadr (split-string line ":: ")))))))



(defun helm-zsh-history ()
  "Display command line history from history file.
You can specify at `helm-shell-history-file'."
  (interactive)
  (helm :sources helm-c-shell-history
        :prompt "shell command: "
        :buffer "*helm shell history*"))



(provide 'helm-zsh-history)
;;; helm-zsh-history.el ends here
