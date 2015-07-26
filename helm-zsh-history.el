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
; helm-shell-history (ZSH)

(defvar helm-zsh-history-file "~/.zsh_history" 
  "Specify your the history filepath of zsh.")



(defvar helm-zsh-history-command
  (lambda (pattern)
    (let* ((patterns (split-string pattern))
           (grep (when (string< "" pattern)
                   (helm-zsh-history-make-grep-command patterns))))
      (mapconcat 'identity (delq nil
                                 `(,(concat "\\tac " helm-zsh-history-file)
				   ,(concat "\\perl -lne 'use POSIX qw(strftime);m#: (\\d+):\\d+;(.+)# && printf \"%s :: %s\\n\",strftime(\"%a_%d_%b_%Y_%H:%M:%S\",localtime \$1),\$2'" )
                                   ,grep
                                   ))
                 " | "))))


(defun helm-zsh-history-make-grep-command (patterns)
  "Return grep command form PATTERNS."
  (cl-loop with cmd = "\\grep -E -e "
           for search-word in patterns
           collect (concat cmd " \"" search-word "\" ") into result
           finally return (mapconcat 'identity result " | ")))

;(defvar helm-zsh-history-limit 1000)

(defvar helm-c-zsh-history
  '((name . "helm-zsh-history")
    (candidates-process . (lambda ()
                            (start-process
                             "helm-zsh-history-process" nil "/bin/zsh" "-c"
                             (funcall helm-zsh-history-command
                                      helm-pattern))))
;    (canditate-number-limit . helm-zsh-history-limit)
    (nohighlight)
    (candidates-in-buffer)
    (action . (("Insert" . (lambda (lines)
                             (mapc 'helm-zsh-history-action-function  (helm-marked-candidates))))))
    (delayed)))


(defun helm-zsh-history-action-function (line)
  (cl-case major-mode
    (term-mode (term-send-raw-string (concat (cadr (split-string line ":: ")) "; ")))
    (t         (insert (concat (cadr (split-string line ":: ")) "\n")))))


(defun helm-zsh-history ()
  "Display command line history from history file.
You can specify at `helm-zsh-history-file'."
  (interactive)
  (if (and (executable-find "tac") (executable-find "perl"))
      (helm :sources helm-c-zsh-history
	    :prompt "shell command: "
	    :buffer "*helm zsh history*")
    (message "Cannot find perl and tac executables")))



(provide 'helm-zsh-history)
;;; helm-zsh-history.el ends here
