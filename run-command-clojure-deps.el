;;; run-command-clojure-deps.el --- Run clojure commands from deps.edn files. -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Nils Grunwald <github.com/ngrunwald>
;; Author: Nils Grunwald
;; URL: https://github.com/ngrunwald/emacs-run-command-clojure-deps
;; Created: 2021
;; Version: 0.1.1
;; Keywords: clojure, clojurescript, shell, clj, deps
;; Package-Requires: ((parseedn "20200419.1124"))

;; This file is NOT part of GNU Emacs.

;; run-command-clj-deps.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; run-command-clj-deps.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with run-command-clj-deps.el.
;; If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides an easy way to launch shell commands from deps.edn files.

;;; Code:
(require 'seq)
(require 's)
(require 'parseedn)
(require 'cl-macs)
(require 'subr-x)

(defgroup run-command-clojure-deps nil
  "Easy way to launch shell commands from deps.edn files."
  :prefix "run-command-clojure-deps-")

(defconst run-command-clojure-deps-version "0.1.0")

(defun run-command-clojure-deps--valid-command-p (cmd)
  (and (gethash :command-name cmd)
       (gethash :command-line cmd)))

(defun alias->command (alias-name alias)
  (let* ((switch (cond ((gethash :exec-fn alias) "-X")
                       ((gethash :main-opts alias) "-M")))
         (display (gethash :emacs-run-command/display alias)))
    (when (and switch
               (not (gethash :emacs-run-command/skip alias)))
      (let* ((ht (make-hash-table :size 4))
             (alias-str (substring (symbol-name alias-name) 1)))
        (puthash :command-name alias-str ht)
        (puthash :command-line (format "clj %s%s" switch alias-str) ht)
        (puthash :display (format "[alias]%s"
                                  (if display (s-concat " " display) ""))
                 ht)
        ht))))

(defun run-command-clojure-deps--parse-deps-edn-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (let* ((content (car (parseedn-read)))
           (commands (gethash :emacs/run-commands content))
           (raw-aliases (gethash :aliases content))
           (selected-aliases (cl-loop for v being each hash-value of raw-aliases
                                      using (hash-key k)
                                      collect (alias->command k v))))
      (seq-concatenate 'list
                       (thread-last commands
                         (seq-filter #'run-command-clojure-deps--valid-command-p)
                         (seq-map (lambda (cmd) (puthash :display
                                                         (format "[cmd]%s" (s-concat " "
                                                                                     (gethash :display cmd "")))
                                                         cmd)
                                    cmd)))
                       (seq-filter #'identity selected-aliases)))))

(defun run-command-clj-commands-from-dir (deps-file-path project-dir)
  (let* ((default-directory project-dir)
         (project-cmds (run-command-clojure-deps--parse-deps-edn-file deps-file-path)))
    (seq-map (lambda (elt)
               (let* ((command-name (gethash :command-name elt)))
                 (list :command-name command-name
                       :command-line (gethash :command-line elt)
                       :working-dir project-dir
                       :display (format "%s → %s" command-name (gethash :display elt)))))
             project-cmds)))

(defun run-command-recipe-clj-project ()
  (when (derived-mode-p 'clojure-mode)
    (when-let ((project-dir (locate-dominating-file default-directory "deps.edn"))
               (deps-file-path (s-concat (file-name-as-directory project-dir) "deps.edn")))
      (run-command-clj-commands-from-dir deps-file-path project-dir))))

(defun run-command-recipe-clj-user ()
  (when (derived-mode-p 'clojure-mode)
    (if-let ((project-dir (locate-dominating-file default-directory "deps.edn")))
        (run-command-clj-commands-from-dir "~/.clojure/deps.edn" project-dir)
      (run-command-clj-commands-from-dir "~/.clojure/deps.edn" default-directory))))

;;;###autoload
(defun run-command-clojure-deps-register ()
  "Register recipes fr handling deps.edn files."
  (interactive)
  (with-eval-after-load 'run-command
    (add-to-list 'run-command-recipes 'run-command-recipe-clj-project)
    (add-to-list 'run-command-recipes 'run-command-recipe-clj-user)))

(provide 'run-command-clojure-deps)
;;; run-command-clojure-deps.el ends here
