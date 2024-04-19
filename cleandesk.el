;; cleandesk.el --- org-roam-replica or org-roam-ripgrep -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Maintainer: René Trappel <rtrappel@gmail.com>
;; URL:
;; Version: 0.1
;; Package-Requires: emacs "26", rg
;; Keywords: org-roam notes zettelkasten

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; cleandesk.el allows for quickly processing and refiling files across directories.
;;

(defvar cleandesk-data-folders '("/Users/renetrappel/Documents/"))

(defvar cleandesk-inbox-folder "/Users/renetrappel/Desktop/")

(defvar date-string "%Y_%m_%d-%H%M%S")

(defvar fd-search-string "fd -t d --no-hidden .")

(defun cleandesk-prepapre-folder-list ()
 "Preparing a list of all folders in the cleandesk folders for further tasks."
 (cleandesk-get-folder-list)
 (setq cleandesk-folders "")
 (setq cleandesk-data-folders (hash-table-values cleandesk-name-directory))
   (dolist (cleandesk-data-folder cleandesk-data-folders)
     (with-temp-buffer
       (insert (shell-command-to-string (concat fd-search-string " '" cleandesk-data-folder "' ")))
       (let ((temp-folders (split-string (buffer-string) "\n" t)))
	 (setq cleandesk-folders (append temp-folders cleandesk-folders)))))
	 (setq cleandesk-folders (append cleandesk-data-folders cleandesk-folders)))

(defun cleandesk-jump-to-folder ()
  "Selects a folder from the list of cleandesk folders and opens a dired view."
  (interactive)
  (cleandesk-prepapre-folder-list)
  (dired (completing-read "Select folder: " cleandesk-folders))
  (revert-buffer))
	 
(defun cleandesk-open-inbox ()
 "Opens the inbox folder in a dired view."
 (interactive)
 (dired cleandesk-inbox-folder)
 (revert-buffer))
  
(defun cleandesk-move-files ()
  "Move selected files in dired to a data folder."
  (interactive)
  (cleandesk-prepapre-folder-list)
  (let ((cleandesk-target-folder (completing-read "Target folder: " cleandesk-folders))
	(marked-files (dired-get-marked-files)))
    (dolist (file marked-files)
      (let ((old-filename file)
	    (new-filename (concat cleandesk-target-folder (file-name-nondirectory file))))
	(rename-file old-filename new-filename))))
  (revert-buffer))

(defun cleandesk-rename ()
 "Complete renaming of one or multiple files."
 (interactive)
 (let ((marked-files (dired-get-marked-files))
       (token (read-from-minibuffer "Please provide a token for the new filename: ")))
       (dolist (file marked-files)
	 (let* ((old-filename file)
		(extension (file-name-extension file))
		(old-directory (file-name-directory file))
		(time (format-time-string date-string (nth 5 (file-attributes file))))
		(new-filename (concat old-directory time "-" token "." extension)))
	(rename-file old-filename new-filename))))
  (revert-buffer))

(defun cleandesk-prepend-date ()
 "Prepends date-string to one or multiple files."
 (interactive)
 (let ((marked-files (dired-get-marked-files)))
       (dolist (file marked-files)
	 (let* ((old-filename (file-name-nondirectory file))
		(old-directory (file-name-directory file))
		(time (format-time-string date-string (nth 5 (file-attributes file))))
		(new-filename (concat old-directory time "-" old-filename)))
	(rename-file old-filename new-filename))))
  (revert-buffer))

(defun cleandesk-check-for-cd-file ()
 "Creates a cleandesk file in ~/.cleandesk-directory-list in case one does not yet exist."
 (when (not (file-exists-p "~/.cleandesk-directory-list"))
   (puthash "preset" ((expand-file-name cleandesk-data-folders) cleandesk-name-directory)
   (with-temp-buffer
	  (let ((json-data (json-encode cleandesk-name-directory)))
	    (insert json-data)
	    (write-file "~/.cleandesk-directory-list"))))))

(defun cleandesk-get-folder-list ()
 "Return cleandesk-name-directory, a hashtable that includes a list of names and locations of all directories."
 (setq cleandesk-name-directory (make-hash-table :test 'equal))
 (cleandesk-check-for-cd-file)
 (with-temp-buffer
   (insert-file-contents "~/.cleandesk-directory-list")
   (if (fboundp 'json-parse-buffer)
       (setq cleandesk-name-directory (json-parse-buffer)))))

(defun cleandesk-add-folder ()
  "Add a directory as a cleandesk directory."
  (interactive)
  (cleandesk-get-folder-list)
  (let* ((new-directory (expand-file-name (read-directory-name "Enter a directory name: "))))
    (if (yes-or-no-p (format "Are you sure you want to add %s as a new cleandesk directory? " new-directory))
	(progn
	  (unless (file-exists-p new-directory)
	    (make-directory new-directory t))
	  (let* ((name (read-from-minibuffer "Please provide a name for the new cleandesk directory: ")))
	    (puthash name new-directory cleandesk-name-directory)
	    (with-temp-buffer
	     (let* ((json-data (json-encode cleandesk-name-directory)))
	       (insert json-data)
	       (write-file "~/.cleandesk-directory-list")))))
    (message "%s was not created!" new-directory))
  (clrhash cleandesk-name-directory)))

(defun cleandesk-remove-folder ()
  "Allow to remove a cleandesk directory for the list of cleandesk directories."
  (interactive)
  (cleandesk-get-folder-list)
  (setq directories (hash-table-keys cleandesk-name-directory))
  (setq selection (completing-read "Which cleandesk directory should be removed? " directories))
  (if (not (member selection directories))
      (message "Directory does not exist.")
    (if (string-equal selection "main")
	(message "The directory \"main\" cannot be removed!")
      (if (yes-or-no-p (format "Are you sure you want to remove %s as a cleandesk directory? " (gethash selection cleandesk-name-directory)))
	  (progn
	    (remhash selection cleandesk-name-directory)
	    (with-temp-buffer
	      (setq json-data (json-encode cleandesk-name-directory))
	      (insert json-data)
	      (write-file "~/.cleandesk-directory-list"))
	    (setq org-directory (gethash "main" cleandesk-name-directory))))))
  (clrhash cleandesk-name-directory))