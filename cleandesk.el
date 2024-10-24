;; Copyright (C) 2024 Free Software Foundation, Inc.

;;; cleandesk.el --- Quickly process files across directories 

;; Maintainer: René Trappel <rtrappel@gmail.com>
;; URL: https://github.com/rtrppl/cleandesk
;; Version: 0.4
;; Package-Requires: ((emacs "26"))
;; Keywords: files

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

;; cleandesk.el allows for quickly processing files across directories.
;;
;;
;;; Code:

(require 'dired)  ; For dired-get-marked-files
(require 'json)   ; For json-encode

(defvar cleandesk-inbox-folder "~/")  ;; Using the home folder as a default starting point
(defvar cleandesk-date-string "%Y_%m_%d-%H%M%S-")
(defvar cleandesk-search-tool "fd") ;; choose between find and fd; fd is much faster and standard
(defvar cleandesk-fd-search-string "-t d --no-hidden .")
(defvar cleandesk-find-search-string "-type d ! -name '.*' | sed 's@//@/@'")
(defvar cleandesk-folders nil "All folders cleandesk operates on.")
(defvar cleandesk-name-directory (make-hash-table :test 'equal))
(defvar cleandesk-data-folders nil "Includes all folders that cleandesk should opperate on (excluding subfolders).")

(defun cleandesk-is-mac-p ()
  "Return t if the current system is a Mac (Darwin)."
;; Cleandesk needs to check this because cleandesk-search uses mdfind (Mac-only).
  (eq system-type 'darwin))

(defun cleandesk-prepapre-folder-list ()
 "Preparing a list of all folders in the cleandesk folders for further tasks."
 (cleandesk-get-folder-list)
 (setq cleandesk-folders "")
 (setq cleandesk-data-folders (hash-table-values cleandesk-name-directory))
   (dolist (cleandesk-data-folder cleandesk-data-folders)
     (with-temp-buffer
       (when (string-equal cleandesk-search-tool "fd")
	 (insert (shell-command-to-string (concat "fd " cleandesk-fd-search-string " '" cleandesk-data-folder "' "))))
       (when (string-equal cleandesk-search-tool "find")
	 (insert (shell-command-to-string (concat "find " cleandesk-data-folder " " cleandesk-find-search-string))))
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
 "Opens the inbox folder in a Dired view."
 (interactive)
 (dired cleandesk-inbox-folder)
 (revert-buffer))
  
(defun cleandesk-move-files ()
  "Move marked files in Dired to a data folder."
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
 "Complete renaming of one or multiple marked files."
 (interactive)
 (let ((marked-files (dired-get-marked-files))
       (token (read-from-minibuffer "Please provide a token for the new filename: ")))
       (dolist (file marked-files)
	 (let* ((old-filename file)
		(extension (file-name-extension file))
		(old-directory (file-name-directory file))
		(time (format-time-string cleandesk-date-string (nth 5 (file-attributes file))))
		(new-filename (concat old-directory time token "." extension))
		(suffix "A")
		(last-char (aref suffix 0)))
	   (condition-case nil
	       (rename-file old-filename new-filename)
	     (file-error
	      (progn
		(while (file-exists-p new-filename)
		  (setq new-filename (concat old-directory time token "-" suffix "." extension))
		  (setq last-char (+ last-char 1))
		  (setq suffix (string last-char)))
		(rename-file old-filename new-filename)))))))
  (revert-buffer))

(defun cleandesk-simple-rename ()
 "Replacement for dired-do-rename for one or multiple marked files."
 (interactive)
 (let ((marked-files (dired-get-marked-files)))
       (dolist (file marked-files)
	 (let* ((old-filename file)
		(token (read-from-minibuffer "Please change filename: " file))
		(new-filename token))
	   (condition-case nil
	       (rename-file old-filename new-filename)
	     (file-error
	      (progn
		(while (file-exists-p new-filename)
		  (setq new-filename (read-from-minibuffer "Filename already exists! Please adjust : " token))
		  (rename-file old-filename new-filename))))))))
  (revert-buffer))


(defun cleandesk-prepend-date ()
 "Prepends cleandesk-date-string to one or multiple marked files."
 (interactive)
 (let ((marked-files (dired-get-marked-files)))
       (dolist (file marked-files)
	 (let* ((old-filename (file-name-nondirectory file))
		(old-directory (file-name-directory file))
		(time (format-time-string cleandesk-date-string (nth 5 (file-attributes file))))
		(new-filename (concat old-directory time old-filename)))
	(rename-file old-filename new-filename))))
  (revert-buffer))

(defun cleandesk-check-for-cd-file ()
 "Checks for Cleandesk file ~/.cleandesk-directory-list. It directs the user 
to cleandesk-add-folder in case there is none."
 (clrhash cleandesk-name-directory)
 (when (not (file-exists-p "~/.cleandesk-directory-list"))
   (read-char "You need to add at least one folder as a Cleandesk folder. Press any key to proceed.")
   (cleandesk-add-folder)))

(defun cleandesk-get-folder-list ()
 "Return cleandesk-name-directory. cleandesk-name-directory is a hashtable 
that includes a list of names and locations of all parent directories that 
Cleandesk considers."
 (clrhash cleandesk-name-directory)
 (cleandesk-check-for-cd-file)
 (with-temp-buffer
   (insert-file-contents "~/.cleandesk-directory-list")
   (if (fboundp 'json-parse-buffer)
       (setq cleandesk-name-directory (json-parse-buffer)))))

(defun cleandesk-add-folder ()
  "Add a directory to the list of Cleandesk directories."
  (interactive)
  (clrhash cleandesk-name-directory)
  (let* ((new-directory (expand-file-name (read-directory-name "Enter a directory name: ")))
	 (name (read-from-minibuffer "Please provide a name for the new Cleandesk directory: ")))
    (if (yes-or-no-p (format "Are you sure you want to add %s as a new Cleandesk directory? " new-directory))
	(progn
	  (unless (file-exists-p new-directory)
	    (make-directory new-directory t))
	  (when (not (file-exists-p "~/.cleandesk-directory-list"))
	    (puthash name new-directory cleandesk-name-directory))
	  (when (file-exists-p "~/.cleandesk-directory-list")
	    (with-temp-buffer
	      (insert-file-contents "~/.cleandesk-directory-list")
	      (if (fboundp 'json-parse-buffer)
		  (setq cleandesk-name-directory (json-parse-buffer))))
	    (puthash name new-directory cleandesk-name-directory))
	    (with-temp-buffer
	     (let* ((json-data (json-encode cleandesk-name-directory)))
	       (insert json-data)
	       (write-file "~/.cleandesk-directory-list"))))
    (message "%s was not created!" new-directory))
  (clrhash cleandesk-name-directory)))

(defun cleandesk-remove-folder ()
  "Remove a directory from the list of Cleandesk directories."
  (interactive)
  (cleandesk-get-folder-list)
  (let* ((directories (hash-table-keys cleandesk-name-directory))
	(selection (completing-read "Which cleandesk directory should be removed? " directories)))
    (if (not (member selection directories))
	(message "Directory does not exist.")
      (if (yes-or-no-p (format "Are you sure you want to remove %s as a cleandesk directory? " (gethash selection cleandesk-name-directory)))
	  (progn
	    (remhash selection cleandesk-name-directory)
	    (when (not (eq (hash-table-count cleandesk-name-directory) 0)) 
	      (with-temp-buffer
		(let ((json-data (json-encode cleandesk-name-directory)))
		      (insert json-data))
		      (write-file "~/.cleandesk-directory-list")))
	    (when (eq (hash-table-count cleandesk-name-directory) 0)
	      (delete-file "~/.cleandesk-directory-list")))
	(clrhash cleandesk-name-directory)))))

(defun cleandesk-search (arg)
  "Search for all files containing a specific string in the current directory. 
This is based on mdfind/Spotlight. If called with C-u, search will expand to 
all Cleandesk directories."
  (interactive "P")
  (when (cleandesk-is-mac-p)
    (when (equal arg '(4))
    (cleandesk-get-folder-list)
    (let ((cleandesk-mdfind-folders (hash-table-values cleandesk-name-directory))
          (mdfind-search-string (read-from-minibuffer "Search for: "))
          cleandesk-search-results)  ; Initialize as empty list
      (dolist (cleandesk-mdfind-folder cleandesk-mdfind-folders)
	(with-temp-buffer
          (let ((cmd (concat "mdfind " mdfind-search-string " -onlyin " cleandesk-mdfind-folder)))
            (insert (shell-command-to-string cmd))
            (let ((findings (split-string (buffer-string) "\n" t)))
              (dolist (item findings)
		(when (and (stringp item)
                           (string-prefix-p "/" item))
                  (push item cleandesk-search-results)))))))
      (cleandesk-present-results cleandesk-search-results)))
    (when (null arg)
      (let ((current-folder default-directory)
            (mdfind-search-string (read-from-minibuffer "Search for: "))
            cleandesk-search-results) 
	(with-temp-buffer
          (let ((cmd (concat "mdfind " mdfind-search-string " -onlyin " current-folder)))
            (insert (shell-command-to-string cmd))
            (let ((findings (split-string (buffer-string) "\n" t)))
              (dolist (item findings)
		(when (and (stringp item)
                           (string-prefix-p "/" item))
                  (push item cleandesk-search-results))))))
	(cleandesk-present-results cleandesk-search-results)))
  (when (not (cleandesk-is-mac-p))
    (message "Unfortunately, Cleandesk-search currently requires mdfind (=Spotlight), which is macOS-only."))))

(defun cleandesk-present-results (results)
  "If there are results of the search they are presented in dired."
  (let* ((results (nreverse results)))
      (when results
	(push "*Cleandesk search findings*" results)
	(dired results))
      (when (not results)
	(message "Search has produced no results."))))

(provide 'cleandesk)

;;; cleandesk.el ends here
