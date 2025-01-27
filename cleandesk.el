;; Copyright (C) 2024 Free Software Foundation, Inc.

;;; cleandesk.el --- Quickly process files across directories 

;; Maintainer: René Trappel <rtrappel@gmail.com>
;; URL: https://github.com/rtrppl/cleandesk
;; Version: 0.5.2
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
(defvar cleandesk-metadata-tool "fd") ;; choose between find and fd; fd is much faster and standard
(defvar cleandesk-fd-metadata-cmd "fd -t d --no-hidden .")
(defvar cleandesk-find-metadata-cmd "-type d ! -name '.*' | sed 's@//@/@'")
(defvar cleandesk-folders nil "A list with all folders cleandesk operates on.
This is a temporay variable used (and changed by most functions). It is not 
intended for the user to modify! If you want to specfiy folders cleandesk 
should operate on, please modify `cleandesk-data-folders'")
(defvar cleandesk-name-directory (make-hash-table :test 'equal))
(defvar cleandesk-data-folders nil "Includes all folders that cleandesk should opperate on (excluding subfolders).")
;; You do not need to set `cleandesk-data-folders' to begin working with 
;; cleandesk. If set to nil, this variable is modified by `cleandesk-add-folder' and
;; `cleandesk-remove-folder'. If this is set to any value other than nil,
;; these functions do not change `cleandesk-data-folders'.
(defvar cleandesk-dotfile "~/.cleandesk-directory-list")
;; The dotfile stores all `cleandesk-data-folders' and should only be 
;; modified via `cleandesk-add-folder' and `cleandesk-remove-folder'.
;; If `cleandesk-data-folders' are manually set, this variable can be
;; set to nil.
(defvar cleandesk-fd4tree-cmd "fd -t d --no-ignore -L .")
(defvar cleandesk-search-tool "mdfind") ;; choose between mdfind (macOS only), rg (text documents only) and rga (slower)
(defvar cleandesk-tree-cmd "tree -d -f -R ")

(defun cleandesk-is-mac-p ()
  "Return t if the current system is a Mac (Darwin)."
;; Cleandesk needs to check this because cleandesk-search uses mdfind (Mac-only).
  (eq system-type 'darwin))

(defun cleandesk-prepare-folder-list ()
 "Preparing a list of all sub-folders in `cleandesk-data-folders'."
 (let ((cleandesk-working-folders))
   (setq cleandesk-folders "")
   (when (not cleandesk-data-folders)
     (cleandesk-get-folder-list)
     (setq cleandesk-working-folders (hash-table-values cleandesk-name-directory)))
   (when cleandesk-data-folders
     (dolist (folder cleandesk-data-folders)
       (setq cleandesk-working-folders (append (list (expand-file-name folder)) cleandesk-working-folders))))
   (dolist (cleandesk-data-folder cleandesk-working-folders)
     (with-temp-buffer
       (when (string-equal cleandesk-metadata-tool "fd")
	 (insert (shell-command-to-string (concat cleandesk-fd-metadata-cmd " \""  cleandesk-data-folder "\" "))))
       (when (string-equal cleandesk-metadata-tool "find")
	 (insert (shell-command-to-string (concat "find " cleandesk-data-folder " " cleandesk-find-metadata-cmd))))
       (let ((temp-folders (split-string (buffer-string) "\n" t)))
	 (setq cleandesk-folders (append temp-folders cleandesk-folders)))))
   (when (not cleandesk-data-folders) 
     (setq cleandesk-folders (append cleandesk-working-folders cleandesk-folders)))
   (when cleandesk-data-folders
     (dolist (folder cleandesk-data-folders)
       (setq cleandesk-folders (append (list (expand-file-name folder)) cleandesk-folders))))))

(defun cleandesk-jump-to-folder ()
  "Selects a folder from the list of cleandesk folders and opens a dired view."
  (interactive)
  (cleandesk-prepare-folder-list)
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
  (cleandesk-prepare-folder-list)
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

(defun cleandesk-create-org-link-inline-image ()
 "Copy the org-link to one or multiple marked images to the kill-ring."
 (interactive)
 (let ((marked-files (dired-get-marked-files)))
   (with-temp-buffer 
     (dolist (file marked-files)
       (insert (concat "\[\[file:" file "\]\]\n")))
     (kill-new (buffer-substring (point-min) (point-max))))	 
  (revert-buffer)))

(defun cleandesk-create-org-link ()
 "Copy the org-link to one or multiple marked files to the kill-ring."
 (interactive)
 (let ((marked-files (dired-get-marked-files)))
   (with-temp-buffer 
     (dolist (file marked-files)
       (insert (concat "\[\[file:" file "\]\[" (file-name-nondirectory file) "\]\]\n")))
     (kill-new (buffer-substring (point-min) (point-max))))
  (revert-buffer)))

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
 (when (not (file-exists-p cleandesk-dotfile))
   (read-char "You need to add at least one folder as a Cleandesk folder. Press any key to proceed.")
   (cleandesk-add-folder)))

(defun cleandesk-get-folder-list ()
 "Return cleandesk-name-directory. cleandesk-name-directory is a hashtable 
that includes a list of names and locations of all parent directories that 
Cleandesk considers."
 (clrhash cleandesk-name-directory)
 (cleandesk-check-for-cd-file)
 (with-temp-buffer
   (insert-file-contents cleandesk-dotfile)
   (if (fboundp 'json-parse-buffer)
       (setq cleandesk-name-directory (json-parse-buffer)))))

(defun cleandesk-add-folder ()
  "Add a directory to the list of Cleandesk directories."
  (interactive)
    (when (not cleandesk-data-folders)
      (clrhash cleandesk-name-directory)
      (let* ((new-directory (expand-file-name (read-directory-name "Enter a directory name: ")))
	     (name (read-from-minibuffer "Please provide a name for the new Cleandesk directory: ")))
      (when (not (file-exists-p cleandesk-dotfile))
	(puthash name new-directory cleandesk-name-directory))
      (when (file-exists-p cleandesk-dotfile)
	(with-temp-buffer
	  (insert-file-contents cleandesk-dotfile)
	  (if (fboundp 'json-parse-buffer)
	      (setq cleandesk-name-directory (json-parse-buffer))))
	(puthash name new-directory cleandesk-name-directory))
      (with-temp-buffer
	(let* ((json-data (json-encode cleandesk-name-directory)))
	  (insert json-data)
	  (write-file cleandesk-dotfile)))))
  (when cleandesk-data-folders
    (message "cleandesk-add-folder does not work when cleandesk-data-folders has been set manually.")
  (clrhash cleandesk-name-directory)))

(defun cleandesk-remove-folder ()
  "Remove a directory from the list of Cleandesk directories."
  (interactive)
   (when (not cleandesk-data-folders)
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
		   (write-file cleandesk-dotfile)))
	       (when (eq (hash-table-count cleandesk-name-directory) 0)
		 (delete-file cleandesk-dotfile)))
	   (clrhash cleandesk-name-directory)))))
   (when cleandesk-data-folders
    (message "cleandesk-remove-folder does not work when cleandesk-data-folders has been set manually.")))

(defun cleandesk-search (arg)
  "Search for all files containing a specific string in the current directory. 
This is based on mdfind/Spotlight. If called with C-u, search will expand to 
all Cleandesk directories."
  (interactive "P")
  (when (cleandesk-is-mac-p)
    (when (equal arg '(4))
    (cleandesk-get-folder-list)
    (let ((cleandesk-search-folders (hash-table-values cleandesk-name-directory))
          (search-string (read-from-minibuffer "Search for: "))
          cleandesk-search-results)  ; Initialize as empty list
      (dolist (cleandesk-mdfind-folder cleandesk-search-folders)
	(with-temp-buffer
	  (when (string-equal cleandesk-search-tool "mdfind")
            (let ((cmd (concat "mdfind " search-string " -onlyin " cleandesk-mdfind-folder)))
              (insert (shell-command-to-string cmd))))
	  (when (string-equal cleandesk-search-tool "rga")
            (let ((cmd (concat "rga -l -e \"" search-string "\" " cleandesk-mdfind-folder)))
              (insert (shell-command-to-string cmd))))
	  (when (string-equal cleandesk-search-tool "rg")
            (let ((cmd (concat "rg -l -e \"" search-string "\" " cleandesk-mdfind-folder)))
              (insert (shell-command-to-string cmd))))
            (let ((findings (split-string (buffer-string) "\n" t)))
              (dolist (item findings)
		(when (and (stringp item)
                           (string-prefix-p "/" item))
                  (push item cleandesk-search-results))))))
      (cleandesk-present-results cleandesk-search-results))))
    (when (null arg)
      (let ((current-folder (shell-quote-argument (cleandesk-return-current-folder)))
            (search-string (read-from-minibuffer "Search for: "))
            cleandesk-search-results) 
	(with-temp-buffer
          (when (string-equal cleandesk-search-tool "mdfind")
	    (let* ((cmd (concat "mdfind " search-string " -onlyin " current-folder)))
              (insert (shell-command-to-string cmd))))
	  (when (string-equal cleandesk-search-tool "rga")
            (let* ((cmd (concat "rga -i -l -e \"" search-string "\" " current-folder)))
              (insert (shell-command-to-string cmd))))
	  (when (string-equal cleandesk-search-tool "rg")
            (let* ((cmd (concat "rg -i -l -e \"" search-string "\" " current-folder)))
	      (insert (shell-command-to-string cmd))))
          (let* ((findings (split-string (buffer-string) "\n" t)))
            (dolist (item findings)
	      (when (and (stringp item)
                         (string-prefix-p "/" item))
                  (push item cleandesk-search-results)))))
	(cleandesk-present-results cleandesk-search-results)))
  (when (not (cleandesk-is-mac-p))
    (message "Unfortunately, Cleandesk-search currently requires mdfind (=Spotlight), which is macOS-only.")))

(defun cleandesk-present-results (results)
  "If there are results of the search they are presented in dired."
  (let* ((results (nreverse results)))
      (when results
	(push "*Cleandesk search findings*" results)
	(dired results))
      (when (not results)
	(message "Search has produced no results."))))

(defun cleandesk-return-current-folder ()
  "Returns the current folder shown in Dired."
  (let* ((folder dired-directory)
	 (folder (expand-file-name folder)))
    folder))

(defun cleandesk-return-all-subfolders (folder)
   "Returns all subfolders for the directory `folder' as a hashtable."
  (let* ((cleandesk-subfolders (make-hash-table :test 'equal))
	 (subfolder-list))
    (with-temp-buffer
       (when (string-equal cleandesk-metadata-tool "fd")
	 (insert (shell-command-to-string (concat cleandesk-fd4tree-cmd " \"" folder "\" "))))
       (when (string-equal cleandesk-metadata-tool "find")
	 (insert (shell-command-to-string (concat "find " folder " " cleandesk-find-metadata-cmd))))
       (let ((temp-folders (split-string (buffer-string) "\n" t)))
	 (setq subfolder-list (append temp-folders subfolder-list))))
    (dolist (item subfolder-list)
      (when (and (stringp item)
                 (string-prefix-p "/" item))
	(let* ((value (directory-file-name item))
	       (value (file-name-nondirectory value))
	       (key (concat "\\" (directory-file-name item))))
	(puthash key value cleandesk-subfolders))))
    cleandesk-subfolders))

(defun cleandesk-show-tree ()
  "Presents a buffer for `tree' with directories as Orgmode links."
  (interactive)
  (let* ((folder (cleandesk-return-current-folder))
	 (tree-buffer (concat "**tree for " folder "**")))
   (with-current-buffer (get-buffer-create tree-buffer)
     (erase-buffer)
     (org-mode)
     (cleandesk-tree-buffer-mode)
     (switch-to-buffer tree-buffer)
     (insert (concat "# Use n, p to navigate, o to open in Dired, O to open using system default,\n# and q for exit.\n\n"))
     (insert (concat "\[\[file:" folder "\]\[" folder "\]\]\n\n"))  
     (insert (cleandesk-compile-tree folder))
     (goto-char (point-min))
     (org-next-link)
     (org-next-link))))

(defun cleandesk-compile-tree (folder)
  "Prepares the `tree' output for `cleandesk-show-tree'."
  (let* ((tree)
	(cleandesk-subfolders (cleandesk-return-all-subfolders folder))
	(keys (hash-table-keys cleandesk-subfolders)))
  (with-temp-buffer
    (insert (shell-command-to-string (concat cleandesk-tree-cmd " \"" folder "\" ")))
    (setq tree (buffer-string)))
  (dolist (item keys)
    (let* ((key item)
	   (to-be-replaced (replace-regexp-in-string "^\\\\" "" item))
	   (replacement (concat "\[\[file:" to-be-replaced "\]\[" (gethash key cleandesk-subfolders) "\]\]")))
    (setq tree (string-replace (concat "─ " to-be-replaced) (concat "─ "replacement) tree))
    (setq tree (replace-regexp-in-string "^[^├└│].*\n?" "" tree))
    (setq tree (string-replace "│  " "│  " tree))))
  tree))
    
(define-minor-mode cleandesk-tree-buffer-mode
  "A minor mode for `tree' results buffers."
  :lighter " cleandesk-tree-results-buffer"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "q") (lambda () (interactive) (kill-buffer (current-buffer))))
	    (define-key map (kbd "n") 'org-next-link)
	    (define-key map (kbd "p") 'org-previous-link)
	    (define-key map (kbd "o") 'cleandesk-dired-open-at-point)
	    (define-key map (kbd "O") 'org-open-at-point)
            map))

(defun cleandesk-dired-open-at-point ()
  "Open the file or directory at point in Dired using the default handler 
or open subdirectory in Dired."
  (interactive)
  (cond
   ((derived-mode-p 'dired-mode)
    (let ((file (dired-get-file-for-visit)))
      (if (file-directory-p file)
          (dired file)  
        (find-file file))))  
   ((derived-mode-p 'org-mode)
    (let* ((link (org-element-context))
           (type (org-element-property :type link))
           (path (org-element-property :path link)))
      (if (and (string= type "file")
               (file-directory-p path))
          (dired path)  
        (org-open-at-point)))))) 

(provide 'cleandesk)

;;; cleandesk.el ends here
