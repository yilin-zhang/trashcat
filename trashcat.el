;;; trashcat.el --- Clean up macOS apps and their residual files -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Yilin Zhang
;; Maintainer: Yilin Zhang
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, tools
;; URL: https://github.com/yilin-zhang/trashcat

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Trashcat finds macOS application bundles and their residual files, then
;; moves them to the Trash after interactive review.
;;
;; Usage: M-x trashcat, pick an application.  The resulting buffer lists
;; every item found (the .app plus residual files in ~/Library).  Entries
;; start marked `D' (delete).  Review, unmark any false positives, and
;; press `x' to move them to the Trash.
;;
;; Discovery is performed by functions on `trashcat-source-functions'.
;; Each function receives a plist `(:app-names :app-path :bundle-id)' and
;; returns a list of `trashcat-entry' structs.  Add your own source for
;; extra locations.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)


;;; Customization ----------------------------------------------------------

(defgroup trashcat nil
  "Clean up macOS applications and their residual files."
  :group 'tools
  :prefix "trashcat-")

(defcustom trashcat-app-locations
  '("/Applications" "~/Applications")
  "Directories scanned for `.app' bundles."
  :type '(repeat directory)
  :group 'trashcat)

(defcustom trashcat-residual-locations
  '((app-support      . "Library/Application Support")
    (caches           . "Library/Caches")
    (preferences      . "Library/Preferences")
    (logs             . "Library/Logs")
    (saved-state      . "Library/Saved Application State")
    (containers       . "Library/Containers")
    (group-containers . "Library/Group Containers")
    (launch-agents    . "Library/LaunchAgents")
    (http-storages    . "Library/HTTPStorages")
    (webkit           . "Library/WebKit")
    (cookies          . "Library/Cookies"))
  "Alist of (TYPE . REL-PATH) for residual-file locations.
REL-PATH is resolved relative to `$HOME'.  TYPE is a symbol used as
the entry's type tag in the list buffer."
  :type '(alist :key-type symbol :value-type string)
  :group 'trashcat)

(defcustom trashcat-source-functions
  '(trashcat-source-app-bundle
    trashcat-source-library-residuals
    trashcat-source-preferences-byhost)
  "Functions that discover entries for an application.
Each function receives a plist with keys `:app-names' (list of string),
`:app-path' (string), and `:bundle-id' (string or nil), and returns a
list of `trashcat-entry' structs."
  :type '(repeat function)
  :group 'trashcat)

(defcustom trashcat-check-running t
  "If non-nil, warn when trashing an app bundle whose process is running."
  :type 'boolean
  :group 'trashcat)


;;; Faces ------------------------------------------------------------------

(defface trashcat-type-face
  '((t :inherit font-lock-type-face))
  "Face for the Type column."
  :group 'trashcat)

(defface trashcat-size-face
  '((t :inherit font-lock-constant-face))
  "Face for the Size column."
  :group 'trashcat)

(defface trashcat-path-face
  '((t :inherit font-lock-string-face))
  "Face for the Path column."
  :group 'trashcat)

(defface trashcat-confidence-exact-face
  '((t :inherit success))
  "Face for `exact' and `bundle-id' confidence entries."
  :group 'trashcat)

(defface trashcat-confidence-name-face
  '((t :inherit warning))
  "Face for `name-match' confidence entries (review recommended)."
  :group 'trashcat)


;;; Data model -------------------------------------------------------------

(cl-defstruct (trashcat-entry
               (:constructor trashcat-entry-create)
               (:copier nil))
  type          ; Symbol: `bundle' or a key from `trashcat-residual-locations'
  path          ; Absolute path string
  size          ; Size in bytes, or nil if not yet computed
  source        ; Symbol: which source function produced this entry
  confidence)   ; Symbol: `exact', `bundle-id', or `name-match'


;;; Buffer-local state -----------------------------------------------------

(defvar-local trashcat--app-name nil
  "Display name of the app for this buffer (user's search term).")

(defvar-local trashcat--app-info nil
  "Discovery info plist: (:app-names :app-path :bundle-id).")

(defvar-local trashcat--entries nil
  "List of `trashcat-entry' structs currently shown in the buffer.")


;;; Utilities --------------------------------------------------------------

(defun trashcat--format-size (bytes)
  "Format BYTES as a human-readable size string."
  (if (zerop bytes)
      "0B"
    (let ((units '("B" "KB" "MB" "GB" "TB"))
          (size (float bytes))
          (i 0))
      (while (and (>= size 1024) (< i (1- (length units))))
        (setq size (/ size 1024.0)
              i (1+ i)))
      (format "%.2f %s" size (nth i units)))))

(defun trashcat--normalize-name (s)
  "Return downcased S with spaces removed."
  (downcase (replace-regexp-in-string " " "" s)))


;;; Info.plist extraction --------------------------------------------------

(defun trashcat--plist-string (plist-file key)
  "Read the string value at KEY from PLIST-FILE, or nil if absent."
  (with-temp-buffer
    (when (zerop (call-process "plutil" nil '(t nil) nil
                               "-extract" key "raw" "-o" "-" plist-file))
      (let ((s (string-trim (buffer-string))))
        (and (not (string-empty-p s)) s)))))

(defun trashcat--app-info (app-path app-name)
  "Return discovery info plist for APP-PATH.
Combines the user-supplied APP-NAME with `CFBundleName',
`CFBundleDisplayName', and the .app filename into a deduplicated
`:app-names' list.  `:bundle-id' is `CFBundleIdentifier' or nil."
  (let* ((plist (expand-file-name "Contents/Info.plist" app-path))
         (have-plist (file-exists-p plist))
         (bid  (and have-plist
                    (trashcat--plist-string plist "CFBundleIdentifier")))
         (name (and have-plist
                    (trashcat--plist-string plist "CFBundleName")))
         (disp (and have-plist
                    (trashcat--plist-string plist "CFBundleDisplayName")))
         (file-name (file-name-sans-extension
                     (file-name-nondirectory
                      (directory-file-name app-path))))
         (candidates (delete-dups
                      (delq nil (list app-name file-name name disp)))))
    (list :app-names candidates
          :app-path  app-path
          :bundle-id bid)))


;;; Matching ---------------------------------------------------------------

(defun trashcat--word-in-filename-p (word filename)
  "Return non-nil if WORD appears as a word in FILENAME.
Word boundaries are start/end of string or any non-alphanumeric char."
  (and (stringp word)
       (not (string-empty-p word))
       (let ((case-fold-search t)
             (pattern (concat "\\(?:\\`\\|[^a-z0-9]\\)"
                              (regexp-quote word)
                              "\\(?:\\'\\|[^a-z0-9]\\)")))
         (string-match-p pattern filename))))

(defun trashcat--name-match-confidence (filename name)
  "Return match confidence of FILENAME against a single app NAME.
Returns `exact', `name-match', or nil."
  (and (stringp name)
       (not (string-empty-p name))
       (let* ((f (downcase filename))
              (stem (downcase (file-name-sans-extension filename)))
              (lc (downcase name))
              (nospaces (trashcat--normalize-name name)))
         (cond
          ((or (equal stem lc)
               (equal stem nospaces))
           'exact)
          ((or (trashcat--word-in-filename-p lc f)
               (trashcat--word-in-filename-p nospaces f))
           'name-match)
          (t nil)))))

(defun trashcat--bundle-id-match-confidence (filename bundle-id)
  "Return `bundle-id' if FILENAME matches BUNDLE-ID, else nil.
Matches exact, `bid.*', `bid-*', and `*.bid' (Group Container suffix)."
  (and (stringp bundle-id)
       (not (string-empty-p bundle-id))
       (let ((f (downcase filename))
             (bid (downcase bundle-id)))
         (and (or (equal f bid)
                  (string-prefix-p (concat bid ".") f)
                  (string-prefix-p (concat bid "-") f)
                  (string-suffix-p (concat "." bid) f))
              'bundle-id))))

(defun trashcat--match-confidence (filename app-names bundle-id)
  "Return confidence for FILENAME against APP-NAMES and BUNDLE-ID.
APP-NAMES is a list of candidate name strings.  Returns the strongest
match found among the candidates: `exact' > `bundle-id' > `name-match',
or nil if none match."
  (let ((name-results
         (delq nil
               (mapcar (lambda (n) (trashcat--name-match-confidence filename n))
                       app-names))))
    (cond
     ((memq 'exact name-results) 'exact)
     ((trashcat--bundle-id-match-confidence filename bundle-id))
     ((memq 'name-match name-results) 'name-match)
     (t nil))))


;;; App bundle discovery ---------------------------------------------------

(defun trashcat--find-app-bundle (app-name)
  "Return absolute path to the .app bundle for APP-NAME, or nil."
  (let ((want (concat app-name ".app"))
        (want-lc (downcase app-name)))
    (or
     ;; 1. Exact filename match.
     (cl-some (lambda (loc)
                (let* ((d (expand-file-name loc))
                       (p (expand-file-name want d)))
                  (and (file-directory-p p) p)))
              trashcat-app-locations)
     ;; 2. Case-insensitive substring fallback.
     (cl-some
      (lambda (loc)
        (let ((d (expand-file-name loc)))
          (and (file-directory-p d)
               (cl-some
                (lambda (name)
                  (let ((p (expand-file-name name d)))
                    (and (string-match-p (regexp-quote want-lc)
                                         (downcase name))
                         (file-directory-p p)
                         p)))
                (directory-files d nil "\\.app\\'")))))
      trashcat-app-locations))))

(defun trashcat--list-installed-apps ()
  "Return a sorted, deduplicated list of installed app names (without .app)."
  (let (names)
    (dolist (loc trashcat-app-locations)
      (let ((d (expand-file-name loc)))
        (when (file-directory-p d)
          (dolist (f (directory-files d nil "\\.app\\'"))
            (when (file-directory-p (expand-file-name f d))
              (push (substring f 0 -4) names))))))
    (sort (delete-dups names) #'string<)))


;;; Size computation -------------------------------------------------------

(defun trashcat--du-sizes (paths)
  "Return a hash table mapping each path in PATHS to its size in bytes.
Uses a single `du -sk' invocation over all PATHS."
  (let ((sizes (make-hash-table :test 'equal)))
    (when paths
      (with-temp-buffer
        (apply #'call-process "du" nil '(t nil) nil "-sk" paths)
        (goto-char (point-min))
        (while (re-search-forward "^\\([0-9]+\\)\t\\(.+\\)$" nil t)
          (puthash (match-string 2)
                   (* 1024 (string-to-number (match-string 1)))
                   sizes))))
    sizes))

(defun trashcat--fill-sizes (entries)
  "Fill the :size slot of every entry in ENTRIES.  Return ENTRIES."
  (let* ((existing (seq-filter (lambda (e)
                                 (file-exists-p (trashcat-entry-path e)))
                               entries))
         (sizes (trashcat--du-sizes
                 (mapcar #'trashcat-entry-path existing))))
    (dolist (e entries)
      (setf (trashcat-entry-size e)
            (or (gethash (trashcat-entry-path e) sizes) 0)))
    entries))


;;; Source functions -------------------------------------------------------

(defun trashcat-source-app-bundle (props)
  "Return the .app bundle as a single entry, given discovery PROPS."
  (when-let* ((path (plist-get props :app-path)))
    (list (trashcat-entry-create
           :type 'bundle
           :path path
           :source 'app-bundle
           :confidence 'exact))))

(defun trashcat--scan-directory (dir type source app-names bundle-id)
  "Return entries for items in DIR that match APP-NAMES / BUNDLE-ID.
Each entry is tagged with TYPE and SOURCE.  Silently skips DIR when it
cannot be listed — e.g. macOS TCC blocks `~/Library/Cookies',
`~/Library/Safari', `~/Library/Mail' without Full Disk Access."
  (let (entries)
    (condition-case _err
        (when (file-directory-p dir)
          (dolist (item (directory-files dir nil "\\`[^.]"))
            (when-let* ((conf (trashcat--match-confidence
                              item app-names bundle-id)))
              (push (trashcat-entry-create
                     :type type
                     :path (expand-file-name item dir)
                     :source source
                     :confidence conf)
                    entries))))
      (file-error nil))
    (nreverse entries)))

(defun trashcat-source-library-residuals (props)
  "Scan `trashcat-residual-locations' for matches against PROPS."
  (let ((app-names (plist-get props :app-names))
        (bundle-id (plist-get props :bundle-id))
        entries)
    (dolist (loc trashcat-residual-locations)
      (setq entries
            (append entries
                    (trashcat--scan-directory
                     (expand-file-name (cdr loc) "~")
                     (car loc) 'library-residuals
                     app-names bundle-id))))
    entries))

(defun trashcat-source-preferences-byhost (props)
  "Scan ~/Library/Preferences/ByHost/ for matches against PROPS.
Covers per-host preference plists named `<bid>.<UUID>.plist'."
  (trashcat--scan-directory
   (expand-file-name "Library/Preferences/ByHost" "~")
   'byhost 'preferences-byhost
   (plist-get props :app-names)
   (plist-get props :bundle-id)))


;;; Discovery pipeline -----------------------------------------------------

(defun trashcat--discover (app-info)
  "Run every `trashcat-source-functions' against APP-INFO.
Return a deduplicated-by-path list of entries."
  (let (entries seen)
    (dolist (fn trashcat-source-functions)
      (dolist (e (funcall fn app-info))
        (let ((p (trashcat-entry-path e)))
          (unless (member p seen)
            (push p seen)
            (push e entries)))))
    (nreverse entries)))


;;; Running-app detection --------------------------------------------------

(defun trashcat--app-running-p (app-path)
  "Return non-nil if APP-PATH corresponds to a running process.
Uses `pgrep -f' to match the full command line against APP-PATH."
  (and (executable-find "pgrep")
       (zerop (call-process "pgrep" nil nil nil "-f"
                            (regexp-quote app-path)))))


;;; Trashing ---------------------------------------------------------------

(defun trashcat--trash (path)
  "Move PATH to the Trash.  Return non-nil on success."
  (condition-case _err
      (progn
        (if (file-directory-p path)
            (delete-directory path t t)
          (delete-file path t))
        t)
    (error nil)))

(defun trashcat--trash-entries (entries)
  "Move every entry in ENTRIES to the Trash.
Return a plist of the form (:succeeded N :failed M :failed-paths (...))."
  (let ((ok 0) (fail 0) failed)
    (dolist (e entries)
      (let ((p (trashcat-entry-path e)))
        (if (trashcat--trash p)
            (cl-incf ok)
          (cl-incf fail)
          (push p failed))))
    (list :succeeded ok :failed fail :failed-paths (nreverse failed))))


;;; UI: mode ---------------------------------------------------------------

(defvar trashcat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m")   #'trashcat-mark)
    (define-key map (kbd "u")   #'trashcat-unmark)
    (define-key map (kbd "M")   #'trashcat-mark-all)
    (define-key map (kbd "U")   #'trashcat-unmark-all)
    (define-key map (kbd "t")   #'trashcat-toggle-mark)
    (define-key map (kbd "x")   #'trashcat-execute)
    (define-key map (kbd "RET") #'trashcat-visit)
    (define-key map (kbd "d")   #'trashcat-remove-entry)
    (define-key map (kbd "g")   #'trashcat-refresh)
    (define-key map (kbd "?")   #'describe-mode)
    map)
  "Keymap for `trashcat-mode'.")

(define-derived-mode trashcat-mode tabulated-list-mode "Trashcat"
  "Major mode for reviewing and trashing macOS application leftovers.

\\{trashcat-mode-map}"
  (setq tabulated-list-format
        [("Type"       18 t)
         ("Size"       10 trashcat--sort-size :right-align t)
         ("Conf"       11 t)
         ("Path"        0 t)]
        tabulated-list-padding 2
        tabulated-list-sort-key '("Size" . t))
  (tabulated-list-init-header))


;;; UI: rendering ----------------------------------------------------------

(defun trashcat--confidence-face (conf)
  "Return the face for confidence level CONF."
  (pcase conf
    ((or 'exact 'bundle-id) 'trashcat-confidence-exact-face)
    ('name-match 'trashcat-confidence-name-face)
    (_ 'default)))

(defun trashcat--entry-to-row (entry)
  "Convert ENTRY to a tabulated-list row `(ID VECTOR)'."
  (let ((conf (trashcat-entry-confidence entry)))
    (list entry
          (vector
           (propertize (symbol-name (trashcat-entry-type entry))
                       'face 'trashcat-type-face)
           (propertize (trashcat--format-size
                        (or (trashcat-entry-size entry) 0))
                       'face 'trashcat-size-face)
           (propertize (symbol-name (or conf 'unknown))
                       'face (trashcat--confidence-face conf))
           (propertize (abbreviate-file-name (trashcat-entry-path entry))
                       'face (trashcat--confidence-face conf))))))

(defun trashcat--sort-size (a b)
  "Compare tabulated-list rows A and B by size."
  (< (or (trashcat-entry-size (car a)) 0)
     (or (trashcat-entry-size (car b)) 0)))

(defun trashcat--populate ()
  "Refresh `tabulated-list-entries' from `trashcat--entries'."
  (setq tabulated-list-entries
        (mapcar #'trashcat--entry-to-row trashcat--entries))
  (tabulated-list-print t))


;;; UI: mark commands ------------------------------------------------------

(defun trashcat--apply-tag (predicate tag)
  "For each row whose ID satisfies PREDICATE, set TAG."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((id (tabulated-list-get-id)))
        (when (and id (funcall predicate id))
          (tabulated-list-put-tag tag))
        (forward-line 1)))))

(defun trashcat--marked-entries ()
  "Return the list of entries currently marked for deletion."
  (let (result)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (eq (char-after) ?D)
          (let ((id (tabulated-list-get-id)))
            (when id (push id result))))
        (forward-line 1)))
    (nreverse result)))

(defun trashcat-mark ()
  "Mark the entry at point for deletion."
  (interactive)
  (tabulated-list-put-tag "D" t))

(defun trashcat-unmark ()
  "Unmark the entry at point."
  (interactive)
  (tabulated-list-put-tag " " t))

(defun trashcat-mark-all ()
  "Mark every entry for deletion."
  (interactive)
  (trashcat--apply-tag (lambda (_) t) "D"))

(defun trashcat-unmark-all ()
  "Unmark every entry."
  (interactive)
  (trashcat--apply-tag (lambda (_) t) " "))

(defun trashcat-toggle-mark ()
  "Toggle the mark on the entry at point."
  (interactive)
  (if (eq (char-after) ?D)
      (tabulated-list-put-tag " " t)
    (tabulated-list-put-tag "D" t)))


;;; UI: entry actions ------------------------------------------------------

(defun trashcat-visit ()
  "Open the entry at point in Dired."
  (interactive)
  (let ((entry (tabulated-list-get-id)))
    (unless entry (user-error "No entry at point"))
    (let ((path (trashcat-entry-path entry)))
      (if (file-directory-p path)
          (dired path)
        (dired (file-name-directory path))))))

(defun trashcat-remove-entry ()
  "Remove the entry at point from the list (without trashing)."
  (interactive)
  (let ((entry (tabulated-list-get-id)))
    (unless entry (user-error "No entry at point"))
    (setq trashcat--entries (delq entry trashcat--entries))
    (trashcat--populate)))

(defun trashcat-refresh ()
  "Re-scan for residuals.  Preserves marks by path."
  (interactive)
  (unless trashcat--app-info
    (user-error "Not a Trashcat buffer"))
  (let ((marked (mapcar #'trashcat-entry-path (trashcat--marked-entries))))
    (setq trashcat--entries
          (trashcat--fill-sizes (trashcat--discover trashcat--app-info)))
    (trashcat--populate)
    (trashcat--apply-tag
     (lambda (id) (member (trashcat-entry-path id) marked))
     "D")
    (message "Refreshed: %d entries" (length trashcat--entries))))

(defun trashcat--confirm-running (entries)
  "If ENTRIES contains the app bundle and it is running, confirm with the user.
Return non-nil to proceed, nil to abort."
  (or (not trashcat-check-running)
      (let ((bundle-entry
             (seq-find (lambda (e) (eq (trashcat-entry-type e) 'bundle))
                       entries)))
        (not bundle-entry))
      (let ((app-path (plist-get trashcat--app-info :app-path)))
        (not (trashcat--app-running-p app-path)))
      (yes-or-no-p
       (format "%s appears to be running.  Trash anyway? "
               trashcat--app-name))))

(defun trashcat-execute ()
  "Move all marked entries to the Trash."
  (interactive)
  (let* ((entries (trashcat--marked-entries))
         (n (length entries))
         (total (seq-reduce
                 (lambda (acc e) (+ acc (or (trashcat-entry-size e) 0)))
                 entries 0)))
    (cond
     ((zerop n)
      (user-error "No entries marked"))
     ((not (yes-or-no-p
            (format "Move %d item%s (%s) to Trash? "
                    n (if (= n 1) "" "s")
                    (trashcat--format-size total))))
      (message "Cancelled"))
     ((not (trashcat--confirm-running entries))
      (message "Cancelled"))
     (t
      (let* ((result (trashcat--trash-entries entries))
             (ok (plist-get result :succeeded))
             (failed (plist-get result :failed))
             (failed-paths (plist-get result :failed-paths))
             (succeeded (seq-remove
                         (lambda (e)
                           (member (trashcat-entry-path e) failed-paths))
                         entries)))
        (setq trashcat--entries
              (cl-set-difference trashcat--entries succeeded :test #'eq))
        (trashcat--populate)
        (when failed-paths
          (trashcat--apply-tag
           (lambda (id) (member (trashcat-entry-path id) failed-paths))
           "D")
          (dolist (p failed-paths)
            (message "Failed: %s" p)))
        (message "Trashed %d of %d item%s%s"
                 ok n (if (= n 1) "" "s")
                 (if (zerop failed) "" (format " (%d failed)" failed))))))))


;;; Entry point ------------------------------------------------------------

;;;###autoload
(defun trashcat (app-name)
  "Start Trashcat on APP-NAME.
With a prefix argument, prompt for the application name as free text
rather than selecting from a completion list."
  (interactive
   (list
    (if current-prefix-arg
        (read-string "Application name: ")
      (let ((apps (trashcat--list-installed-apps)))
        (if apps
            (completing-read "Application: " apps nil t)
          (read-string "Application name: "))))))
  (let ((app-path (trashcat--find-app-bundle app-name)))
    (unless app-path
      (user-error "Could not find application %s" app-name))
    (let* ((info (trashcat--app-info app-path app-name))
           (entries (trashcat--fill-sizes (trashcat--discover info))))
      (unless entries
        (user-error "No files found for %s" app-name))
      (let ((buf (get-buffer-create (format "*trashcat: %s*" app-name))))
        (with-current-buffer buf
          (trashcat-mode)
          (setq trashcat--app-name app-name
                trashcat--app-info info
                trashcat--entries entries)
          (trashcat--populate)
          (trashcat-mark-all))
        (pop-to-buffer buf)))))

(provide 'trashcat)

;;; trashcat.el ends here
