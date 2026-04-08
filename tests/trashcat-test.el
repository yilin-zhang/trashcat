;;; trashcat-test.el --- ERT tests for trashcat.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Pin current behavior before refactor.  Run with:
;;   emacsclient --eval "(progn (add-to-list 'load-path \"<repo>\") \
;;     (add-to-list 'load-path \"<repo>/tests\") \
;;     (require 'trashcat) (require 'trashcat-test) \
;;     (ert-run-tests-batch \"^trashcat\"))"

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'trashcat)

;;; ─── Fixtures ────────────────────────────────────────────────────────────────

(defconst trashcat-test--plist-template
  (concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
          "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\""
          " \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n"
          "<plist version=\"1.0\">\n"
          "<dict>\n"
          "    <key>CFBundleIdentifier</key>\n"
          "    <string>%s</string>\n"
          "</dict>\n"
          "</plist>\n")
  "Minimal XML Info.plist with a single CFBundleIdentifier slot.")

(defun trashcat-test--make-app (apps-dir name bundle-id)
  "Create a fake .app bundle NAME under APPS-DIR with BUNDLE-ID.
Return the absolute bundle path."
  (let* ((app-path (expand-file-name (concat name ".app") apps-dir))
         (contents-dir (expand-file-name "Contents" app-path))
         (plist-path (expand-file-name "Info.plist" contents-dir)))
    (make-directory contents-dir t)
    (with-temp-file plist-path
      (insert (format trashcat-test--plist-template bundle-id)))
    app-path))

(defun trashcat-test--touch (path &optional contents)
  "Create file PATH (including parent dirs) with optional CONTENTS."
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (when contents (insert contents))))

(defmacro trashcat-test--with-fs (var &rest body)
  "Run BODY inside a fresh temp directory bound to VAR, cleaning up after."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,var (file-name-as-directory
                (make-temp-file "trashcat-test-" t))))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,var t))))

;;; ─── trashcat--format-size ───────────────────────────────────────────────────

(ert-deftest trashcat/format-size-zero ()
  (should (equal (trashcat--format-size 0) "0B")))

(ert-deftest trashcat/format-size-bytes ()
  (should (equal (trashcat--format-size 512) "512.00 B")))

(ert-deftest trashcat/format-size-kib ()
  (should (equal (trashcat--format-size 2048) "2.00 KB")))

(ert-deftest trashcat/format-size-mib ()
  (should (equal (trashcat--format-size (* 5 1024 1024)) "5.00 MB")))

(ert-deftest trashcat/format-size-gib ()
  (should (equal (trashcat--format-size (* 3 1024 1024 1024)) "3.00 GB")))

;;; ─── trashcat--get-file-size ─────────────────────────────────────────────────

(ert-deftest trashcat/get-file-size-regular-file ()
  (trashcat-test--with-fs root
    (let ((f (expand-file-name "small.txt" root)))
      (with-temp-file f (insert "0123456789"))
      (should (= (trashcat--get-file-size f) 10)))))

(ert-deftest trashcat/get-file-size-directory-nonzero ()
  (trashcat-test--with-fs root
    (trashcat-test--touch (expand-file-name "nested/a.txt" root) "hello")
    (should (> (trashcat--get-file-size root) 0))))

;;; ─── trashcat--get-bundle-identifier ─────────────────────────────────────────

(ert-deftest trashcat/bundle-identifier-reads-plist ()
  (trashcat-test--with-fs root
    (let ((trashcat--bundle-id nil)
          (app (trashcat-test--make-app root "Foo" "com.example.Foo")))
      (trashcat--get-bundle-identifier app)
      (should (equal trashcat--bundle-id "com.example.Foo")))))

(ert-deftest trashcat/bundle-identifier-missing-plist-leaves-nil ()
  (trashcat-test--with-fs root
    (let ((trashcat--bundle-id nil)
          (app (expand-file-name "Empty.app" root)))
      (make-directory (expand-file-name "Contents" app) t)
      (trashcat--get-bundle-identifier app)
      (should (null trashcat--bundle-id)))))

;;; ─── trashcat-find-app-bundle ────────────────────────────────────────────────

(ert-deftest trashcat/find-app-bundle-exact-match ()
  (trashcat-test--with-fs root
    (trashcat-test--make-app root "Foo" "com.example.Foo")
    (let ((trashcat-app-locations (list root))
          (trashcat--bundle-id nil))
      (let ((path (trashcat-find-app-bundle "Foo")))
        (should path)
        (should (equal (file-name-nondirectory (directory-file-name path))
                       "Foo.app"))
        (should (equal trashcat--bundle-id "com.example.Foo"))))))

(ert-deftest trashcat/find-app-bundle-substring-fallback ()
  "Querying 'cool' finds 'MyCoolApp.app' via the case-insensitive fallback."
  (trashcat-test--with-fs root
    (trashcat-test--make-app root "MyCoolApp" "com.example.mycoolapp")
    (let ((trashcat-app-locations (list root))
          (trashcat--bundle-id nil))
      (let ((path (trashcat-find-app-bundle "cool")))
        (should path)
        (should (equal (file-name-nondirectory (directory-file-name path))
                       "MyCoolApp.app"))))))

(ert-deftest trashcat/find-app-bundle-missing-returns-nil ()
  (trashcat-test--with-fs root
    (let ((trashcat-app-locations (list root))
          (trashcat--bundle-id nil))
      (should (null (trashcat-find-app-bundle "Nope"))))))

;;; ─── trashcat-get-app-list ───────────────────────────────────────────────────

(ert-deftest trashcat/get-app-list-strips-extension-and-sorts ()
  (trashcat-test--with-fs root
    (trashcat-test--make-app root "Zebra" "com.z")
    (trashcat-test--make-app root "Alpha" "com.a")
    (trashcat-test--make-app root "Mango" "com.m")
    (make-directory (expand-file-name "NotAnApp" root))
    (let ((trashcat-app-locations (list root)))
      (should (equal (trashcat-get-app-list) '("Alpha" "Mango" "Zebra"))))))

;;; ─── trashcat-find-all-related-files ─────────────────────────────────────────

(ert-deftest trashcat/find-all-related-files-bundle-plus-residuals ()
  "Returns the .app bundle plus residuals matching app-name or bundle-id."
  (trashcat-test--with-fs root
    (let* ((apps-dir (expand-file-name "Applications" root))
           (lib-dir  (expand-file-name "Library/Application Support" root)))
      (make-directory apps-dir t)
      (make-directory lib-dir t)
      (trashcat-test--make-app apps-dir "CoolApp" "com.example.CoolApp")
      ;; Matches by app name (directory).
      (trashcat-test--touch (expand-file-name "CoolApp/data.bin" lib-dir) "x")
      ;; Matches by bundle id.
      (trashcat-test--touch (expand-file-name "com.example.CoolApp.plist" lib-dir))
      ;; Unrelated — must NOT be picked up.
      (trashcat-test--touch (expand-file-name "something-else.bin" lib-dir))
      (let* ((trashcat-app-locations (list apps-dir))
             (trashcat-residual-locations
              `(("Application Support" . ,lib-dir)))
             (trashcat--bundle-id nil)
             (files (trashcat-find-all-related-files "CoolApp"))
             (paths (mapcar #'trashcat-file-path files))
             (types (mapcar #'trashcat-file-type files)))
        (should (= (length files) 3))
        (should (member "App Bundle" types))
        (should (seq-some (lambda (p) (string-suffix-p "CoolApp.app" p)) paths))
        (should (seq-some (lambda (p) (string-match-p "/CoolApp\\'" p)) paths))
        (should (seq-some (lambda (p)
                            (string-match-p "com\\.example\\.CoolApp\\.plist\\'" p))
                          paths))
        (should-not
         (seq-some (lambda (p) (string-match-p "something-else" p)) paths))))))

(ert-deftest trashcat/find-all-related-files-name-without-spaces ()
  "App name with a space matches residuals stored without the space."
  (trashcat-test--with-fs root
    (let* ((apps-dir (expand-file-name "Applications" root))
           (lib-dir  (expand-file-name "Library/Caches" root)))
      (make-directory apps-dir t)
      (make-directory lib-dir t)
      (trashcat-test--make-app apps-dir "My App" "com.example.MyApp")
      (trashcat-test--touch (expand-file-name "MyApp/cache.db" lib-dir))
      (let* ((trashcat-app-locations (list apps-dir))
             (trashcat-residual-locations `(("Caches" . ,lib-dir)))
             (trashcat--bundle-id nil)
             (files (trashcat-find-all-related-files "My App"))
             (paths (mapcar #'trashcat-file-path files)))
        (should (seq-some (lambda (p) (string-match-p "/MyApp\\'" p)) paths))))))

(provide 'trashcat-test)
;;; trashcat-test.el ends here
