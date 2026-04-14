;;; trashcat-test.el --- ERT tests for trashcat.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with:
;;   emacsclient --eval "(progn \
;;     (add-to-list 'load-path \"<repo>\") \
;;     (add-to-list 'load-path \"<repo>/tests\") \
;;     (require 'trashcat) (require 'trashcat-test) \
;;     (ert-run-tests-batch \"^trashcat\"))"

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'trashcat)


;;; ─── Fixtures ────────────────────────────────────────────────────────────

(defconst trashcat-test--plist-template
  (concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
          "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\""
          " \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n"
          "<plist version=\"1.0\">\n"
          "<dict>\n"
          "%s"
          "</dict>\n"
          "</plist>\n")
  "Skeleton Info.plist with a %s slot for key/value pairs.")

(defun trashcat-test--plist-pair (key value)
  "Format one KEY/VALUE pair as XML."
  (format "    <key>%s</key>\n    <string>%s</string>\n" key value))

(defun trashcat-test--make-app (apps-dir name &rest keys)
  "Create a fake .app bundle NAME under APPS-DIR.
KEYS is a flat list of KEY VALUE pairs written into Info.plist.
Return the absolute .app path."
  (let* ((app (expand-file-name (concat name ".app") apps-dir))
         (contents (expand-file-name "Contents" app))
         (plist (expand-file-name "Info.plist" contents))
         (pairs ""))
    (make-directory contents t)
    (while keys
      (setq pairs (concat pairs
                          (trashcat-test--plist-pair (pop keys) (pop keys)))))
    (with-temp-file plist
      (insert (format trashcat-test--plist-template pairs)))
    app))

(defun trashcat-test--touch (path &optional contents)
  "Create file PATH (and parents) with optional CONTENTS."
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (when contents (insert contents))))

(defmacro trashcat-test--with-fs (var &rest body)
  "Evaluate BODY with VAR bound to a fresh temp directory (auto-cleaned)."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,var (file-name-as-directory
                (make-temp-file "trashcat-test-" t))))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,var t))))


;;; ─── trashcat--format-size ───────────────────────────────────────────────

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

(ert-deftest trashcat/format-size-tib ()
  (should (equal (trashcat--format-size (* 2 1024 1024 1024 1024)) "2.00 TB")))


;;; ─── trashcat--normalize-name ────────────────────────────────────────────

(ert-deftest trashcat/normalize-name-strips-spaces ()
  (should (equal (trashcat--normalize-name "My App") "myapp")))

(ert-deftest trashcat/normalize-name-downcases ()
  (should (equal (trashcat--normalize-name "CoolApp") "coolapp")))

(ert-deftest trashcat/normalize-name-idempotent ()
  (should (equal (trashcat--normalize-name
                  (trashcat--normalize-name "My App"))
                 "myapp")))


;;; ─── trashcat--plist-string / trashcat--app-info ─────────────────────────

(ert-deftest trashcat/plist-string-reads-key ()
  (trashcat-test--with-fs root
    (let ((app (trashcat-test--make-app
                root "Foo"
                "CFBundleIdentifier" "com.example.Foo"
                "CFBundleName" "Foo")))
      (should (equal
               (trashcat--plist-string
                (expand-file-name "Contents/Info.plist" app)
                "CFBundleIdentifier")
               "com.example.Foo")))))

(ert-deftest trashcat/plist-string-missing-key-returns-nil ()
  (trashcat-test--with-fs root
    (let ((app (trashcat-test--make-app
                root "Foo" "CFBundleIdentifier" "com.example.Foo")))
      (should (null
               (trashcat--plist-string
                (expand-file-name "Contents/Info.plist" app)
                "NonexistentKey"))))))

(ert-deftest trashcat/app-info-collects-all-names ()
  "app-info must gather user input, filename, bundle-name, display-name."
  (trashcat-test--with-fs root
    (let* ((app (trashcat-test--make-app
                 root "Code"
                 "CFBundleIdentifier" "com.microsoft.VSCode"
                 "CFBundleName" "Code"
                 "CFBundleDisplayName" "Visual Studio Code"))
           (info (trashcat--app-info app "Visual Studio Code")))
      (should (equal (plist-get info :bundle-id) "com.microsoft.VSCode"))
      (should (equal (plist-get info :app-path) app))
      ;; De-duplicated names in original preference order.
      (should (member "Visual Studio Code" (plist-get info :app-names)))
      (should (member "Code" (plist-get info :app-names)))
      ;; `Code' appears twice (filename + CFBundleName) but dedup removes it.
      (should (= (seq-count (lambda (x) (equal x "Code"))
                            (plist-get info :app-names))
                 1)))))

(ert-deftest trashcat/app-info-missing-plist ()
  "Missing Info.plist leaves bundle-id nil but still includes filename."
  (trashcat-test--with-fs root
    (let* ((app (expand-file-name "Orphan.app" root)))
      (make-directory app t)
      (let ((info (trashcat--app-info app "Orphan")))
        (should (null (plist-get info :bundle-id)))
        (should (member "Orphan" (plist-get info :app-names)))))))


;;; ─── trashcat--word-in-filename-p ────────────────────────────────────────

(ert-deftest trashcat/word-in-filename-matches-whole-name ()
  (should (trashcat--word-in-filename-p "mail" "mail.plist")))

(ert-deftest trashcat/word-in-filename-case-insensitive ()
  (should (trashcat--word-in-filename-p "mail" "Mail.plist"))
  (should (trashcat--word-in-filename-p "MAIL" "mail.plist")))

(ert-deftest trashcat/word-in-filename-rejects-larger-word ()
  "`mail' must not match `MailMate'."
  (should-not (trashcat--word-in-filename-p "mail" "MailMate.plist"))
  (should-not (trashcat--word-in-filename-p "go" "Google")))

(ert-deftest trashcat/word-in-filename-allows-boundary-chars ()
  (should (trashcat--word-in-filename-p "mail" "com.example.mail.plist"))
  (should (trashcat--word-in-filename-p "mail" "mail-cache"))
  (should (trashcat--word-in-filename-p "mail" "mail_cache"))
  (should (trashcat--word-in-filename-p "mail" "pre.mail")))

(ert-deftest trashcat/word-in-filename-rejects-empty-word ()
  (should-not (trashcat--word-in-filename-p "" "anything")))


;;; ─── trashcat--match-confidence ──────────────────────────────────────────

(ert-deftest trashcat/match-exact-equals-name ()
  (should (eq (trashcat--match-confidence "MyApp" '("MyApp") nil) 'exact)))

(ert-deftest trashcat/match-exact-equals-name-with-extension ()
  (should (eq (trashcat--match-confidence "MyApp.plist" '("MyApp") nil)
              'exact)))

(ert-deftest trashcat/match-exact-name-no-spaces ()
  "Filename `MyApp' must match app name `My App' as exact."
  (should (eq (trashcat--match-confidence "MyApp" '("My App") nil) 'exact)))

(ert-deftest trashcat/match-bundle-id-exact ()
  (should (eq (trashcat--match-confidence
               "com.example.Foo" '("Foo") "com.example.Foo")
              'bundle-id)))

(ert-deftest trashcat/match-bundle-id-prefix-dot ()
  (should (eq (trashcat--match-confidence
               "com.example.Foo.plist" '("Foo") "com.example.Foo")
              'bundle-id)))

(ert-deftest trashcat/match-bundle-id-prefix-dash ()
  (should (eq (trashcat--match-confidence
               "com.example.Foo-helper" '("Foo") "com.example.Foo")
              'bundle-id)))

(ert-deftest trashcat/match-bundle-id-group-container-suffix ()
  "Group Containers prefix folders with the team id."
  (should (eq (trashcat--match-confidence
               "TEAMID.com.example.Foo" '("Foo") "com.example.Foo")
              'bundle-id)))

(ert-deftest trashcat/match-name-loose ()
  "Loose word match falls back to name-match confidence."
  (should (eq (trashcat--match-confidence
               "com.other.Foo.plist" '("Foo") nil)
              'name-match)))

(ert-deftest trashcat/match-none ()
  (should (null (trashcat--match-confidence
                 "unrelated.plist" '("Foo") "com.example.Foo"))))

(ert-deftest trashcat/match-rejects-larger-word ()
  "`Mail' must not match `MailMate'."
  (should (null (trashcat--match-confidence "MailMate" '("Mail") nil))))

(ert-deftest trashcat/match-multi-name-any-hits ()
  "When multiple candidate names are given, any matching one counts."
  (should (eq (trashcat--match-confidence
               "Code" '("Visual Studio Code" "Code") nil)
              'exact))
  (should (eq (trashcat--match-confidence
               "VisualStudioCode" '("Visual Studio Code" "Code") nil)
              'exact)))

(ert-deftest trashcat/match-exact-beats-name-match ()
  "When one candidate is `exact' and another is `name-match', exact wins."
  (should (eq (trashcat--match-confidence
               "Code" '("Foo" "Code") nil)
              'exact)))

(ert-deftest trashcat/match-ignores-empty-strings ()
  (should (null (trashcat--match-confidence "Foo" '("" nil) nil))))


;;; ─── trashcat--find-app-bundle ───────────────────────────────────────────

(ert-deftest trashcat/find-app-bundle-exact ()
  (trashcat-test--with-fs root
    (trashcat-test--make-app root "Foo" "CFBundleIdentifier" "com.example.Foo")
    (let ((trashcat-app-locations (list root)))
      (let ((p (trashcat--find-app-bundle "Foo")))
        (should p)
        (should (equal (file-name-nondirectory (directory-file-name p))
                       "Foo.app"))))))

(ert-deftest trashcat/find-app-bundle-case-insensitive-substring ()
  (trashcat-test--with-fs root
    (trashcat-test--make-app root "MyCoolApp"
                             "CFBundleIdentifier" "com.example.mycoolapp")
    (let ((trashcat-app-locations (list root)))
      (let ((p (trashcat--find-app-bundle "cool")))
        (should p)
        (should (equal (file-name-nondirectory (directory-file-name p))
                       "MyCoolApp.app"))))))

(ert-deftest trashcat/find-app-bundle-missing ()
  (trashcat-test--with-fs root
    (let ((trashcat-app-locations (list root)))
      (should (null (trashcat--find-app-bundle "Nope"))))))


;;; ─── trashcat--list-installed-apps ───────────────────────────────────────

(ert-deftest trashcat/list-installed-apps-strips-extension-sorts ()
  (trashcat-test--with-fs root
    (trashcat-test--make-app root "Zebra" "CFBundleIdentifier" "com.z")
    (trashcat-test--make-app root "Alpha" "CFBundleIdentifier" "com.a")
    (trashcat-test--make-app root "Mango" "CFBundleIdentifier" "com.m")
    (make-directory (expand-file-name "NotAnApp" root))
    (let ((trashcat-app-locations (list root)))
      (should (equal (trashcat--list-installed-apps)
                     '("Alpha" "Mango" "Zebra"))))))

(ert-deftest trashcat/list-installed-apps-deduplicates ()
  "An app in two locations must appear only once."
  (trashcat-test--with-fs root
    (let ((d1 (expand-file-name "one" root))
          (d2 (expand-file-name "two" root)))
      (make-directory d1 t)
      (make-directory d2 t)
      (trashcat-test--make-app d1 "Foo" "CFBundleIdentifier" "com.example.Foo")
      (trashcat-test--make-app d2 "Foo" "CFBundleIdentifier" "com.example.Foo")
      (let ((trashcat-app-locations (list d1 d2)))
        (should (equal (trashcat--list-installed-apps) '("Foo")))))))


;;; ─── trashcat--du-sizes / trashcat--fill-sizes ───────────────────────────

(ert-deftest trashcat/du-sizes-single-file ()
  (trashcat-test--with-fs root
    (let ((f (expand-file-name "a.txt" root)))
      (with-temp-file f (insert (make-string 4096 ?x)))
      (let ((sizes (trashcat--du-sizes (list f))))
        (should (> (gethash f sizes) 0))))))

(ert-deftest trashcat/du-sizes-multiple-paths ()
  (trashcat-test--with-fs root
    (let ((a (expand-file-name "a.txt" root))
          (b (expand-file-name "b.txt" root)))
      (with-temp-file a (insert "A"))
      (with-temp-file b (insert "B"))
      (let ((sizes (trashcat--du-sizes (list a b))))
        (should (> (gethash a sizes) 0))
        (should (> (gethash b sizes) 0))))))

(ert-deftest trashcat/du-sizes-empty-input ()
  (let ((sizes (trashcat--du-sizes nil)))
    (should (hash-table-p sizes))
    (should (zerop (hash-table-count sizes)))))

(ert-deftest trashcat/fill-sizes-populates-size ()
  (trashcat-test--with-fs root
    (let ((f (expand-file-name "f.txt" root)))
      (with-temp-file f (insert (make-string 2048 ?x)))
      (let* ((e (trashcat-entry-create :path f :type 'foo))
             (result (trashcat--fill-sizes (list e))))
        (should (> (trashcat-entry-size (car result)) 0))))))

(ert-deftest trashcat/fill-sizes-missing-path-size-zero ()
  "Non-existent path gets size 0 instead of erroring."
  (trashcat-test--with-fs root
    (let* ((missing (expand-file-name "nope.txt" root))
           (e (trashcat-entry-create :path missing :type 'foo))
           (result (trashcat--fill-sizes (list e))))
      (should (= (trashcat-entry-size (car result)) 0)))))


;;; ─── Source: app bundle ──────────────────────────────────────────────────

(ert-deftest trashcat/source-app-bundle-returns-entry ()
  (trashcat-test--with-fs root
    (let* ((app (trashcat-test--make-app
                 root "Foo" "CFBundleIdentifier" "com.example.Foo"))
           (entries (trashcat-source-app-bundle
                     (list :app-names '("Foo")
                           :app-path app
                           :bundle-id "com.example.Foo"))))
      (should (= (length entries) 1))
      (let ((e (car entries)))
        (should (eq (trashcat-entry-type e) 'bundle))
        (should (equal (trashcat-entry-path e) app))
        (should (eq (trashcat-entry-confidence e) 'exact))))))

(ert-deftest trashcat/source-app-bundle-no-path-returns-empty ()
  (should (null (trashcat-source-app-bundle
                 (list :app-names '("Foo") :app-path nil :bundle-id nil)))))


;;; ─── Source: library residuals ───────────────────────────────────────────

(ert-deftest trashcat/source-library-residuals-matches-by-name ()
  (trashcat-test--with-fs root
    (let* ((lib (expand-file-name "Library/Application Support" root)))
      (make-directory lib t)
      (trashcat-test--touch (expand-file-name "CoolApp/data.bin" lib) "x")
      (let* ((trashcat-residual-locations `((app-support . ,lib)))
             (entries (trashcat-source-library-residuals
                       (list :app-names '("CoolApp") :app-path nil
                             :bundle-id nil))))
        (should (= (length entries) 1))
        (let ((e (car entries)))
          (should (eq (trashcat-entry-type e) 'app-support))
          (should (eq (trashcat-entry-confidence e) 'exact)))))))

(ert-deftest trashcat/source-library-residuals-matches-by-bundle-id ()
  (trashcat-test--with-fs root
    (let* ((lib (expand-file-name "Library/Preferences" root)))
      (make-directory lib t)
      (trashcat-test--touch (expand-file-name "com.example.Foo.plist" lib))
      (let* ((trashcat-residual-locations `((preferences . ,lib)))
             (entries (trashcat-source-library-residuals
                       (list :app-names '("Foo") :app-path nil
                             :bundle-id "com.example.Foo"))))
        (should (= (length entries) 1))
        (should (eq (trashcat-entry-confidence (car entries))
                    'bundle-id))))))

(ert-deftest trashcat/source-library-residuals-app-name-with-space ()
  "App with space matches folder stored without the space."
  (trashcat-test--with-fs root
    (let* ((lib (expand-file-name "Library/Caches" root)))
      (make-directory lib t)
      (trashcat-test--touch (expand-file-name "MyApp/cache.db" lib))
      (let* ((trashcat-residual-locations `((caches . ,lib)))
             (entries (trashcat-source-library-residuals
                       (list :app-names '("My App") :app-path nil
                             :bundle-id nil))))
        (should (= (length entries) 1))
        (should (string-match-p "/MyApp\\'"
                                (trashcat-entry-path (car entries))))))))

(ert-deftest trashcat/source-library-residuals-multi-name ()
  "Alternate names (e.g. CFBundleName `Code') must also match."
  (trashcat-test--with-fs root
    (let* ((lib (expand-file-name "Library/Application Support" root)))
      (make-directory lib t)
      (trashcat-test--touch (expand-file-name "Code/userdata" lib))
      (let* ((trashcat-residual-locations `((app-support . ,lib)))
             (entries (trashcat-source-library-residuals
                       (list :app-names '("Visual Studio Code" "Code")
                             :app-path nil :bundle-id nil))))
        (should (= (length entries) 1))
        (should (eq (trashcat-entry-confidence (car entries)) 'exact))))))

(ert-deftest trashcat/source-library-residuals-skips-unrelated ()
  (trashcat-test--with-fs root
    (let* ((lib (expand-file-name "Library/Application Support" root)))
      (make-directory lib t)
      (trashcat-test--touch (expand-file-name "SomethingElse/x.bin" lib))
      (let* ((trashcat-residual-locations `((app-support . ,lib)))
             (entries (trashcat-source-library-residuals
                       (list :app-names '("Foo") :app-path nil
                             :bundle-id nil))))
        (should (null entries))))))

(ert-deftest trashcat/source-library-residuals-skips-boundary-overreach ()
  "`Mail' must not drag `MailMate' into results."
  (trashcat-test--with-fs root
    (let* ((lib (expand-file-name "Library/Application Support" root)))
      (make-directory lib t)
      (trashcat-test--touch (expand-file-name "MailMate/data.bin" lib))
      (trashcat-test--touch (expand-file-name "Mail/data.bin" lib))
      (let* ((trashcat-residual-locations `((app-support . ,lib)))
             (entries (trashcat-source-library-residuals
                       (list :app-names '("Mail") :app-path nil
                             :bundle-id nil)))
             (paths (mapcar #'trashcat-entry-path entries)))
        (should (= (length paths) 1))
        (should (string-match-p "/Mail\\'" (car paths)))))))

(ert-deftest trashcat/source-library-residuals-group-container ()
  "Group Containers folders are prefixed with TEAMID."
  (trashcat-test--with-fs root
    (let* ((lib (expand-file-name "Library/Group Containers" root)))
      (make-directory lib t)
      (trashcat-test--touch
       (expand-file-name "TEAMID.com.example.Foo/data.bin" lib))
      (let* ((trashcat-residual-locations `((group-containers . ,lib)))
             (entries (trashcat-source-library-residuals
                       (list :app-names '("Foo") :app-path nil
                             :bundle-id "com.example.Foo"))))
        (should (= (length entries) 1))
        (should (eq (trashcat-entry-confidence (car entries))
                    'bundle-id))))))

(ert-deftest trashcat/source-library-residuals-saved-state-suffix ()
  "`com.example.Foo.savedState' should match via bid-prefix."
  (trashcat-test--with-fs root
    (let* ((lib (expand-file-name "Library/Saved Application State" root)))
      (make-directory lib t)
      (make-directory
       (expand-file-name "com.example.Foo.savedState" lib) t)
      (let* ((trashcat-residual-locations `((saved-state . ,lib)))
             (entries (trashcat-source-library-residuals
                       (list :app-names '("Foo") :app-path nil
                             :bundle-id "com.example.Foo"))))
        (should (= (length entries) 1))
        (should (eq (trashcat-entry-confidence (car entries))
                    'bundle-id))))))

(ert-deftest trashcat/scan-directory-swallows-file-error ()
  "A TCC-blocked directory must be skipped silently, not crash."
  (trashcat-test--with-fs root
    (cl-letf (((symbol-function 'directory-files)
               (lambda (&rest _)
                 (signal 'file-error
                         '("Opening directory" "Operation not permitted")))))
      (should (null (trashcat--scan-directory
                     root 'cookies 'test-source
                     '("Foo") "com.example.Foo"))))))


;;; ─── Source: preferences byhost ──────────────────────────────────────────

(ert-deftest trashcat/source-preferences-byhost-matches-plists ()
  (trashcat-test--with-fs root
    ;; Stand in for ~ via HOME override.
    (let ((process-environment
           (cons (format "HOME=%s" (directory-file-name root))
                 process-environment)))
      (let* ((byhost (expand-file-name "Library/Preferences/ByHost" root)))
        (make-directory byhost t)
        (trashcat-test--touch
         (expand-file-name "com.example.Foo.ABCD-1234.plist" byhost))
        (trashcat-test--touch
         (expand-file-name "com.unrelated.plist" byhost))
        (let* ((entries (trashcat-source-preferences-byhost
                         (list :app-names '("Foo") :app-path nil
                               :bundle-id "com.example.Foo")))
               (paths (mapcar #'trashcat-entry-path entries)))
          (should (= (length paths) 1))
          (should (string-match-p "com\\.example\\.Foo\\.ABCD-1234\\.plist\\'"
                                  (car paths)))
          (should (eq (trashcat-entry-type (car entries)) 'byhost)))))))


;;; ─── trashcat--discover ──────────────────────────────────────────────────

(ert-deftest trashcat/discover-runs-all-sources ()
  (trashcat-test--with-fs root
    (let* ((apps (expand-file-name "apps" root))
           (lib  (expand-file-name "lib" root)))
      (make-directory apps t)
      (make-directory lib t)
      (let ((app-path
             (trashcat-test--make-app apps "Foo"
                                      "CFBundleIdentifier" "com.example.Foo")))
        (trashcat-test--touch (expand-file-name "Foo/x.bin" lib) "x")
        (let* ((trashcat-source-functions
                '(trashcat-source-app-bundle
                  trashcat-source-library-residuals))
               (trashcat-residual-locations `((app-support . ,lib)))
               (info (list :app-names '("Foo")
                           :app-path app-path
                           :bundle-id "com.example.Foo"))
               (entries (trashcat--discover info))
               (types (mapcar #'trashcat-entry-type entries)))
          (should (memq 'bundle types))
          (should (memq 'app-support types)))))))

(ert-deftest trashcat/discover-dedupes-by-path ()
  (let* ((src1 (lambda (_)
                 (list (trashcat-entry-create
                        :type 'foo :path "/tmp/x"
                        :source 'src1 :confidence 'exact))))
         (src2 (lambda (_)
                 (list (trashcat-entry-create
                        :type 'foo :path "/tmp/x"
                        :source 'src2 :confidence 'exact))))
         (trashcat-source-functions (list src1 src2))
         (entries (trashcat--discover '(:app-names ("Foo")
                                        :app-path nil
                                        :bundle-id nil))))
    (should (= (length entries) 1))))


;;; ─── Trashing ────────────────────────────────────────────────────────────

(ert-deftest trashcat/trash-entries-counts-success-and-failure ()
  (cl-letf (((symbol-function 'trashcat--trash)
             (lambda (p) (not (string-match-p "bad" p)))))
    (let* ((entries
            (list (trashcat-entry-create :type 'foo :path "/tmp/ok1")
                  (trashcat-entry-create :type 'foo :path "/tmp/bad")
                  (trashcat-entry-create :type 'foo :path "/tmp/ok2")))
           (result (trashcat--trash-entries entries)))
      (should (= (plist-get result :succeeded) 2))
      (should (= (plist-get result :failed) 1))
      (should (equal (plist-get result :failed-paths) '("/tmp/bad"))))))

(ert-deftest trashcat/trash-entries-empty ()
  (let ((result (trashcat--trash-entries nil)))
    (should (zerop (plist-get result :succeeded)))
    (should (zerop (plist-get result :failed)))))


;;; ─── Running-app detection ───────────────────────────────────────────────

(ert-deftest trashcat/app-running-p-mocks-pgrep ()
  "pgrep exit 0 → running, non-zero → not running."
  (cl-letf (((symbol-function 'executable-find) (lambda (_) "/usr/bin/pgrep"))
            ((symbol-function 'call-process)
             (lambda (_cmd _in _out _disp &rest _args) 0)))
    (should (trashcat--app-running-p "/Applications/Foo.app")))
  (cl-letf (((symbol-function 'executable-find) (lambda (_) "/usr/bin/pgrep"))
            ((symbol-function 'call-process)
             (lambda (_cmd _in _out _disp &rest _args) 1)))
    (should-not (trashcat--app-running-p "/Applications/Foo.app"))))

(ert-deftest trashcat/app-running-p-no-pgrep-returns-nil ()
  (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
    (should-not (trashcat--app-running-p "/Applications/Foo.app"))))


;;; ─── trashcat--sort-size ─────────────────────────────────────────────────

(ert-deftest trashcat/sort-size-compares-entries ()
  (let ((a (list (trashcat-entry-create :size 100) nil))
        (b (list (trashcat-entry-create :size 200) nil)))
    (should (trashcat--sort-size a b))
    (should-not (trashcat--sort-size b a))))

(ert-deftest trashcat/sort-size-nil-treated-as-zero ()
  (let ((a (list (trashcat-entry-create :size nil) nil))
        (b (list (trashcat-entry-create :size 1) nil)))
    (should (trashcat--sort-size a b))))


;;; ─── trashcat--confidence-face ───────────────────────────────────────────

(ert-deftest trashcat/confidence-face-exact ()
  (should (eq (trashcat--confidence-face 'exact)
              'trashcat-confidence-exact-face)))

(ert-deftest trashcat/confidence-face-bundle-id ()
  (should (eq (trashcat--confidence-face 'bundle-id)
              'trashcat-confidence-exact-face)))

(ert-deftest trashcat/confidence-face-name-match ()
  (should (eq (trashcat--confidence-face 'name-match)
              'trashcat-confidence-name-face)))

(ert-deftest trashcat/confidence-face-unknown ()
  (should (eq (trashcat--confidence-face 'foo) 'default)))


;;; ─── trashcat--entry-to-row ──────────────────────────────────────────────

(ert-deftest trashcat/entry-to-row-shape ()
  (let* ((e (trashcat-entry-create
             :type 'caches :path "/tmp/x" :size 1024
             :source 'library-residuals :confidence 'exact))
         (row (trashcat--entry-to-row e)))
    (should (eq (car row) e))                            ; ID is the struct
    (should (= (length (cadr row)) 4))                   ; 4 columns
    (should (equal (aref (cadr row) 0) "caches"))
    (should (string-match-p "KB" (aref (cadr row) 1)))
    (should (equal (aref (cadr row) 2) "exact"))))


;;; ─── UI: marking in a real buffer ────────────────────────────────────────

(defmacro trashcat-test--in-buffer (entries &rest body)
  "Evaluate BODY in a fresh trashcat buffer containing ENTRIES."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (trashcat-mode)
     (setq trashcat--entries ,entries)
     (trashcat--populate)
     ,@body))

(ert-deftest trashcat/mark-all-marks-every-row ()
  (trashcat-test--in-buffer
      (list (trashcat-entry-create :type 'bundle :path "/tmp/a" :size 1)
            (trashcat-entry-create :type 'caches :path "/tmp/b" :size 2))
    (trashcat-mark-all)
    (should (= (length (trashcat--marked-entries)) 2))))

(ert-deftest trashcat/unmark-all-clears-marks ()
  (trashcat-test--in-buffer
      (list (trashcat-entry-create :type 'bundle :path "/tmp/a" :size 1))
    (trashcat-mark-all)
    (trashcat-unmark-all)
    (should (null (trashcat--marked-entries)))))

(ert-deftest trashcat/marked-entries-returns-nil-when-none ()
  (trashcat-test--in-buffer
      (list (trashcat-entry-create :type 'bundle :path "/tmp/a" :size 1))
    (should (null (trashcat--marked-entries)))))

(ert-deftest trashcat/apply-tag-obeys-predicate ()
  (let ((a (trashcat-entry-create :type 'bundle :path "/tmp/a" :size 1))
        (b (trashcat-entry-create :type 'caches :path "/tmp/b" :size 2)))
    (trashcat-test--in-buffer (list a b)
      (trashcat--apply-tag
       (lambda (e) (eq (trashcat-entry-type e) 'bundle))
       "D")
      (let ((marked (trashcat--marked-entries)))
        (should (= (length marked) 1))
        (should (eq (trashcat-entry-type (car marked)) 'bundle))))))


(provide 'trashcat-test)
;;; trashcat-test.el ends here
