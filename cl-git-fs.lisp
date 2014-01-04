;;;; cl-git-fs.lisp
(in-package #:cl-git-fs)

;; (defclass author ()
;;   ((name :reader name :initarg :name :initform (error "An author must have a name"))
;;    (email :reader email :initarg :email :initform nil)))

;; (defclass revision ()
;;   ((id :reader id :initarg :id)
;;    (timestamp :reader timestamp :initarg :timestamp)
;;    (description :reader description :initarg :description)
;;    (changes :reader changes :initarg :changes)))

(defun cat (&rest strings)
  (apply #'concatenate 'string strings))

(defmethod git ((repo pathname) (command symbol) &rest args)
  (with-output-to-string (s)
    (sb-ext:run-program 
     "git" `(,(string-downcase (symbol-name command)) ,@args)
     :directory repo :wait t :search t :output s)
    s))

(defmethod initialize! ((repo pathname))
  (ensure-directories-exist repo)
  (git repo :init)
  (git repo :config "receive.denyCurrentBranch" "ignore"))

(defmethod git-commit ((repo pathname) (files list) (log-message string) (author-name string) &key email)
  (apply #'git repo :commit 
       "--author" (format nil "~a~@[ <~a>~]" author-name email)
       "-m" log-message
       files))

(defmethod git-exists? ((repo pathname) (file-name pathname))
  (not (string= "" (latest repo file-name))))

(defmethod save-file! ((repo pathname) (file-name pathname) contents)
  (let ((full-name (merge-pathnames file-name repo)))
    (ensure-directories-exist full-name)
    (with-open-file (s full-name :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-sequence contents s)))
  (git repo :add file-name)
  (git-commit repo (list file-name) "Me" "A Message"))

(defmethod retrieve-file ((repo pathname) (file-name pathname) &optional (revision-id "HEAD"))
  (let ((obj-name (cat revision-id ":" (format nil "~a" file-name))))
    (when (string= "blob" (subseq (git repo :cat-file "-t" obj-name) 0 4))
      (git repo :cat-file "-p" obj-name))))

(defmethod delete-file! ((repo pathname) (file-name pathname))
  (git repo :rm file-name)
  (git-commit repo (list file-name) "Me" "A Message"))

(defmethod move-file! ((repo pathname) (file-name pathname) (new-name pathname))
  (when (git-exists? repo file-name)
    (git repo :mv file-name new-name)
    (git-commit repo (list file-name new-name) "Me" "A Message")))

;; TODO this should be parsed into a list of revisions
(defparameter *git-log-format* "%x01%H%x00%ct%x00%an%x00%ae%x00%B")

;; Log entries delimited by #\Soh, fields delimited by #\Nul

;; H  -> commit hash
;; ct -> committer date (UNIX timestamp)
;; an -> author name
;; ae -> author email
;; B  -> raw body (unwrapped subject and body)

(defmethod history ((repo pathname) (file-names list) &key since until limit)
  (let ((args `("-z" ,(cat "--pretty=format:" *git-log-format*) 
		,@(when since (list (cat "--since='" since "'")))
		,@(when until (list (cat "--until='" until "'")))
		,@(when limit (list "-n" limit))
		"--" ,@file-names)))
    (apply #'git repo :whatchanged args)))

(defmethod latest ((repo pathname) (file-name pathname))
  (git repo :rev-list "--max-count=1" "HEAD" "--" (format nil "~a" file-name)))

(defmethod revision ((repo pathname) (revision-id string))
  (git repo :whatchanged "-z" (cat "--pretty=format:" *git-log-format*) "--max-count=1" revision-id))

;; TODO both of these should return a list of file paths
(defmethod index ((repo pathname))
  (git repo :ls-tree "-r" "-t" "-z" "HEAD"))

(defmethod list-directory ((repo pathname) (directory pathname))
  (let ((full-dir (cl-fad:directory-exists-p (merge-pathnames directory repo))))
    (when full-dir
      (git repo :ls-tree "-r" "-t" "-z" (format nil "HEAD:~a" directory)))))

(defmethod backslash-escape ((characters string) (target string))
  (coerce 
   (loop with chars = (coerce characters 'list)
      for char in (coerce target 'list)
      when (member char chars) collect #\\
      collect char)
   'string))

(defmethod escape-regex ((regex string))
  (backslash-escape "?*+{}[]\\^$.()" regex))

(defmethod grep ((repo pathname) (search-terms list) &key ignore-case? match-all? whole-words?)
  (let ((args `("-I" "-n" "--null"
		     ,@(when ignore-case? (list "--ignore-case"))
		     ,@(when match-all? (list "--all-match"))
		     ,@(when whole-words? (list "-word-regexp"))
		     ,@(loop for term in search-terms
			  collect "-e" collect (escape-regex term)))))
    (apply #'git repo :grep args)))

(defmethod ids-match? (repo id-a id-b)
  "git lets you pass a unique prefix in place of the full SHA hash that identifies a commit. Therefore, two hashes match if one is a prefix of the other. Keep in mind that this means a prefix like `a` or `1` is probably going to give you useless results."
  (or (alexandria:starts-with-subseq id-a id-b)
      (alexandria:starts-with-subseq id-b id-a)) nil)
