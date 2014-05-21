(in-package :cl-git-fs)

(defun cat (&rest strings)
  "Shorthand for (concatenate 'string [strings])"
  (apply #'concatenate 'string strings))

(defmacro define-change (name args (msg-template &rest msg-args) &body body)
  `(defmethod ,name (,@args &key (author "Default Author") (email "default@email") (message (format nil ,msg-template ,@msg-args)))
     ,@body))

;;;;;;;;;; Thank you, krzysz00 and Clayton Stanley from [here](http://stackoverflow.com/questions/10049338/common-lisp-launch-subprocess-with-different-working-directory-than-lisp-proces)
(defmacro with-cwd (dir &body body)
  (with-gensyms (original-directory)
    `(let ((,original-directory (get-cwd)))
       (unwind-protect (progn
			 (cwd ,dir)
			 ,@body)
	 (cwd ,original-directory)))))

(defun get-cwd ()
  "Get the current directory pathname in an implementation-portable way"
  (let ((dir #+allegro (excl:current-directory)
	     #+clisp (#+lisp=cl ext:default-directory #-lisp=cl lisp:default-directory)
	     #+(or cmu scl) (ext:default-directory)
	     #+sbcl (sb-unix:posix-getcwd/)
	     #+CCL (ccl:current-directory)
	     #+cormanlisp (ccl:get-current-directory)
	     #+lispworks (hcl:get-working-directory)
	     #+mcl (ccl:mac-default-directory)
	     #-(or allegro clisp cmu scl cormanlisp mcl sbcl lispworks CCL) (truename ".")))
    (if (stringp dir)
	(parse-namestring dir)
	dir)))

(defun cwd (&optional dir)
  "Change directory and set default pathname in an implementation-portable way."
  (cond
    ((not (null dir))
     (when (and (typep dir 'logical-pathname)
		(translate-logical-pathname dir))
       (setq dir (translate-logical-pathname dir)))
     (when (stringp dir)
       (setq dir (parse-namestring dir)))
     #+allegro (excl:chdir dir)
     #+clisp (#+lisp=cl ext:cd #-lisp=cl lisp:cd dir)
     #+(or cmu scl) (setf (ext:default-directory) dir)
     #+cormanlisp (ccl:set-current-directory dir)
     #+(and mcl (not openmcl)) (ccl:set-mac-default-directory dir)
     #+openmcl (ccl:cwd dir)
     #+gcl (si:chdir dir)
     #+lispworks (hcl:change-directory dir)
     #+sbcl (sb-posix:chdir dir)
     (setq cl:*default-pathname-defaults* dir)
     (get-cwd))
    (t (get-cwd))))

;;;;;;;;;; Log format-related state
(defparameter +unix-epoch-difference+
  (encode-universal-time 0 0 0 1 1 1970 0))

(defparameter +git-log-format+ 
  "%x01%H%x00%ct%x00%an%x00%ae%x00%B"
  "A git format string for log output

%x01 -> #\Soh
%x00 -> #\Nul
%H  -> a commit hash
%ct -> committer date (in UNIX timestamp format; we use +unix-epoch-difference+ later to compensate)
%an -> author name
%ae -> author email
%B  -> raw body (unwrapped subject and body)")

(defparameter +format+ (cat "--pretty=format:" +git-log-format+))

;;;;;;;;;; Parsing/escaping functions
(defun git-output->revisions (raw-output)
  (loop for entry in (split-sequence #\Soh raw-output :remove-empty-subseqs t)
     for (hash date author-name email raw-body) = (split-sequence #\Nul entry)
     collect (list hash (+ (parse-integer date) +unix-epoch-difference+)
		   author-name email raw-body)))

(defun git-output->paths (raw-output)
  (let ((lines (split-sequence #\Nul raw-output :remove-empty-subseqs t)))
    (loop for ln in lines
       for (permissions type hash path) = (split-sequence-if (lambda (char) (member char (list #\Space #\Tab))) ln)
       collect path)))

(defmethod backslash-escape ((characters string) (target string))
  "Takes a string of characters, and a target string.
Escapes all characters in the target that appear in the bag of characters."
  (coerce 
   (loop with chars = (coerce characters 'list)
      for char in (coerce target 'list)
      when (member char chars) collect #\\
      collect char)
   'string))

(defmethod escape-regex ((regex string))
  "Escapes regex special characters for use in shell command arguments."
  (backslash-escape "?*+{}[]\\^$.()" regex))

;;;;;;;;;; Git command shorthands
(defmethod git ((repo string) (command symbol) &rest args)
  (with-output-to-string (s)
    (with-cwd repo
      (uiop:run-program `("git" ,(string-downcase (symbol-name command)) ,@args) :output s))
    s))

(defmethod git-commit! ((repo string) (files list) (author-name string) (author-email string) (log-message string))
  "Shortcut for calling (git :commit ...) with the appropriate --author and -m flag"
  (apply #'git repo :commit
	 "--author" (format nil "~a <~a>" author-name author-email)
	 "-m" log-message
	 files))

(defmethod needs-saving? ((repo string) (file-name string))
  "Returns t if the given file either
- changed according to `git-changed?`
- both exists on disk and is untracked"
  (or (git-changed? repo file-name)
      (and (cl-fad:file-exists-p (merge-pathnames file-name repo)) 
	   (not (latest repo file-name)))))

(defmethod git-changed? ((repo string) (file-name string))
  "Returns t if the given file is both tracked and currently modified. 
Basically, returns true if this file would show up as modified in a `git status` call."
  (and (latest repo file-name)
       (not (string= "" (git repo :diff "--name-only" file-name)))))
