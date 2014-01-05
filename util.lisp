(in-package :cl-git-fs)

(defun cat (&rest strings)
  "Shorthand for (concatenate 'string [strings])"
  (apply #'concatenate 'string strings))

(defparameter +unix-epoch-difference+
  (encode-universal-time 0 0 0 1 1 1970 0))

(defparameter +git-log-format+ 
  "%x01%H%x00%ct%x00%an%x00%ae%x00%B"
  "A git format string for log output

%x01 -> #\Soh
%x00 -> #\Nul
%H  -> a commit hash
%ct -> committer date (UNIX timestamp format; use +unix-epoch-difference+ to compensate)
%an -> author name
%ae -> author email
%B  -> raw body (unwrapped subject and body)")

(defparameter +format+ (cat "--pretty=format:" +git-log-format+))

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

;; TODO this should define a different interface for SBCL, CCL, Lispworks and Clisp
(defmethod git ((repo string) (command symbol) &rest args)
  (with-output-to-string (s)
    (sb-ext:run-program 
     "git" `(,(string-downcase (symbol-name command)) ,@args)
     :directory (pathname repo) :wait t :search t :output s)
    s))
