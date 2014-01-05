;;;; cl-git-fs.lisp
(in-package #:cl-git-fs)

(defmethod git-commit! ((repo pathname) (files list) (author author) (log-message string))
  (apply #'git repo :commit 
       "--author" (->string author)
       "-m" log-message
       files))

(defmethod initialize! ((repo pathname))
  "Takes a pathname, and initializes + configures a git repository there.
Ensures that the directory exists first."
  (ensure-directories-exist repo)
  (git repo :init)
  (git repo :config "receive.denyCurrentBranch" "ignore")
  repo)

(defmethod save-file! ((repo pathname) (file-name pathname) contents (author author) (message string))
  (let ((full-name (merge-pathnames file-name repo)))
    (ensure-directories-exist full-name)
    (with-open-file (s full-name :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-sequence contents s)))
  (git repo :add file-name)
  (git-commit! repo (list file-name) author message))

(defmethod delete-file! ((repo pathname) (file-name pathname) (author author)  (message string))
  "Removes the specified filename from the specified repo."
  (git repo :rm file-name)
  (git-commit! repo (list file-name) author message))

(defmethod move-file! ((repo pathname) (file-name pathname) (new-name pathname) (author author)  (message string))
  "Moves the specified file-name to the specified new-name in the specified repo."
  (when (git-exists? repo file-name)
    (git repo :mv file-name new-name)
    (git-commit! repo (list file-name new-name) author message)))

(defmethod retrieve-file ((repo pathname) (file-name pathname) &optional (revision-id "HEAD"))
  "Retrieves the contents of the given file blob.
Checks HEAD by default, but accepts an optional revision-id."
  (let ((obj-name (cat revision-id ":" (format nil "~a" file-name))))
    (when (string= "blob" (subseq (git repo :cat-file "-t" obj-name) 0 4))
      (git repo :cat-file "-p" obj-name))))

(defmethod history ((repo pathname) (file-names list) &key since until limit)
  "Returns a list of (hash [timestamp in universal-time format] author raw-comment) in the given repo related to the given file-names.
Optionally takes since, until and limit parameters.
See the git-whatchanged man page for examples of since and until strings. 
The limit parameter is a number."
  (let ((args `("-z" ,+format+ 
		,@(when since (list (cat "--since='" since "'")))
		,@(when until (list (cat "--until='" until "'")))
		,@(when limit (list "-n" limit))
		"--" ,@file-names)))
    (git-output->revisions (apply #'git repo :whatchanged args))))

(defmethod latest ((repo pathname) (file-name pathname))
  "Returns the hash of the latest commit relevant to file-name in the specified repo."
  (let ((raw (git repo :rev-list "--max-count=1" "HEAD" "--" (format nil "~a" file-name))))
    (subseq raw 0 (- (length raw) 1))))

(defmethod revision ((repo pathname) (revision-id string))
  "Returns (hash [timestamp in universal-time format] author raw-comment) for the given commit in the given repo."
  (first
   (git-output->revisions
    (git repo :whatchanged "-z" +format+ "--max-count=1" revision-id))))

(defmethod index ((repo pathname))
  "Returns a list of all files in the repo."
  (git-output->pathnames (git repo :ls-tree "-r" "-t" "-z" "HEAD")))

(defmethod list-directory ((repo pathname) (directory pathname))
  "Returns a list of all files in the specified repo subdirectory."
  (let ((full-dir (cl-fad:directory-exists-p (merge-pathnames directory repo))))
    (when full-dir
      (git-output->pathnames (git repo :ls-tree "-r" "-t" "-z" (format nil "HEAD:~a" directory))))))

(defmethod grep ((repo pathname) (search-terms list) &key ignore-case? match-all? whole-words?)
  "Searches the specified repo for search-terms (a list of regexes).
Optionally ignores case, matches all (rather than first) and matches only whole words.
Returns a list of (pathname line-number snippet"
  (let ((args `("-I" "-n" "--null"
		     ,@(when ignore-case? (list "--ignore-case"))
		     ,@(when match-all? (list "--all-match"))
		     ,@(when whole-words? (list "-word-regexp"))
		     ,@(loop for term in search-terms
			  collect "-e" collect (escape-regex term)))))
    (loop for line in (split-sequence #\Newline (apply #'git repo :grep args) :remove-empty-subseqs t)
       for (path line-num snippet) = (split-sequence #\Nul line)
       collect (list (pathname path) (parse-integer line-num) snippet))))

(defmethod git-exists? ((repo pathname) (file-name pathname))
  "Checks whether the given file-name exists in the given repo. This is not the same as file-exists-p; a file might exist in history and not on the filesystem in the current commit."
  (not (string= "" (latest repo file-name))))

(defmethod ids-match? (repo id-a id-b)
  "git lets you pass a unique prefix in place of the full SHA hash that identifies a commit. Therefore, two hashes match if one is a prefix of the other. Keep in mind that this means a prefix like `a` or `1` is probably going to give you useless results."
  (or (alexandria:starts-with-subseq id-a id-b)
      (alexandria:starts-with-subseq id-b id-a)) nil)
