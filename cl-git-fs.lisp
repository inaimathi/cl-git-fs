;;;; cl-git-fs.lisp
(in-package #:cl-git-fs)

;;;;;;;;;; Repo modifying procedures
(defmethod initialize! ((repo string))
  "Takes a pathname, and initializes + configures a git repository there.
Ensures that the directory exists first."
  (ensure-directories-exist repo)
  (git repo :init)
  (git repo :config "receive.denyCurrentBranch" "ignore")
  repo)

(defmethod save-file! ((repo string) (file-name string) &key (author "Default Author") (email "default@email") (message "Minor change"))
  (let ((full-name (merge-pathnames file-name repo)))
    (when (needs-saving? repo file-name)
      (git repo :add file-name)
      (git-commit! repo (list file-name) author email message))))

(defmethod delete-file! ((repo string) (file-name string) &key (author "Default Author") (email "default@email") (message "Minor change"))
  "Removes the specified filename from the specified repo."
  (git repo :rm file-name)
  (git-commit! repo (list file-name) author email message))

(defmethod move-file! ((repo string) (file-name string) (new-name string) &key (author "Default Author") (email "default@email") (message "Minor change"))
  "Moves the specified file-name to the specified new-name in the specified repo."
  (when (latest repo file-name)
    (git repo :mv file-name new-name)
    (git-commit! repo (list file-name new-name) author email message)))

;;;;;;;;;; Repo querying functions
(defmethod retrieve-file ((repo string) (file-name string) &optional (revision-id "HEAD"))
  "Retrieves the contents of the given file blob at the given revision. Checks HEAD by default.
Will not show any uncommitted changes (just read the file for those)."
  (let ((obj-name (cat revision-id ":" file-name)))
    (when (string= "blob" (subseq (git repo :cat-file "-t" obj-name) 0 4))
      (git repo :cat-file "-p" obj-name))))

(defmethod history ((repo string) (file-name string) &key since until limit)
  (history repo (list file-name) :since since :until until :limit limit))

(defmethod history ((repo string) (file-names list) &key since until limit)
  "Returns a list of (hash [timestamp in universal-time format] author-name email raw-comment) in the given repo related to the given file-names.
Optionally takes since, until and limit parameters.
See the git-whatchanged man page for examples of since and until strings. 
The limit parameter is a number."
  (let ((args `("-z" ,+format+ 
		,@(when since (list (cat "--since='" since "'")))
		,@(when until (list (cat "--until='" until "'")))
		,@(when limit (list "-n" limit))
		"--" ,@file-names)))
    (git-output->revisions (apply #'git repo :whatchanged args))))

(defmethod latest ((repo string) (file-name string))
  "Returns the hash of the latest commit relevant to file-name in the specified repo.
Returns NIL if the file is not tracked by the specified repo."
  (let ((raw (git repo :rev-list "--max-count=1" "HEAD" "--" file-name)))
    (when (and (> (length raw) 0) (not (alexandria:starts-with-subseq "fatal:" raw)))
      (subseq raw 0 (- (length raw) 1)))))

(defmethod revision ((repo string) (revision-id string))
  "Returns (hash [timestamp in universal-time format] author-name email raw-comment) for the given commit in the given repo."
  (first
   (git-output->revisions
    (git repo :whatchanged "-z" +format+ "--max-count=1" revision-id))))

(defmethod index ((repo string))
  "Returns a list of all files in the repo."
  (git-output->paths (git repo :ls-tree "-r" "-t" "-z" "HEAD")))

(defmethod list-directory ((repo string) (directory string))
  "Returns a list of all files in the specified repo subdirectory."
  (let ((full-dir (cl-fad:directory-exists-p (merge-pathnames directory repo))))
    (when full-dir
      (git-output->paths 
       (git repo :ls-tree "-r" "-t" "-z" (cat "HEAD:" directory))))))

(defmethod grep ((repo string) (search-term string) &key ignore-case? match-all? whole-words?)
  (grep repo (list search-term) :ignore-case? ignore-case? :match-all? match-all? :whole-words? whole-words?))

(defmethod grep ((repo string) (search-terms list) &key ignore-case? match-all? whole-words?)
  "Searches the specified repo for search-terms (a list of regexes).
Optionally ignores case, matches all (rather than first) and matches only whole words.
Returns a list of (pathname line-number snippet) of matching pieces of files."
  (let ((args `("-I" "-n" "--null"
		     ,@(when ignore-case? (list "--ignore-case"))
		     ,@(when match-all? (list "--all-match"))
		     ,@(when whole-words? (list "-word-regexp"))
		     ,@(loop for term in search-terms
			  collect "-e" collect (escape-regex term)))))
    (loop for line in (split-sequence #\Newline (apply #'git repo :grep args) :remove-empty-subseqs t)
       for (path line-num snippet) = (split-sequence #\Nul line)
       collect (list (pathname path) (parse-integer line-num) snippet))))

(defmethod ids-match? (repo id-a id-b)
  "git lets you pass a unique prefix in place of the full SHA hash that identifies a commit. Therefore, two hashes match if one is a prefix of the other. Keep in mind that this means a prefix like `a` or `1` is probably going to give you useless results."
  (or (alexandria:starts-with-subseq id-a id-b)
      (alexandria:starts-with-subseq id-b id-a)) nil)
