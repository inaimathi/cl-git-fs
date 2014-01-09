;;;; package.lisp

(defpackage #:cl-git-fs
  (:nicknames :gitfs)
  (:use #:cl)
  (:import-from :anaphora #:aif #:awhen #:it)
  (:import-from :alexandria #:with-gensyms)
  (:import-from :split-sequence #:split-sequence #:split-sequence-if)
  (:export #:initialize! #:save-file! #:delete-file! #:move-file!
	   #:retrieve-file #:history #:latest #:revision #:index #:list-directory #:grep
	   #:ids-match?))

