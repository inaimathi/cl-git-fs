;;;; package.lisp

(defpackage #:cl-git-fs
  (:use #:cl)
  (:export #:initialize #:save-file #:delete-file #:move-file
	   #:retrieve-file #:history #:latest #:revision #:index #:list-directory #:grep
	   #:ids-match?))

