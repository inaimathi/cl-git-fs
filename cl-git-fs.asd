;;;; cl-git-fs.asd

(asdf:defsystem #:cl-git-fs
  :serial t
  :description "Interface for using git as a file storage back-end a-la gitit."
  :author "Inaimathi <leo.zovic@gmail.com>"
  :license "GPL"
  :depends-on (#:alexandria #:anaphora #:external-program #:cl-fad #:split-sequence)
  :components ((:file "package")
	       (:file "util")
               (:file "cl-git-fs")))

