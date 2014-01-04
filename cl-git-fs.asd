;;;; cl-git-fs.asd

(asdf:defsystem #:cl-git-fs
  :serial t
  :description "Interface for using git as a file storage back-end a-la gitit."
  :author "Inaimathi <leo.zovic@gmail.com>"
  :license "GPL"
  :depends-on (#:alexandria #:cl-fad)
  :components ((:file "package")
               (:file "cl-git-fs")))

