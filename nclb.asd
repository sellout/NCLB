(asdf:defsystem #:nclb
  :pathname #P"src/"
  :depends-on (alexandria)
  :components ((:file "package")
               (:file "nclb" :depends-on ("package"))))
