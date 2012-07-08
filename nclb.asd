(asdf:defsystem #:nclb
  :pathname #P"src/"
  :components ((:file "package")
               (:file "nclb" :depends-on ("package"))))
