(asdf:defsystem nclb.application
  :depends-on (nclb com.dvlsoft.clon)
  :pathname "application/"
  :components ((:file "package")
               (:file "build" :depends-on ("package"))))
