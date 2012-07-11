(in-package #:nclb.application)

(defsynopsis (:postfix "INPUT-FILE [OUTPUT-FILE]")
  (text :contents "A simple language-agnostic literate program processor.")
  (flag :short-name "r" :long-name "reversible"
        :description "Generate a reversible output file that can be converted back to its original source.")
  (group (:header "Immediate exit options:")
     (flag :short-name "h" :long-name "help"
           :description "Print this help and exit.")
     (flag :short-name "v" :long-name "version"
           :description "Print version number and exit.")))

(defun main ()
  (make-context)
  (when (getopt :long-name "help")
    (help)
    (exit))
  (when (getopt :long-name "version")
    (format t "NCLB version ~A"
            (asdf:component-version (asdf:find-system :nclb)))
    (exit))
  (destructuring-bind (input-filename &optional output-filename) (remainder)
    (convert-file input-filename
                  :output-filename output-filename
                  :reversiblep (getopt :long-name "reversible"))))

(defun build (&optional (name "nclb"))
  (dump name main))
