(require 'lisp-mode)
(require 'markdown-mode)

(require 'mmm-compat)
(require 'mmm-vars)
(require 'mmm-auto)

(defface mmm-nclb-code-submode-face '((t (:background "Wheat")))
  "Face used for submodes containing code."
  :group 'mmm-faces)

(mmm-add-group
 'nclb
 `((nclb-doc
    :submode markdown-mode
    :front "^#|@$"
    :back "^@|#$"
    :face mmm-default-submode-face)
   (nclb-code
    :submode lisp-mode
    :front "^@|#$"
    :back "^#|@$"
    :face mmm-nclb-code-submode-face)
   (nclb-comment
    :submode text-mode
    :front "^;@@"
    :include-front t
    :back "\n"
    :face mmm-comment-submode-face)
   (nclb-doc-one-line
    :submode markdown-mode
    :face mmm-default-submode-face
    :front "^;@"
    :include-front t
    :back "\n")))

(add-to-list 'mmm-mode-ext-classes-alist '(nil "lisp" nclb))
