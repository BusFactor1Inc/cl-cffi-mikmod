(defsystem :mikmod
    :description "A higher level interface to the mikmod library."
    :depends-on ("cl-cffi-mikmod")
    :components ((:file "mikmod")))
