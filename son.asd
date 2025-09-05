(asdf:defsystem "son"
  :depends-on ("alexa" "parse-float" "closer-mop")
  :components ((:file "src/son")
               (:file "src/jsonconv")))
