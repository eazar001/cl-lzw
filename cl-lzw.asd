(defsystem #:cl-lzw
  :description "Implementation of LZW compression algorithm."
  :author "Ebrahim Azarisooreh <ebrahim.azarisooreh@gmail.com>"
  :license "MIT"
  :pathname "src/"
  :components ((:file "lzw"))
  :in-order-to ((test-op (test-op "cl-lzw/tests"))))

(defsystem #:cl-lzw/tests
  :depends-on (#:cl-lzw :fiveam)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "package")
                                     (:file "tests"))))
  :perform (test-op (o s)
                    (symbol-call :fiveam :run! (find-symbol "ALL-TESTS" "CL-LZW-TESTS"))))
