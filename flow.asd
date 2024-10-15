(asdf:defsystem flow
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A flowchart and generalised graph library."
  :homepage "https://Shinmera.github.io/flow/"
  :bug-tracker "https://github.com/Shinmera/flow/issues"
  :source-control (:git "https://github.com/Shinmera/flow.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "conditions")
               (:file "nodes")
               (:file "static-node")
               (:file "graph")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :text-draw
               :closer-mop))
