#|
 This file is a part of flow
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem flow
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A flowchart and generalised graph library."
  :homepage "https://Shinmera.github.io/flow/"
  :bug-tracker "https://github.com/Shinmera/flow/issues"
  :source-control (:git "https://github.com/Shinmera/flow.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "nodes")
               (:file "static-node")
               (:file "graph")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :closer-mop))
