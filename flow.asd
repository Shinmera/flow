#|
 This file is a part of flow
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem flow
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A sound system."
  :homepage "https://github.com/Shinmera/flow"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "nodes")
               (:file "graph")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :closer-mop))
