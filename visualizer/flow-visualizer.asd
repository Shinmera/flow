#|
 This file is a part of flow
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem flow-visualizer
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A visualizer for the flow flowchart library."
  :homepage "https://Shinmera.github.io/flow/"
  :bug-tracker "https://github.com/Shinmera/flow/issues"
  :source-control (:git "https://github.com/Shinmera/flow.git")
  :serial T
  :components ((:file "package")
               (:file "nodes")
               (:file "visualizer"))
  :defsystem-depends-on (:qtools)
  :depends-on (:flow
               :qtcore
               :qtgui)
  :build-operation "qt-program-op"
  :build-pathname "flow-visualizer"
  :entry-point "flow-visualizer:start")
