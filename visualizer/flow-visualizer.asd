(asdf:defsystem flow-visualizer
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A visualizer for the flow flowchart library."
  :homepage "https://shinmera.com/docs/flow/"
  :bug-tracker "https://shinmera.com/project/flow/issues"
  :source-control (:git "https://shinmera.com/project/flow.git")
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
