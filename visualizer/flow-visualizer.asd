#|
 This file is a part of flow
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem flow-visualizer
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A visualizer for the flow flowchart library."
  :homepage "https://github.com/Shinmera/flow"
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
