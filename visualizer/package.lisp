(in-package #:cl-user)
(defpackage #:flow-visualizer
  (:nicknames #:org.shirakumo.flow.visualizer)
  (:use #:cl+qt #:flow)
  (:shadowing-import-from #:flow #:connect #:disconnect #:slot)
  (:export
   #:start))
