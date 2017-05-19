#|
 This file is a part of flow
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:flow-visualizer
  (:nicknames #:org.shirakumo.flow.visualizer)
  (:use #:cl+qt #:flow)
  (:shadowing-import-from #:flow #:connect #:disconnect #:slot)
  (:export
   #:start))
