#|
 This file is a part of flow
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:flow-visualizer)
(in-readtable :qtools)

(define-widget main (QMainWindow)
  ())

(define-initializer (main setup)
  (setf (q+:window-title main) "Flow Visualizer"))

(define-subwidget (main canvas) (make-instance 'canvas)
  (setf (q+:central-widget main) canvas))

(define-subwidget (main toolbar) (make-instance 'toolbar :canvas canvas)
  (q+:add-dock-widget main (q+:qt.left-dock-widget-area) toolbar))


(define-widget canvas (QWidget)
  ((nodes :initform NIL :accessor nodes)
   (tool :initform NIL :accessor tool)))

(define-initializer (canvas setup)
  (setf (q+:size-policy canvas) (values (q+:qsizepolicy.minimum)
                                        (q+:qsizepolicy.minimum))))

(define-override (canvas paint-event) (ev)
  (with-finalizing ((painter (q+:make-qpainter canvas)))
    (setf (q+:background painter) (q+:make-qbrush (q+:make-qcolor 150 150 150)
                                                  (q+:qt.dense7-pattern)))
    (q+:fill-rect painter (q+:rect canvas) (q+:make-qcolor 255 255 255))
    (q+:erase-rect painter (q+:rect canvas))
    (dolist (node nodes)
      (paint node painter))))

(define-override (canvas mouse-press-event) (ev)
  (when (tool canvas)
    (begin tool (q+:x (q+:pos-f ev)) (q+:y (q+:pos-f ev)))))

(define-override (canvas mouse-release-event) (ev)
  (when (tool canvas)
    (end tool (q+:x (q+:pos-f ev)) (q+:y (q+:pos-f ev)))))

(define-override (canvas mouse-move-event) (ev)
  (when (tool canvas)
    (update tool (q+:x (q+:pos-f ev)) (q+:y (q+:pos-f ev)))))

(define-override (canvas key-release-event) (ev)
  (when (tool canvas)
    (action tool (q+:key ev))))


(define-widget toolbar (QDockWidget)
  ((canvas :initarg :canvas :reader canvas)))

(define-initializer (toolbar setup)
  (setf (q+:window-title toolbar) "Tools")
  (setf (q+:features toolbar) (q+:qdockwidget.dock-widget-movable)))

(define-subwidget (toolbar container) (q+:make-qwidget)
  (setf (q+:widget toolbar) container))

(define-subwidget (toolbar group) (q+:make-qbuttongroup))

(define-subwidget (toolbar layout) (q+:make-qgridlayout container)
  (setf (q+:margin layout) 0)
  (flet ((add-button (class i j)
           (let ((button (make-instance class)))
             (setf (q+:checkable button) T)
             (q+:add-button group button)
             (q+:add-widget layout button i j (q+:qt.align-top)))))
    (loop with cols = 2
          for n from 0
          for class in '(select-tool)
          do (add-button class (floor n cols) (mod n cols)))))

(define-slot (toolbar clicked) ((id "int"))
  (declare (connected group (button-clicked int)))
  (let ((tool (q+:button group id)))
    (setf (tool canvas) tool)))


(define-widget tool (QPushButton)
  ((label :initform "?")))

(define-initializer (tool setup)
  (setf (q+:minimum-size tool) (values 36 36))
  (setf (q+:maximum-size tool) (values 36 36))
  (setf (q+:size-policy tool) (values (q+:qsizepolicy.maximum)
                                      (q+:qsizepolicy.maximum)))
  (etypecase label
    (string (setf (q+:text tool) label))
    (qobject (setf (q+:icon tool) label))))

(defmethod begin ((tool tool) x y))
(defmethod end ((tool tool) x y))
(defmethod update ((tool tool) x y))
(defmethod action ((tool tool) key))

(define-widget select-tool (QPushButton tool)
  ((selected :initform NIL :accessor selected)))

(define-widget node-tool (QPushButton tool)
  ())


(defun start ()
  (with-main-window (w 'main)))
