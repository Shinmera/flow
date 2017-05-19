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
                                        (q+:qsizepolicy.minimum)))
  (setf (q+:focus-policy canvas) (q+:qt.strong-focus)))

(define-override (canvas paint-event) (ev)
  (with-finalizing ((painter (q+:make-qpainter canvas)))
    (setf (q+:background painter) (q+:make-qbrush (q+:make-qcolor 150 150 150)
                                                  (q+:qt.dense7-pattern)))
    (q+:fill-rect painter (q+:rect canvas) (q+:make-qcolor 255 255 255))
    (q+:erase-rect painter (q+:rect canvas))
    (dolist (node nodes)
      (paint node painter))
    (when tool
      (paint tool painter))))

(define-override (canvas mouse-press-event) (ev)
  (when (tool canvas)
    (begin tool (q+:x (q+:pos-f ev)) (q+:y (q+:pos-f ev)))
    (q+:repaint canvas))
  (q+:ignore ev))

(define-override (canvas mouse-release-event) (ev)
  (when (tool canvas)
    (end tool (q+:x (q+:pos-f ev)) (q+:y (q+:pos-f ev)))
    (q+:repaint canvas))
  (q+:ignore ev))

(define-override (canvas mouse-move-event) (ev)
  (when (tool canvas)
    (update tool (q+:x (q+:pos-f ev)) (q+:y (q+:pos-f ev)))
    (q+:repaint canvas))
  (q+:ignore ev))

(define-override (canvas key-press-event) (ev)
  (when (tool canvas)
    (action tool ev)
    (q+:repaint canvas))
  (q+:ignore ev))


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
           (let ((button (make-instance class :canvas canvas)))
             (setf (q+:checkable button) T)
             (q+:add-button group button)
             (q+:add-widget layout button i j))))
    (loop for n from 0
          for class in '(select-tool
                         start-node-tool end-node-tool
                         process-node-tool conditional-node-tool)
          do (add-button class n 0)
          finally (let ((empty (q+:make-qwidget toolbar)))
                    (setf (q+:size-policy empty) (values (q+:qsizepolicy.expanding)
                                                         (q+:qsizepolicy.preferred)))
                    (q+:add-widget layout empty n 0)))))

(define-slot (toolbar clicked) ((id "int"))
  (declare (connected group (button-clicked int)))
  (let ((tool (q+:button group id)))
    (setf (tool canvas) tool)))


(define-widget tool (QPushButton)
  ((canvas :initarg :canvas :accessor canvas)
   (start-pos :initform (q+:make-qpointf 0 0) :accessor start-pos)
   (current-pos :initform (q+:make-qpointf 0 0) :accessor current-pos)
   (label :initform "?")))

(define-initializer (tool setup)
  (setf (q+:minimum-size tool) (values 36 36))
  (setf (q+:maximum-size tool) (values 36 36))
  (setf (q+:size-policy tool) (values (q+:qsizepolicy.maximum)
                                      (q+:qsizepolicy.maximum)))
  (setf (q+:tool-tip tool) (string (class-name (class-of tool))))
  (etypecase label
    (string (setf (q+:text tool) label))
    (qobject (setf (q+:icon tool) label))))

(defmethod begin :before ((tool tool) x y)
  (setf (q+:x (start-pos tool)) x)
  (setf (q+:y (start-pos tool)) y)
  (setf (q+:x (current-pos tool)) x)
  (setf (q+:y (current-pos tool)) y))

(defmethod update :after ((tool tool) x y)
  (setf (q+:x (current-pos tool)) x)
  (setf (q+:y (current-pos tool)) y))

(defmethod begin ((tool tool) x y))
(defmethod end ((tool tool) x y))
(defmethod update ((tool tool) x y))
(defmethod action ((tool tool) key))
(defmethod paint ((tool tool) painter))

(define-widget select-tool (QPushButton tool)
  ((selected :initform NIL :accessor selected)
   (editing :initform NIL :accessor editing)))

(defun unit-at-point (mx my nodes)
  (dolist (node nodes)
    (with-attributes (x y w h) node
      (when (and (<= x mx (+ x w))
                 (<= y my (+ y h)))
        (dolist (port (ports node))
          (let ((point (location port)))
            (when (<= (sqrt (+ (expt (- (q+:x point) mx) 2)
                               (expt (- (q+:y point) my) 2)))
                      5.0)
              (return-from unit-at-point port))))
        (return-from unit-at-point node)))))

(defmethod begin ((tool select-tool) mx my)
  (let ((new (unit-at-point mx my (nodes (canvas tool)))))
    (if (eql new (selected tool))
        (setf (editing tool) (not (editing tool)))
        (setf (selected tool) new))))

(defmethod update ((tool select-tool) mx my)
  (typecase (selected tool)
    (node
     (let ((dx (- mx (q+:x (current-pos tool))))
           (dy (- my (q+:y (current-pos tool)))))
       (incf (attribute (selected tool) 'x) dx)
       (incf (attribute (selected tool) 'y) dy)))))

(defmethod end ((tool select-tool) mx my)
  (let ((end (unit-at-point mx my (nodes (canvas tool)))))
    (typecase (selected tool)
      (port
       (when (and (typep end 'port) (not (eql end (selected tool))))
         (connect (selected tool) end))
       (setf (selected tool) NIL))
      (node
       ))))

(defmethod paint ((tool select-tool) painter)
  (typecase (selected tool)
    (port
     (q+:draw-line painter (location (selected tool)) (current-pos tool)))))

(defmethod action ((tool select-tool) ev)
  (let ((selected (selected tool)))
    (when selected
      (cond ((editing tool)
             (with-attributes (text) selected
               (unless text
                 (setf text ""))
               (qtenumcase (q+:key ev)
                 ((q+:qt.key_backspace)
                  (setf text (subseq text 0 (max 0 (1- (length text))))))
                 (T
                  (setf text (concatenate 'string text (q+:text ev)))))))
            (T
             (qtenumcase (q+:key ev)
               ((q+:qt.key_delete)
                (sever selected)
                (setf (nodes (canvas tool)) (remove selected (nodes (canvas tool)))))))))))

(define-widget node-tool (QPushButton tool)
  ((node-type :initform NIL :accessor node-type)
   (current :initform NIL :accessor current)))

(defmethod begin ((tool node-tool) x y)
  (let ((node (make-instance (node-type tool))))
    (setf (attribute node 'x) x)
    (setf (attribute node 'y) y)
    (setf (attribute node 'w) 10)
    (setf (attribute node 'h) 10)
    (setf (current tool) node)
    (push node (nodes (canvas tool)))))

(defmethod update ((tool node-tool) x y)
  (let ((node (current tool)))
    (when node
      (let ((w (- x (attribute node 'x)))
            (h (- y (attribute node 'y))))
        (setf (attribute node 'w) (max 10 w))
        (setf (attribute node 'h) (max 10 h))))))

(defmethod end ((tool node-tool) x y)
  (setf (current tool) NIL))

(defmacro define-node-tool (class)
  (let ((tool-name (intern (format NIL "~a-~a" class 'node-tool))))
    `(define-widget ,tool-name (QPushButton node-tool)
       ((node-type :initform ',class)))))

(define-node-tool start)
(define-node-tool end)
(define-node-tool process)
(define-node-tool conditional)

(defun start ()
  (with-main-window (w 'main)))
