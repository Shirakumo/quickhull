#|
 This file is a part of quickhull
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.quickhull)

(define-condition edge-solver-failed (warning)
  ()
  (:report (lambda (c s) (format s "Failed to construct edge loop. Convex hull may not be ideal."))))

(declaim (inline dbg))
#++
(defun dbg (format &rest args)
  (if (stringp format)
      (format *debug-io* "~&~?~%" format args)
      (apply #'dbg "~a" format args)))
#-+
(defun dbg (format &rest args)
  (declare (ignore format args)))

(declaim (inline v))
(defun v (vertices i)
  (let ((i (* 3 i)))
    (vec (aref vertices (+ i 0))
         (aref vertices (+ i 1))
         (aref vertices (+ i 2)))))

(defstruct (ray
            (:include vec3)
            (:constructor %ray (3d-vectors::%vx3 3d-vectors::%vy3 3d-vectors::%vz3
                                direction)))
  (direction (vec 0 0 0) :type vec3))

(defmethod print-object ((ray ray) stream)
  (print (list 'ray (vcopy ray) (ray-direction ray)) stream))

(defun ray (position direction)
  (%ray (vx position) (vy position) (vz position) direction))

(defstruct (plane
            (:include vec3)
            (:constructor %plane (3d-vectors::%vx3 3d-vectors::%vy3 3d-vectors::%vz3
                                  distance)))
  (distance 0.0 :type single-float))

(defmethod print-object ((plane plane) stream)
  (print (list 'plane (vcopy plane) (plane-distance plane)) stream))

(defun plane (normal distance)
  (%plane (vx normal) (vy normal) (vz normal)
          (etypecase distance
            (single-float distance)
            (vec3 (- (v. normal distance))))))

(defun vraysqrdist (point ray)
  (let ((diff (v- point ray)))
    (- (vsqrlength diff)
       (/ (expt (v. diff (ray-direction ray)) 2)
          (vsqrlength (ray-direction ray))))))

(defun plane-sigdist (vertices i plane)
  (+ (* (vx plane) (aref vertices (+ 0 (* i 3))))
     (* (vy plane) (aref vertices (+ 1 (* i 3))))
     (* (vz plane) (aref vertices (+ 2 (* i 3))))
     (plane-distance plane)))

(defun triangle-normal (vertices a b c)
  (let ((a (vec (aref vertices (+ (* 3 a) 0)) (aref vertices (+ (* 3 a) 1)) (aref vertices (+ (* 3 a) 2))))
        (b (vec (aref vertices (+ (* 3 b) 0)) (aref vertices (+ (* 3 b) 1)) (aref vertices (+ (* 3 b) 2))))
        (c (vec (aref vertices (+ (* 3 c) 0)) (aref vertices (+ (* 3 c) 1)) (aref vertices (+ (* 3 c) 2)))))
    (vc (v- a c) (v- b c))))

(defun above-plane-p (vertices i plane)
  (< 0 (plane-sigdist vertices i plane)))

(defstruct (half-edge
            (:constructor half-edge (&optional end opp face next)))
  (end 0 :type (unsigned-byte 32))
  (opp 0 :type (unsigned-byte 32))
  (face 0 :type (unsigned-byte 32))
  (next 0 :type (unsigned-byte 32))
  (disabled-p NIL :type boolean))

(defmethod print-object ((edge half-edge) stream)
  (format stream "#{end: ~a opp: ~a face: ~a next: ~a}"
          (half-edge-end edge) (half-edge-opp edge) (half-edge-face edge) (half-edge-next edge)))

(defstruct (face-data
            (:constructor face-data (&optional index entered-from-half-edge)))
  (index 0 :type (unsigned-byte 32))
  (entered-from-half-edge (1- (ash 1 32)) :type (unsigned-byte 32)))

(defstruct (face
            (:constructor face))
  (plane (%plane 0.0 0.0 0.0 0.0) :type plane)
  (half-edge 0 :type (unsigned-byte 32))
  (farthest-point 0 :type (unsigned-byte 32))
  (farthest-point-distance 0.0 :type single-float)
  (checked-iteration 0 :type (unsigned-byte 32))
  (visible-p NIL :type boolean)
  (in-stack-p NIL :type boolean)
  (disabled-p NIL :type boolean)
  (horizon-edges 0 :type (unsigned-byte 8))
  (points-on-positive-side (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T) :type (array (unsigned-byte 32) (*))))

(defmethod print-object ((face face) stream)
  (let ((plane (face-plane face)))
    (format stream "#{plane: P(~a,~a,~a,~a) he: ~d most-distant: ~a visible: ~a in-stack: ~a horizon: ~a disabled: ~a points: ~a}"
            (vx plane) (vy plane) (vz plane) (plane-distance plane)
            (face-half-edge face) (face-farthest-point face) 
            (face-visible-p face) (face-in-stack-p face) (face-horizon-edges face) 
            (face-disabled-p face) (face-points-on-positive-side face))))

(declaim (ftype (function (T) (simple-array single-float)) ensure-vertices))
(defun ensure-vertices (vertices)
  (etypecase vertices
    ((simple-array single-float) vertices)
    (sequence
     (map-into (make-array (length vertices) :element-type 'single-float) #'float vertices))))
