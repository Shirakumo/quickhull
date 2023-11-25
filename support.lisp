(in-package #:org.shirakumo.fraf.quickhull)

(define-condition edge-solver-failed (warning)
  ()
  (:report "Failed to construct edge loop. Convex hull may not be ideal."))

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
            (:constructor %ray (varr3 direction)))
  (direction (vec 0 0 0) :type vec3))

(defmethod print-object ((ray ray) stream)
  (print (list 'ray (vcopy ray) (ray-direction ray)) stream))

(defun ray (position direction)
  (%ray (copy-seq (varr3 position)) direction))

(defstruct (plane
            (:include vec3)
            (:constructor %plane (varr3 distance)))
  (distance 0.0 :type single-float))

(defmethod print-object ((plane plane) stream)
  (print (list 'plane (vcopy plane) (plane-distance plane)) stream))

(defun plane (normal distance)
  (%plane (copy-seq (varr3 normal))
          (etypecase distance
            (single-float distance)
            (real (float distance 0f0))
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
  (let ((a (v vertices a))
        (b (v vertices b))
        (c (v vertices c)))
    (vc (v- a c) (v- b c))))

(defun triangle-centroid (vertices a b c)
  (nv* (vec (+ (aref vertices (+ 0 (* 3 a)))
               (aref vertices (+ 0 (* 3 b)))
               (aref vertices (+ 0 (* 3 c))))
            (+ (aref vertices (+ 1 (* 3 a)))
               (aref vertices (+ 1 (* 3 b)))
               (aref vertices (+ 1 (* 3 c))))
            (+ (aref vertices (+ 2 (* 3 a)))
               (aref vertices (+ 2 (* 3 b)))
               (aref vertices (+ 2 (* 3 c)))))
       1/3))

(defun above-plane-p (vertices vertex plane)
  (< 0 (plane-sigdist vertices vertex plane)))

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
  (plane (%plane (make-array 3 :element-type 'single-float :initial-element 0f0) 0f0) :type plane)
  (half-edge 0 :type (unsigned-byte 32))
  (farthest-point 0 :type (unsigned-byte 32))
  (farthest-point-distance 0.0d0 :type double-float)
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

(declaim (ftype (function (T) simple-array) ensure-vertices))
(defun ensure-vertices (vertices)
  (etypecase vertices
    ((simple-array single-float (*)) vertices)
    ((simple-array double-float (*)) vertices)
    (sequence
     (etypecase (elt vertices 0)
       (single-float
        (map-into (make-array (length vertices) :element-type 'single-float)
                  (lambda (x) (float x 0f0)) vertices))
       (real
        (map-into (make-array (length vertices) :element-type 'double-float)
                  (lambda (x) (float x 0d0)) vertices))))))
