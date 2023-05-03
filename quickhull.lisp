(defpackage #:org.shirakumo.fraf.quickhull
  (:use #:cl #:org.shirakumo.flare.vector)
  (:export))

(in-package #:org.shirakumo.fraf.quickhull)

(defstruct (ray
            (:include vec3)
            (:constructor %ray (3d-vectors::%vx3 3d-vectors::%vy3 3d-vectors::%vz3
                                direction)))
  (direction (vec 0 0 0) :type vec3))

(defun ray (position direction)
  (%plane (vx position) (vy position) (vz position) direction))

(defstruct (plane
            (:include vec3)
            (:constructor %plane (3d-vectors::%vx3 3d-vectors::%vy3 3d-vectors::%vz3
                                  distance)))
  (distance 0.0 :type single-float))

(defun plane (normal distance)
  (%plane (vx normal) (vy normal) (vz normal) distance))

(defun vraysqrdist (point ray)
  (let ((diff (v- point ray)))
    (- (vsqrlength diff)
       (/ (expt (v. diff (ray-direction ray)) 2)
          (vsqrlength (ray-direction ray))))))

(defun plane-sigdist (point plane)
  (+ (v. point plane) (plane-distance plane)))

(defun triangle-normal (a b c)
  (vc (v- a c) (v- b c)))

(defstruct (half-edge
            (:constructor half-edge (&optional end opp face next)))
  (end 0 :type (unsigned-byte 32))
  (opp 0 :type (unsigned-byte 32))
  (face 0 :type (unsigned-byte 32))
  (next 0 :type (unsigned-byte 32))
  (disabled-p NIL :type boolean))

(defstruct (face
            (:include plane)
            (:constructor face))
  (half-edge 0 :type (unsigned-byte 32))
  (farthest-point 0 :type (unsigned-byte 32))
  (farthest-point-distance 0.0 :type single-float)
  (visible-p T :type boolean)
  (in-stack-p T :type boolean)
  (disabled-p T :type boolean)
  (horizon-edges #b11 :type (unsigned-byte 8))
  (points-on-positive-side (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T) :type (array (unsigned-byte 32) (*))))

(defclass mesh-builder ()
  ((faces :initform (make-array 0 :adjustable T :fill-pointer T) :accessor faces)
   (half-edges :initform (make-array 0 :adjustable T :fill-pointer T) :accessor half-edges)
   (disabled-faces :initform (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T) :accessor disabled-faces)
   (disabled-half-edges :initform (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T) :accessor disabled-half-edges)))

(defmethod shared-initialize ((builder mesh-builder) slots &key a b c d)
  (when (and a b c d)
    (let ((faces (faces builder))
          (half-edges (half-edges builder)))
      (setf (fill-pointer faces) 0)
      (setf (fill-pointer half-edges) 0)
      (setf (fill-pointer (disabled-faces builder)) 0)
      (setf (fill-pointer (disabled-half-edges builder)) 0)
      (vector-push-extend (half-edge b 6 0 1) half-edges)
      (vector-push-extend (half-edge c 9 0 2) half-edges)
      (vector-push-extend (half-edge a 3 0 0) half-edges)
      (vector-push-extend (half-edge c 2 1 4) half-edges)
      (vector-push-extend (half-edge d 11 1 5) half-edges)
      (vector-push-extend (half-edge a 7 1 3) half-edges)
      (vector-push-extend (half-edge a 0 2 7) half-edges)
      (vector-push-extend (half-edge d 5 2 8) half-edges)
      (vector-push-extend (half-edge b 10 2 6) half-edges)
      (vector-push-extend (half-edge b 1 3 10) half-edges)
      (vector-push-extend (half-edge d 8 3 11) half-edges)
      (vector-push-extend (half-edge c 4 3 9) half-edges)
      (vector-push-extend (face :half-edge 0) faces)
      (vector-push-extend (face :half-edge 3) faces)
      (vector-push-extend (face :half-edge 6) faces)
      (vector-push-extend (face :half-edge 9) faces))))

(defun add-face (mesh-builder)
  (cond ((< 0 (length (disabled-faces mesh-builder)))
         (let* ((idx (vector-pop (disabled-faces mesh-builder)))
                (face (aref (faces mesh-builder) idx)))
           (setf (face-farthest-point-distance face) 0.0)
           (setf (face-disabled-p face) NIL)
           idx))
        (T
         (vector-push-extend (face) (faces mesh-builder))
         (1- (length (faces mesh-builder))))))

(defun add-half-edge (mesh-builder)
  (cond ((< 0 (length (disabled-half-edges mesh-builder)))
         (let* ((idx (vector-pop (disabled-half-edges mesh-builder)))
                (half-edge (aref (half-edges mesh-builder) idx)))
           (setf (half-edge-disabled-p half-edge) NIL)
           idx))
        (T
         (vector-push-extend (half-edge) (half-edges mesh-builder))
         (1- (length (half-edges mesh-builder))))))

(defun disable-face (mesh-builder index)
  (setf (disabled-p (aref (faces mesh-builder) index)) T)
  (vector-push-extend index (disabled-faces mesh-builder)))

(defun disable-half-edge (mesh-builder index)
  (setf (disabled-p (aref (half-edges mesh-builder) index)) T)
  (vector-push-extend index (disabled-half-edges mesh-builder)))

(defun face-vertices (mesh-builder face)
  (let ((half-edge (aref (half-edges mesh-builder) (face-half-edge face))))
    (loop repeat 3
          collect (half-edge-end half-edge)
          do (setf half-edge (aref (half-edges mesh-builder) (half-edge-next half-edge))))))

(defun half-edge-vertices (mesh-builder half-edge)
  (list (half-edge-end (aref (mesh-builder-half-edges mesh-builder) (half-edge-opp half-edge)))
        (half-edge-end half-edge)))

(defun face-half-edges (mesh-builder face)
  (let ((half-edge (face-half-edge face)))
    (loop repeat 3
          collect (aref (half-edges mesh-builder) half-edge)
          do (setf half-edge (half-edge-next (aref (half-edges mesh-builder) half-edge))))))

(defclass half-edge-mesh ()
  ((vertices :accessor vertices)
   (faces :accessor faces)
   (half-edges :accessor half-edges)))

(defmethod initialize-instance :after ((mesh half-edge-mesh) &key mesh-builder in-vertices)
  (let ((face-mapping (make-hash-table :test 'eql))
        (half-edge-mapping (make-hash-table :test 'eql))
        (vertex-mapping (make-hash-table :test 'eql))
        (vertices (make-array 0 :element-type 'single-float :adjustable T :fill-pointer T))
        (faces (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T))
        (half-edges (make-array 0 :element-type 'half-edge :adjustable T :fill-pointer T)))
    (loop for face across (faces mesh-builder)
          for i from 0
          do (unless (face-disabled-p face)
               (setf (gethash i face-mapping) (length faces))
               (vector-push-extend (face-half-edge face) faces)
               (loop for half-edge in (face-half-edges mesh-builder face)
                     for vertex = (half-edge-end half-edge)
                     do (unless (gethash vertex vertex-mapping)
                          (setf (gethash vertex vertex-mapping) (length vertices))
                          (vector-push-extend (aref in-vertices vertex) vertices)))))
    (loop for half-edge across (half-edges mesh-builder)
          for i from 0
          do (unless (half-edge-disabled-p half-edge)
               (setf (gethash i half-edge-mapping) (length half-edges))
               (vector-push-extend half-edge half-edges)))
    (setf (vertices mesh) (map '(simple-array single-float (*)) #'identity vertices))
    (setf (faces mesh) (map '(simple-array (unsigned-byte 32) (*))
                            (lambda (x) (gethash x half-edge-mapping))
                            faces))
    (setf (half-edges mesh) (map '(simple-array half-edge (*))
                                 (lambda (x)
                                   (half-edge (gethash (half-edge-end x) vertex-mapping)
                                              (gethash (half-edge-opp x) half-edge-mapping)
                                              (gethash (half-edge-face x) face-mapping)
                                              (gethash (half-edge-next x) half-edge-mapping)))
                                 half-edges))))
