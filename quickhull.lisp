(defpackage #:org.shirakumo.fraf.quickhull
  (:use #:cl #:org.shirakumo.flare.vector)
  (:export))

(in-package #:org.shirakumo.fraf.quickhull)

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

(defun plane-sigdist (point plane)
  (+ (v. point plane) (plane-distance plane)))

(defun triangle-normal (a b c)
  (vc (v- a c) (v- b c)))

(defun above-plane-p (point plane)
  (<= 0 (+ (plane-distance plane) (v. plane point))))

(defstruct (half-edge
            (:constructor half-edge (&optional end opp face next)))
  (end 0 :type (unsigned-byte 32))
  (opp 0 :type (unsigned-byte 32))
  (face 0 :type (unsigned-byte 32))
  (next 0 :type (unsigned-byte 32))
  (disabled-p NIL :type boolean))

(defmethod print-object ((edge half-edge) stream)
  (format stream "#{end: ~a opp: ~a face: ~a next: ~a disabled: ~a}"
          (half-edge-end edge) (half-edge-opp edge) (half-edge-face edge)
          (half-edge-next edge) (half-edge-disabled-p edge)))

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
    (format stream "#{plane: P(~a,~a,~a,~a) he: ~d most-distant: ~a visible: ~a in-stack: ~a horizon: ~a points: ~a}"
            (vx plane) (vy plane) (vz plane) (plane-distance plane)
            (face-half-edge face) (face-farthest-point face) 
            (face-visible-p face) (face-in-stack-p face) (face-horizon-edges face) (face-points-on-positive-side face))))

(defclass mesh-builder ()
  ((faces :initform (make-array 0 :adjustable T :fill-pointer T) :accessor faces)
   (half-edges :initform (make-array 0 :adjustable T :fill-pointer T) :accessor half-edges)
   (disabled-faces :initform (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T) :accessor disabled-faces)
   (disabled-half-edges :initform (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T) :accessor disabled-half-edges)))

(defmethod initialize-instance :after ((builder mesh-builder) &key a b c d)
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
    (vector-push-extend (face :half-edge 9) faces)))

(defmethod print-object ((mesh-builder mesh-builder) stream)
  (print-unreadable-object (mesh-builder stream :type T)
    (format stream "~d face~:p ~d half-edge~:p"
            (- (length (faces mesh-builder))
               (length (disabled-faces mesh-builder)))
            (- (length (half-edges mesh-builder))
               (length (disabled-half-edges mesh-builder))))))

(defmethod describe-object ((mesh-builder mesh-builder) stream)
  (format stream "Faces:")
  (loop for face across (faces mesh-builder)
        do (format stream "~%  ~a" face))
  (format stream "~%Half Edges:")
  (loop for half-edge across (half-edges mesh-builder)
        do (format stream "~%  ~a" half-edge)))

(defun add-face (mesh-builder)
  (cond ((< 0 (length (disabled-faces mesh-builder)))
         (let* ((idx (vector-pop (disabled-faces mesh-builder)))
                (face (aref (faces mesh-builder) idx)))
           (setf (face-farthest-point-distance face) 0.0)
           idx))
        (T
         (vector-push-extend (face) (faces mesh-builder))
         (1- (length (faces mesh-builder))))))

(defun add-half-edge (mesh-builder)
  (cond ((< 0 (length (disabled-half-edges mesh-builder)))
         (let* ((idx (vector-pop (disabled-half-edges mesh-builder)))
                (half-edge (aref (half-edges mesh-builder) idx)))
           idx))
        (T
         (vector-push-extend (half-edge) (half-edges mesh-builder))
         (1- (length (half-edges mesh-builder))))))

(defun add-point (face points vertex eps2)
  (let ((dist (plane-sigdist (aref points vertex) (face-plane face))))
    (when (and (< 0 dist) (< (* eps2 (vsqrlength (face-plane face))) (* dist dist)))
      (vector-push-extend vertex (face-points-on-positive-side face))
      (when (< (face-farthest-point-distance face) dist)
        (setf (face-farthest-point-distance face) dist)
        (setf (face-farthest-point face) vertex))
      T)))

(defun disable-face (mesh-builder index)
  (let ((face (aref (faces mesh-builder) index)))
    (setf (face-disabled-p face) T)
    (vector-push-extend index (disabled-faces mesh-builder))
    (shiftf (face-points-on-positive-side face) (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T))))

(defun disable-half-edge (mesh-builder index)
  (let ((half-edge (aref (half-edges mesh-builder) index)))
    (setf (half-edge-disabled-p half-edge) T)
    (vector-push-extend index (disabled-half-edges mesh-builder))
    half-edge))

(defun face-vertices (mesh-builder face)
  (let ((half-edge (aref (half-edges mesh-builder) (face-half-edge face))))
    (loop repeat 3
          collect (half-edge-end half-edge)
          do (setf half-edge (aref (half-edges mesh-builder) (half-edge-next half-edge))))))

(defun half-edge-vertices (mesh-builder half-edge)
  (list (half-edge-end (aref (half-edges mesh-builder) (half-edge-opp half-edge)))
        (half-edge-end half-edge)))

(defun face-half-edges (mesh-builder face)
  (let ((half-edge (face-half-edge face)))
    (loop repeat 3
          collect half-edge
          do (setf half-edge (half-edge-next (aref (half-edges mesh-builder) half-edge))))))

(defun vertices->points (vertices)
  (let ((points (make-array (truncate (length vertices) 3))))
    (loop for v from 0 below (length vertices) by 3
          for p from 0
          do (setf (aref points p) (vec (aref vertices (+ v 0))
                                        (aref vertices (+ v 1))
                                        (aref vertices (+ v 2)))))
    points))

(defun compute-extrema (points)
  (let ((extrema (make-array 6 :element-type 'single-float))
        (indices (make-array 6 :element-type '(unsigned-byte 32))))
    (loop for point across points
          for i from 0
          do (cond ((< (aref extrema 0) (vx point)) (setf (aref extrema 0) (vx point)) (setf (aref indices 0) i))
                   ((< (vx point) (aref extrema 1)) (setf (aref extrema 1) (vx point)) (setf (aref indices 1) i)))
             (cond ((< (aref extrema 2) (vy point)) (setf (aref extrema 2) (vy point)) (setf (aref indices 2) i))
                   ((< (vy point) (aref extrema 3)) (setf (aref extrema 3) (vy point)) (setf (aref indices 3) i)))
             (cond ((< (aref extrema 4) (vz point)) (setf (aref extrema 4) (vz point)) (setf (aref indices 4) i))
                   ((< (vz point) (aref extrema 5)) (setf (aref extrema 5) (vz point)) (setf (aref indices 5) i))))
    indices))

(defun compute-scale (extrema points)
  (loop for i from 0 below (length extrema)
        for v = (aref points (aref extrema i))
        for a = (abs (ecase (truncate i 2)
                       (0 (vx v))
                       (1 (vy v))
                       (2 (vz v))))
        maximize a))

(defun compute-initial-mesh (points extrema eps2)
  (flet ((make-mesh-builder (a b c d)
           (make-instance 'mesh-builder :a a :b b :c c :d d)))
    (case (length points)
      ((0 1 2)
       (error "Mesh has no volume. Not enough points to form a triangle."))
      ((3 4)
       (let ((plane (plane (triangle-normal (aref points 0) (aref points 1) (aref points 2)) (aref points 0))))
         (if (above-plane-p (aref points (min 3 (length points))) plane)
             (make-mesh-builder 1 0 2 (min 3 (length points)))
             (make-mesh-builder 0 1 2 (min 3 (length points))))))
      (T
       (let (base-a base-b base-c base-d)
         (let ((max-dist eps2))
           (loop for i from 0 below (length extrema)
                 do (loop for j from (1+ i) below (length extrema)
                          for dist = (vsqrdistance (aref points (aref extrema i)) (aref points (aref extrema j)))
                          do (when (< max-dist dist)
                               (setf max-dist dist)
                               (setf base-a (aref extrema i))
                               (setf base-b (aref extrema j)))))
           (unless base-a
             (error "Mesh has no volume. All points are the same.")))
         (let ((max-dist eps2)
               (ray (ray (aref points base-a) (v- (aref points base-b) (aref points base-a)))))
           (loop for i from 0 below (length points)
                 for point = (aref points i)
                 for dist = (vraysqrdist point ray)
                 do (when (< max-dist dist)
                      (setf max-dist dist)
                      (setf base-c i)))
           (unless base-c
             (error "Mesh has no volume. All points are part of a 1-dimensional line.")))
         (let ((max-dist eps2)
               (plane (plane (triangle-normal (aref points base-a) (aref points base-b) (aref points base-c)) (aref points base-a))))
           (loop for i from 0 below (length points)
                 for point = (aref points i)
                 for dist = (plane-sigdist point plane)
                 do (when (< max-dist dist)
                      (setf max-dist dist)
                      (setf base-d i)))
           (unless base-d
             ;; FIXME: 2D case. Inject an extra point to give the mesh volume and remove the point again on finish.
             (error "Mesh has no volume. All points are part of a 2-dimensional plane."))
           (let ((mesh-builder (if (above-plane-p (aref points base-d) plane)
                                   (make-mesh-builder base-b base-a base-c base-d)
                                   (make-mesh-builder base-a base-b base-c base-d))))
             (loop for face across (faces mesh-builder)
                   for (a b c) = (face-vertices mesh-builder face)
                   for normal = (triangle-normal (aref points a) (aref points b) (aref points c))
                   do (setf (face-plane face) (plane normal (aref points a))))
             (dotimes (i (length points) mesh-builder)
               (loop for face across (faces mesh-builder)
                     until (add-point face points i eps2))))))))))

(defun reorder-horizon-edges (horizon-edges half-edges)
  (loop for i from 0 below (length horizon-edges)
        for end = (half-edge-end (aref half-edges (aref horizon-edges i)))
        for found-next = NIL
        do (loop for j from (1+ i) below (length horizon-edges)
                 for begin = (half-edge-end (aref half-edges (half-edge-opp (aref half-edges (aref horizon-edges j)))))
                 do (when (= begin end)
                      (rotatef (aref horizon-edges j) (aref horizon-edges (1+ i)))
                      (setf found-next T)
                      (return)))
           (unless found-next
             (return NIL))
        finally (return T)))

(defun quickhull (vertices &key (eps 0.0001))
  ;; TODO: Avoid copying of vertices to points
  (let* ((points (vertices->points vertices))
         (extrema (compute-extrema points))
         (scale (compute-scale extrema points))
         (eps2 (expt (* eps scale) 2))
         (mesh-builder (compute-initial-mesh points extrema eps2))
         (faces (faces mesh-builder))
         (half-edges (half-edges mesh-builder))
         (visible-faces (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T))
         (horizon-edges (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T))
         (possibly-visible-faces (make-array 0 :element-type T :adjustable T :fill-pointer T))
         (new-face-indices (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T))
         (new-half-edge-indices (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T))
         (disabled-face-points (make-array 0 :element-type T :adjustable T :fill-pointer T))
         (face-list (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T)))
    (loop for i from 0 below 4
          for face = (aref faces i)
          do (when (< 0 (length (face-points-on-positive-side face)))
               (vector-push-extend i face-list)
               (setf (face-in-stack-p face) T)))
    ;; Process our face stack
    (loop for iter from 0
          while (< iter (length face-list))
          do (let* ((top-face-index (aref face-list iter))
                    (top-face (aref faces top-face-index)))
               (setf (face-in-stack-p top-face) NIL)
               (when (and (not (face-disabled-p top-face))
                          (< 0 (length (face-points-on-positive-side top-face))))
                 (print top-face-index)
                 (let* ((active-index (face-farthest-point top-face))
                        (active-point (aref points active-index)))
                   ;; Figure out the set of visible faces
                   (setf (fill-pointer horizon-edges) 0)
                   (setf (fill-pointer possibly-visible-faces) 0)
                   (setf (fill-pointer visible-faces) 0)
                   (vector-push-extend (face-data top-face-index) possibly-visible-faces)
                   (loop while (< 0 (length possibly-visible-faces))
                         for face-data = (vector-pop possibly-visible-faces)
                         for face = (aref faces (print (face-data-index face-data)))
                         do (cond ((and (face-visible-p face) (= iter (face-checked-iteration face))))
                                  ((above-plane-p active-point (face-plane face))
                                   (setf (face-checked-iteration face) iter)
                                   (setf (face-visible-p face) T)
                                   (setf (face-horizon-edges face) 0)
                                   (vector-push-extend (face-data-index face-data) visible-faces)
                                   (loop for half-edge-index in (face-half-edges mesh-builder face)
                                         for half-edge = (aref half-edges half-edge-index)
                                         do (when (/= (face-data-entered-from-half-edge face-data) (half-edge-opp half-edge))
                                              (vector-push-extend (face-data (half-edge-opp half-edge) half-edge-index) possibly-visible-faces))))
                                  (T
                                   (setf (face-checked-iteration face) iter)
                                   (setf (face-visible-p face) NIL)
                                   (vector-push-extend (face-data-entered-from-half-edge face-data) horizon-edges)
                                   (let* ((face (aref faces (half-edge-face (aref half-edges (face-data-entered-from-half-edge face-data)))))
                                          (index (position (face-data-entered-from-half-edge face-data) (face-half-edges mesh-builder face))))
                                     (setf (face-horizon-edges face) (logior (face-horizon-edges face) (ash 1 index)))))))
                   (let ((horizon-edge-count (length horizon-edges))
                         (disable-counter 0))
                     ;; Reorder the edges to form a loop. If this fails, skip it.
                     (cond ((not (reorder-horizon-edges horizon-edges half-edges))
                            (warn "Failed to solve for edge.")
                            (setf (face-points-on-positive-side top-face) (delete active-index (face-points-on-positive-side top-face))))
                           (T
                            ;; Disable edges and faces not on the horizon
                            (setf (fill-pointer new-face-indices) 0)
                            (setf (fill-pointer new-half-edge-indices) 0)
                            (setf (fill-pointer disabled-face-points) 0)
                            (loop for face-index across visible-faces
                                  for disabled-face = (aref faces face-index)
                                  for half-edges = (face-half-edges mesh-builder disabled-face)
                                  do (loop for j from 0 below 3
                                           for half-edge in half-edges
                                           do (when (= 0 (logand (ash 1 j) (face-horizon-edges disabled-face)))
                                                (cond ((< disable-counter (* horizon-edge-count 2))
                                                       (vector-push-extend half-edge new-half-edge-indices)
                                                       (incf disable-counter))
                                                      (T
                                                       (disable-half-edge mesh-builder half-edge)))))
                                     (let ((points (disable-face mesh-builder face-index)))
                                       (when (< 0 (length points))
                                         (vector-push-extend points disabled-face-points))))
                            (when (< disable-counter (* horizon-edge-count 2))
                              (dotimes (i (- (* horizon-edge-count 2) disable-counter))
                                (vector-push-extend (add-half-edge mesh-builder) new-half-edge-indices)))
                            ;; Create new faces using the edge loop
                            (loop for i from 0 below horizon-edge-count
                                  for ab = (aref horizon-edges i)
                                  for (a b) = (half-edge-vertices mesh-builder (aref half-edges ab))
                                  for c = active-index
                                  for new-face-index = (add-face mesh-builder)
                                  for new-face = (aref faces new-face-index)
                                  do (vector-push-extend new-face-index new-face-indices)
                                     (let* ((ca (aref new-half-edge-indices (+ (* 2 i) 0)))
                                            (bc (aref new-half-edge-indices (+ (* 2 i) 1)))
                                            (he-ab (aref half-edges ab))
                                            (he-bc (aref half-edges bc))
                                            (he-ca (aref half-edges ca)))
                                       (setf (half-edge-next he-ab) bc)
                                       (setf (half-edge-next he-bc) ca)
                                       (setf (half-edge-next he-ca) ab)
                                       (setf (half-edge-face he-ab) new-face-index)
                                       (setf (half-edge-face he-bc) new-face-index)
                                       (setf (half-edge-face he-ca) new-face-index)
                                       (setf (half-edge-end he-ca) a)
                                       (setf (half-edge-end he-bc) c)
                                       (setf (half-edge-opp he-ca) (aref new-half-edge-indices (1- (if (< 0 i) (* i 2) (* horizon-edge-count 2)))))
                                       (setf (half-edge-opp he-bc) (aref new-half-edge-indices (mod (* 2 (1+ i)) (* horizon-edge-count 2))))
                                       (setf (face-plane new-face) (plane (triangle-normal (aref points a) (aref points b) active-point) active-point))))
                            ;; Reassign points that were on the positive side of the disabled faces
                            (loop for disabled-points across disabled-face-points
                                  do (loop for point across disabled-points
                                           do (when (/= point active-index)
                                                (loop for j from 0 below horizon-edge-count
                                                      until (add-point (aref faces (aref new-face-indices j)) points point eps2)))))
                            ;; Increase our stack again if necessary
                            (loop for face-index across new-face-indices
                                  for face = (aref faces face-index)
                                  do (when (and (< 0 (length (face-points-on-positive-side face)))
                                                (not (face-in-stack-p face)))
                                       (push face-index face-list)
                                       (setf (face-in-stack-p face) T))))))))))
    (describe mesh-builder)
    (values mesh-builder points)))

(defun extract-half-edge-mesh (mesh-builder points)
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
               (loop for half-edge-index in (face-half-edges mesh-builder face)
                     for half-edge = (aref half-edges half-edge-index)
                     for vertex = (half-edge-end half-edge)
                     for point = (aref points vertex)
                     do (unless (gethash vertex vertex-mapping)
                          (setf (gethash vertex vertex-mapping) (length vertices))
                          (vector-push-extend (vx point) vertices)
                          (vector-push-extend (vy point) vertices)
                          (vector-push-extend (vz point) vertices)))))
    (loop for half-edge across (half-edges mesh-builder)
          for i from 0
          do (unless (half-edge-disabled-p half-edge)
               (setf (gethash i half-edge-mapping) (length half-edges))
               (vector-push-extend half-edge half-edges)))
    (values (map '(simple-array single-float (*)) #'identity vertices)
            (map '(simple-array (unsigned-byte 32) (*))
                 (lambda (x) (gethash x half-edge-mapping))
                 faces)
            (map '(simple-array half-edge (*))
                 (lambda (x)
                   (half-edge (gethash (half-edge-end x) vertex-mapping)
                              (gethash (half-edge-opp x) half-edge-mapping)
                              (gethash (half-edge-face x) face-mapping)
                              (gethash (half-edge-next x) half-edge-mapping)))
                 half-edges))))

(defun half-edge-mesh (vertices &rest args)
  (multiple-value-bind (mesh-builder points) (apply #'quickhull vertices args)
    (extract-half-edge-mesh mesh-builder points)))

(defun sbitp (array index)
  (= 1 (sbit array index)))

(defun extract-convex-hull (mesh-builder points)
  (let* ((vertex-index-mapping (make-hash-table :test 'eql))
         (faces (faces mesh-builder))
         (half-edges (half-edges mesh-builder))
         (vertices (make-array 0 :element-type 'single-float :adjustable T :fill-pointer T))
         (processed-faces (make-array (length faces) :element-type 'bit))
         (face-stack (loop for i from 0 below (length faces)
                           unless (face-disabled-p (aref faces i))
                           collect i))
         (indices (make-array (- (length faces) (length (disabled-faces mesh-builder)))
                              :element-type '(unsigned-byte 32)))
         (index-ptr 0))
    (loop for face-index = (pop face-stack)
          while face-index
          do (unless (sbitp processed-faces face-index)
               (setf (sbit processed-faces face-index) 1)
               (let* ((face (aref faces face-index))
                      (vertex-indices (face-vertices mesh-builder face)))
                 (loop for half-edge-index in (face-half-edges mesh-builder face)
                       for half-edge = (aref half-edges half-edge-index)
                       for face = (half-edge-face (aref half-edges (half-edge-opp half-edge)))
                       do (when (and (not (sbitp processed-faces face))
                                     (not (face-disabled-p (aref faces face))))
                            (push face face-stack)))
                 (loop for vertex in vertex-indices
                       for index-offset in '(0 2 1) ;; CCW reordering
                       for mapping = (gethash vertex vertex-index-mapping)
                       do (if mapping
                              (setf (aref indices (+ index-ptr index-offset)) mapping)
                              (let ((index (truncate (length vertices) 3))
                                    (point (aref points vertex)))
                                (setf (gethash vertex vertex-index-mapping) index)
                                (setf (aref indices (+ index-ptr index-offset)) index)
                                (vector-push-extend (vx point) vertices)
                                (vector-push-extend (vy point) vertices)
                                (vector-push-extend (vz point) vertices))))
                 (incf index-ptr 3))))
    (values (coerce vertices '(simple-array single-float (*)))
            indices)))

(defun convex-hull (vertices &rest args)
  (multiple-value-bind (mesh-builder points) (apply #'quickhull vertices args)
    (extract-convex-hull mesh-builder points)))
