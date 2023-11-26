(in-package #:org.shirakumo.fraf.quickhull)

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
           (setf (face-farthest-point-distance face) 0d0)
           idx))
        (T
         (vector-push-extend (face) (faces mesh-builder))
         (1- (length (faces mesh-builder))))))

(defun add-half-edge (mesh-builder)
  (cond ((< 0 (length (disabled-half-edges mesh-builder)))
         (vector-pop (disabled-half-edges mesh-builder)))
        (T
         (vector-push-extend (half-edge) (half-edges mesh-builder))
         (1- (length (half-edges mesh-builder))))))

(defun add-point (face vertices vertex eps2)
  (let ((dist (plane-sigdist vertices vertex (face-plane face))))
    (when (and (< 0 dist) (< (* eps2 (vsqrlength (face-plane face))) (* dist dist)))
      (vector-push-extend vertex (face-points-on-positive-side face))
      (when (< (face-farthest-point-distance face) dist)
        (setf (face-farthest-point-distance face) (float dist 0d0))
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

(defun compute-extrema (vertices)
  (let (;; #(max-x min-x max-y min-y max-z min-z)
        (extrema (make-array 6 :element-type (array-element-type vertices)))
        ;; #(vertex-index-max-x v-i-min-x v-i-max-y v-i-min-y v-i-max-z v-i-min-z)
        (indices (make-array 6 :element-type '(unsigned-byte 32) :initial-element 0)))
    (setf (aref extrema 0) (setf (aref extrema 1) (aref vertices 0)))
    (setf (aref extrema 2) (setf (aref extrema 3) (aref vertices 1)))
    (setf (aref extrema 4) (setf (aref extrema 5) (aref vertices 2)))
    (loop for i from 1
          for j from 3 below (length vertices) by 3
          do (loop for c from 0 below 3
                   for ei = (* 2 c)
                   for x = (aref vertices (+ j c))
                   do (when (< (aref extrema (+ 0 ei)) x)
                        (setf (aref extrema (+ 0 ei)) x)
                        (setf (aref indices (+ 0 ei)) i))
                      (when (< x (aref extrema (+ 1 ei)))
                        (setf (aref extrema (+ 1 ei)) x)
                        (setf (aref indices (+ 1 ei)) i))))
    indices))

(defun compute-base (vertices extrema eps2)
  ;; Note: EXTREMA is the vector that is called INDICES in
  ;; COMPUTE-EXTREMA, that is, the indices of the vertices with
  ;; {min,max}x{x,y,z} components.
  (let ((vertex-count (truncate (length vertices) 3))
        base-ai base-bi base-ci base-di ; vertex indices
        base-a base-b base-c)
    ;; Try to find the two points that are furthest apart along some
    ;; axis.
    (loop with max-dist = eps2
          for i from 0 below (length extrema)
          do (loop for j from (1+ i) below (length extrema)
                   for vi1 = (aref extrema i) ; vertex index
                   for v1 = (v vertices vi1)
                   for vi2 = (aref extrema j)
                   for v2 = (v vertices vi2)
                   for dist = (vsqrdistance v1 v2)
                   when (< max-dist dist)
                   do (setf max-dist dist)
                      (setf base-ai vi1)
                      (setf base-a v1)
                      (setf base-bi vi2)
                      (setf base-b v2)))
    (unless base-ai
      (error 'points-not-distinct-error))
    ;; If we could find two base points, try to find a third that
    ;; maximizes the distance to a ray through the first two points.
    (loop with ray = (ray base-a (v- base-b base-a))
          with max-dist = eps2
          for i from 0 below vertex-count
          unless (or (= i base-ai) (= i base-bi))
          do (let* ((point (v vertices i))
                    (dist (vraysqrdist point ray)))
               (when (< max-dist dist)
                 (setf max-dist dist)
                 (setf base-ci i)
                 (setf base-c point))))
    (unless base-ci
      (error 'points-colinear-error))
    ;; Find a fourth point
    (let ((plane (plane (vc (v- base-a base-c) (v- base-b base-c)) base-a)))
      (loop with max-dist = (* eps2 (vsqrlength plane))
            for i from 0 below vertex-count
            unless (or (= i base-ai) (= i base-bi) (= i base-ci))
              do (let* ((dist* (plane-sigdist vertices i plane))
                        (dist (* dist* dist*)))
                   (when (< max-dist dist)
                     (setf max-dist dist)
                     (setf base-di i))))
      (values base-ai base-bi base-ci base-di plane))))

(defun compute-scale (extrema vertices)
  (loop for i from 0 below (length extrema)
        for v = (* 3 (aref extrema i))
        maximize (abs (aref vertices (+ v (truncate i 2))))))

(defun compute-initial-mesh (vertices extrema eps2)
  ;; Note: EXTREMA is the vector that is called INDICES in
  ;; COMPUTE-EXTREMA, that is, the indices of the vertices with
  ;; {min,max}x{x,y,z} components.
  (flet ((make-mesh-builder (a b c d)
           (make-instance 'mesh-builder :a a :b b :c c :d d)))
    (let ((num-vertices (truncate (length vertices) 3)))
      (case num-vertices
        ((0 1 2)
         (error 'too-few-points-error))
        (T
         ;; For three points, BASE-D will be missing. That case will
         ;; be handled just like a fourth point that lies in the plane
         ;; of the first three points. That is, an error will be
         ;; signaled or the point cloud will be extruded in a suitable
         ;; direction.
         (multiple-value-bind (base-a base-b base-c base-d plane)
             (compute-base vertices extrema eps2)
           (unless base-d ; extrude by adding new fourth base point
             (let* ((centroid (triangle-centroid vertices base-a base-b base-c))
                    (normal (triangle-normal vertices base-b base-c base-a))
                    (new-vertex (nv+* centroid normal (sqrt (/ eps2 (vsqrlength normal))) #+maybe (/ .001 (vlength normal))))
                    (new (make-array (+ 3 (length vertices)) :element-type (array-element-type vertices))))
               (setf base-d num-vertices)
               (replace new vertices)
               (setf (aref new (+ 0 (* 3 base-d))) (float (vx new-vertex) (aref new 0)))
               (setf (aref new (+ 1 (* 3 base-d))) (float (vy new-vertex) (aref new 0)))
               (setf (aref new (+ 2 (* 3 base-d))) (float (vz new-vertex) (aref new 0)))
               (setf vertices new)))
           (let ((mesh-builder (if (above-plane-p vertices base-d plane)
                                   (make-mesh-builder base-b base-a base-c base-d)
                                   (make-mesh-builder base-a base-b base-c base-d))))
             (loop for face across (faces mesh-builder)
                   for (a b c) = (face-vertices mesh-builder face)
                   for normal = (triangle-normal vertices a b c)
                   do (setf (face-plane face) (plane normal (v vertices a))))
             (dotimes (i num-vertices (values mesh-builder vertices))
               (loop for face across (faces mesh-builder)
                     until (add-point face vertices i eps2))))))))))

(defun reorder-horizon-edges (horizon-edges half-edges)
  (loop for i from 0 below (1- (length horizon-edges))
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

(defun quickhull (vertices &key (eps 0.0001d0) max-vertex-count)
  (let* ((vertices (ensure-vertices vertices))
         (extrema (compute-extrema vertices))
         (scale (compute-scale extrema vertices))
         (eps2 (expt (* eps scale) 2))
         (visible-faces (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T))
         (horizon-edges (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T))
         (possibly-visible-faces (make-array 0 :element-type T :adjustable T :fill-pointer T))
         (new-face-indices (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T))
         (new-half-edge-indices (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T))
         (disabled-face-points (make-array 0 :element-type T :adjustable T :fill-pointer T))
         (face-list (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T)))
    (multiple-value-bind (mesh-builder vertices) (compute-initial-mesh vertices extrema eps2)
      (let ((faces (faces mesh-builder))
            (half-edges (half-edges mesh-builder)))
        (loop for i from 0 below 4
              for face = (aref faces i)
              do (when (< 0 (length (face-points-on-positive-side face)))
                   (vector-push-extend i face-list)
                   (setf (face-in-stack-p face) T)))
        (dbg "Initial List: ~a" face-list)
        ;; Process our face stack
        (loop for iter from 1
              while (< (1- iter) (length face-list))
              do (let* ((top-face-index (aref face-list (1- iter)))
                        (top-face (aref faces top-face-index)))
                   (dbg "--------")
                   (setf (face-in-stack-p top-face) NIL)
                   (when (and (not (face-disabled-p top-face))
                              (< 0 (length (face-points-on-positive-side top-face))))
                     (dbg "Processing: ~a" top-face-index)
                     (let* ((active-index (face-farthest-point top-face))
                            (active-point (v vertices active-index)))
                       ;; Figure out the set of visible faces
                       (setf (fill-pointer horizon-edges) 0)
                       (setf (fill-pointer possibly-visible-faces) 0)
                       (setf (fill-pointer visible-faces) 0)
                       (vector-push-extend (face-data top-face-index) possibly-visible-faces)
                       (loop while (< 0 (length possibly-visible-faces))
                             for face-data = (vector-pop possibly-visible-faces)
                             for face = (aref faces (face-data-index face-data))
                             do (dbg "Examining: ~a" (face-data-index face-data))
                                (cond ((and (face-visible-p face) (= iter (face-checked-iteration face))))
                                      ((= iter (face-checked-iteration face))
                                       (setf (face-visible-p face) NIL)
                                       (vector-push-extend (face-data-entered-from-half-edge face-data) horizon-edges)
                                       (let* ((face (aref faces (half-edge-face (aref half-edges (face-data-entered-from-half-edge face-data)))))
                                              (index (position (face-data-entered-from-half-edge face-data) (face-half-edges mesh-builder face))))
                                         (setf (face-horizon-edges face) (logior (face-horizon-edges face) (ash 1 index)))))
                                      ((above-plane-p vertices active-index (face-plane face))
                                       (setf (face-checked-iteration face) iter)
                                       (setf (face-visible-p face) T)
                                       (setf (face-horizon-edges face) 0)
                                       (vector-push-extend (face-data-index face-data) visible-faces)
                                       (loop for half-edge-index in (face-half-edges mesh-builder face)
                                             for half-edge = (aref half-edges half-edge-index)
                                             do (when (/= (half-edge-opp half-edge) (face-data-entered-from-half-edge face-data))
                                                  (dbg "Queueing: ~a" (half-edge-face (aref half-edges (half-edge-opp half-edge))))
                                                  (vector-push-extend (face-data (half-edge-face (aref half-edges (half-edge-opp half-edge))) half-edge-index) possibly-visible-faces))))
                                      (T
                                       (setf (face-checked-iteration face) iter)
                                       (setf (face-visible-p face) NIL)
                                       (vector-push-extend (face-data-entered-from-half-edge face-data) horizon-edges)
                                       (let* ((face (aref faces (half-edge-face (aref half-edges (face-data-entered-from-half-edge face-data)))))
                                              (index (position (face-data-entered-from-half-edge face-data) (face-half-edges mesh-builder face))))
                                         (setf (face-horizon-edges face) (logior (face-horizon-edges face) (ash 1 index)))))))
                       (let ((horizon-edge-count (length horizon-edges))
                             (disable-counter 0))
                         (dbg "Hor: ~a" horizon-edges)
                         ;; Reorder the edges to form a loop. If this fails, skip it.
                         (cond ((not (reorder-horizon-edges horizon-edges half-edges))
                                (warn 'edge-solver-failed)
                                (setf (face-points-on-positive-side top-face) (delete active-index (face-points-on-positive-side top-face))))
                               (T
                                (dbg "Hor: ~a" horizon-edges)
                                (dbg "Vis: ~a" visible-faces)
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
                                                           (dbg "Push ~a" half-edge)
                                                           (vector-push-extend half-edge new-half-edge-indices)
                                                           (incf disable-counter))
                                                          (T
                                                           (dbg "Disable ~a" half-edge)
                                                           (disable-half-edge mesh-builder half-edge)))))
                                         (dbg "Dis ~a" face-index)
                                         (let ((points (disable-face mesh-builder face-index)))
                                           (when (< 0 (length points))
                                             (dbg "Pushing")
                                             (vector-push-extend points disabled-face-points))))
                                (when (< disable-counter (* horizon-edge-count 2))
                                  (dbg "Needed: ~a" (- (* horizon-edge-count 2) disable-counter))
                                  (dotimes (i (- (* horizon-edge-count 2) disable-counter))
                                    (vector-push-extend (add-half-edge mesh-builder) new-half-edge-indices)))
                                ;; Create new faces using the edge loop
                                (loop for i from 0 below horizon-edge-count
                                      for ab = (aref horizon-edges i)
                                      for (a b) = (half-edge-vertices mesh-builder (aref half-edges ab))
                                      for c = active-index
                                      for new-face-index = (add-face mesh-builder)
                                      for new-face = (aref faces new-face-index)
                                      do (dbg "A: ~a B: ~a C: ~a" a b c)
                                         (vector-push-extend new-face-index new-face-indices)
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
                                           (dbg "CAOPP: ~a" (half-edge-opp he-ca))
                                           (dbg "BCOPP: ~a" (half-edge-opp he-bc))
                                           (setf (face-half-edge new-face) ab)
                                           (setf (face-disabled-p new-face) NIL)
                                           (setf (face-plane new-face) (plane (triangle-normal vertices a b c) active-point))))
                                ;; Reassign points that were on the positive side of the disabled faces
                                (loop for disabled-points across disabled-face-points
                                      do (loop for point across disabled-points
                                               do (if (/= point active-index)
                                                      (loop initially (dbg "B ~a" point)
                                                            for j from 0 below horizon-edge-count
                                                            until (add-point (aref faces (aref new-face-indices j)) vertices point eps2))
                                                      (dbg "A ~a" point))))
                                ;; Increase our stack again if necessary
                                (loop for face-index across new-face-indices
                                      for face = (aref faces face-index)
                                      do (when (and (< 0 (length (face-points-on-positive-side face)))
                                                    (not (face-in-stack-p face)))
                                           (dbg "Pushing face: ~a" face-index)
                                           (vector-push-extend face-index face-list)
                                           (setf (face-in-stack-p face) T)))))))))))
      (values mesh-builder vertices))))

(defun extract-half-edge-mesh (mesh-builder in-vertices)
  (let ((face-mapping (make-hash-table :test 'eql))
        (half-edge-mapping (make-hash-table :test 'eql))
        (vertex-mapping (make-hash-table :test 'eql))
        (vertices (make-array 0 :element-type (array-element-type in-vertices) :adjustable T :fill-pointer T))
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
                     for point = (* 3 vertex)
                     do (unless (gethash vertex vertex-mapping)
                          (setf (gethash vertex vertex-mapping) (length vertices))
                          (vector-push-extend (aref in-vertices (+ point 0)) vertices)
                          (vector-push-extend (aref in-vertices (+ point 1)) vertices)
                          (vector-push-extend (aref in-vertices (+ point 2)) vertices)))))
    (loop for half-edge across (half-edges mesh-builder)
          for i from 0
          do (unless (half-edge-disabled-p half-edge)
               (setf (gethash i half-edge-mapping) (length half-edges))
               (vector-push-extend half-edge half-edges)))
    (values (make-array (length vertices) :element-type (array-element-type vertices) :initial-contents vertices)
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
  (multiple-value-bind (mesh-builder vertices) (apply #'quickhull vertices args)
    (extract-half-edge-mesh mesh-builder vertices)))

(defun sbitp (array index)
  (= 1 (sbit array index)))

(defun extract-convex-hull (mesh-builder in-vertices &key (reduce-vertices T))
  (dbg "============")
  (let* ((vertex-index-mapping (make-hash-table :test 'eql))
         (faces (faces mesh-builder))
         (half-edges (half-edges mesh-builder))
         (vertices (make-array 0 :element-type (array-element-type in-vertices) :adjustable T :fill-pointer T))
         (processed-faces (make-array (length faces) :element-type 'bit))
         (face-stack (list (loop for i from 0 below (length faces)
                                 unless (face-disabled-p (aref faces i))
                                 return i)))
         (indices (make-array (* 3 (- (length faces) (length (disabled-faces mesh-builder))))
                              :element-type '(unsigned-byte 32)))
         (index-ptr 0))
    (dbg "Initial Stack: ~a" face-stack)
    (loop for face-index = (pop face-stack)
          while face-index
          do (unless (sbitp processed-faces face-index)
               (setf (sbit processed-faces face-index) 1)
               (dbg "Top: ~a" face-index)
               (let* ((face (aref faces face-index))
                      (vertex-indices (face-vertices mesh-builder face)))
                 (loop for half-edge-index in (face-half-edges mesh-builder face)
                       for half-edge = (aref half-edges half-edge-index)
                       for face = (half-edge-face (aref half-edges (half-edge-opp half-edge)))
                       do (when (and (not (sbitp processed-faces face))
                                     (not (face-disabled-p (aref faces face))))
                            (push face face-stack)))
                 (loop for cons on vertex-indices
                       for vertex = (car cons)
                       for mapping = (gethash vertex vertex-index-mapping)
                       do (cond (mapping
                                 (setf (car cons) mapping))
                                (reduce-vertices
                                 (let ((point (* 3 vertex)))
                                   (vector-push-extend (aref in-vertices (+ point 0)) vertices)
                                   (vector-push-extend (aref in-vertices (+ point 1)) vertices)
                                   (vector-push-extend (aref in-vertices (+ point 2)) vertices)
                                   (setf (car cons) (1- (truncate (length vertices) 3)))
                                   (setf (gethash vertex vertex-index-mapping) (car cons))))
                                (T)))
                 (setf (aref indices (+ 2 index-ptr)) (first vertex-indices))
                 (setf (aref indices (+ 1 index-ptr)) (third vertex-indices))
                 (setf (aref indices (+ 0 index-ptr)) (second vertex-indices))
                 (incf index-ptr 3))))
    (values (if reduce-vertices
                (make-array (length vertices) :element-type (array-element-type vertices) :initial-contents vertices)
                in-vertices)
            indices)))

(defun convex-hull (vertices &key (reduce-vertices T) (eps 0.0001d0))
  (multiple-value-bind (mesh-builder vertices) (quickhull vertices :eps eps)
    (extract-convex-hull mesh-builder vertices :reduce-vertices reduce-vertices)))
