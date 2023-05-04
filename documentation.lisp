#|
 This file is a part of quickhull
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.quickhull)

(docs:define-docs
  (type edge-solver-failed
    "Warning signalled when the solver fails due to imprecision.

See CONVEX-HULL")
  
  (function convex-hull
    "Extract a convex hull mesh from the given set of vertices.

VERTICES should be a sequence of REALs which describe the point cloud
to extract a convex hull from. Each vertex should come in the form of
consecutive X, Y, and Z coordinates. It is preferred that VERTICES be
a (SIMPLE-ARRAY SINGLE-FLOAT). Otherwise, the array is copied and
elements coerced to single-floats.

If VERTICES only contains 2 or fewer vertices, an error is signalled.
If VERTICES describes a point cloud that doesn't have any volume, an
error is also signalled.

Returns two values:
  1. A (SIMPLE-ARRAY SINGLE-FLOAT) of vertices that lie on the convex
     hull, in the same format as the input.
  2. A (SIMPLE-ARRAY (UNSIGNED-BYTE 32)) of vertex indices, every
     triplet of which describes a triangle face on the hull. The
     vertex index must be multiplied by 3 in order to get the index of
     the first vertex in the returned vertices array.

If a convex hull cannot be constructed exactly due to floating point
imprecision, a warning of type EDGE-SOLVER-FAILED may be signalled. If
EDGE-SOLVER-FAILED is signalled, the resulting convex hull may not be
optimal.

See EDGE-SOLVER-FAILED"))
