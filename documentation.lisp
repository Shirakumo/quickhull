(in-package #:org.shirakumo.fraf.quickhull)

(docs:define-docs
  (type too-few-points-error
    "Error signaled when point cloud consists of too few (one or two)
points.")

  (type points-not-distinct-error
    "Error signaled when the point cloud consists only of identical
points.")

  (type points-colinear-error
    "Error signaled when all points in the point cloud lie on a
1-dimensional line.")

  (type points-in-plane-error
    "Error signaled when all points in the point cloud lie on a
2-dimensional plane and the caller has requested to not extrude the
point cloud..")

  (type edge-solver-failed
    "Warning signalled when the solver fails due to imprecision.

See CONVEX-HULL")

  (function convex-hull
    "Extract a convex hull mesh from the given set of vertices.

VERTICES should be a sequence of FLOATs which describe the point cloud
to extract a convex hull from. Each vertex should come in the form of
consecutive X, Y, and Z coordinates. It is preferred that VERTICES be
a (SIMPLE-ARRAY SINGLE-FLOAT (*)) or (SIMPLE-ARRAY DOUBLE-FLOAT (*)).

If VERTICES only contains 2 or fewer vertices, an error of type
TOO-FEW-POINTS-ERROR is signalled. If VERTICES describes a point cloud
that doesn't have any volume, an error of type
POINTS-NOT-DISTINCT-ERROR, POINTS-COLINEAR-ERROR or
POINTS-IN-PLANE-ERROR is signalled. Signaling of latter error depends
on the EXTRUDE-IF-FLAT parameter (see below).

Returns two values:
  1. A simple array of vertices that lie on the convex hull, in the
     same format as the input, including matching element-type.
  2. A (SIMPLE-ARRAY (UNSIGNED-BYTE 32)) of vertex indices, every
     triplet of which describes a triangle face on the hull. The
     vertex index must be multiplied by 3 in order to get the index of
     the first vertex in the returned vertices array.

If a convex hull cannot be constructed exactly due to floating point
imprecision, a warning of type EDGE-SOLVER-FAILED may be signalled. If
EDGE-SOLVER-FAILED is signalled, the resulting convex hull may not be
optimal.

You may pass an optional EPS parameter (defaulting to 0.0001), which
defines the minimal distance below which two vertices are considered
the same and are merged together.

You may pass an optional REDUCE-VERTICES parameter (defaulting to T),
which if true causes the function to return a fresh array of vertices,
only including those used in the convex hull. If NIL, the same vertex
array that was passed in *may* be returned. A fresh array is
nevertheless returned if the input vertices all lie in a single plane,
thus requiring an extra vertex to form a hull with volume.

You may pass an optional EXTRUDE-IF-FLAT parameter (defaulting to T),
which controls whether a flat point cloud (all points lie in a
2-dimensional plane) is extruded by adding a point to form a proper
volume. If this parameter is NIL and VERTICES describe a flat point
cloud, an error of type POINTS-IN-PLANE-ERROR is signaled.

See TOO-FEW-POINTS-ERROR
See POINTS-NOT-DISTINCT-ERROR
See POINTS-COLINEAR-ERROR
See POINTS-IN-PLANE-ERROR
See EDGE-SOLVER-FAILED"))
