(defpackage #:org.shirakumo.fraf.quickhull
  (:use #:cl #:org.shirakumo.fraf.math.vectors)
  (:export
   #:unsuitable-vertex-data
   #:too-few-points-error
   #:points-not-distinct-error
   #:points-colinear-error
   #:points-in-plane-error
   #:edge-solver-failed
   #:convex-hull))

(in-package #:org.shirakumo.fraf.quickhull)
