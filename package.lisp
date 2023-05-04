#|
 This file is a part of quickhull
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.quickhull
  (:use #:cl #:org.shirakumo.flare.vector)
  (:export
   #:edge-solver-failed
   #:convex-hull))

(in-package #:org.shirakumo.fraf.quickhull)
