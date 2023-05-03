#|
 This file is a part of quickhull
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem quickhull
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An implementation of the Quickhull convex hull construction algorithm"
  :homepage "https://shirakumo.github.io/quickhull/"
  :bug-tracker "https://github.com/shirakumo/quickhull/issues"
  :source-control (:git "https://github.com/shirakumo/quickhull.git")
  :serial T
  :components ((:file "quickhull"))
  :depends-on (:3d-vectors
               :documentation-utils))
