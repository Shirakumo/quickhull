(asdf:defsystem quickhull
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "An implementation of the Quickhull convex hull construction algorithm"
  :homepage "https://shirakumo.org/docs/quickhull/"
  :bug-tracker "https://shirakumo.org/project/quickhull/issues"
  :source-control (:git "https://shirakumo.org/project/quickhull.git")
  :serial T
  :components ((:file "package")
               (:file "support")
               (:file "quickhull")
               (:file "documentation"))
  :depends-on (:3d-math
               :documentation-utils))
