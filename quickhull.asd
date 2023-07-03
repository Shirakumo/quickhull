(asdf:defsystem quickhull
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "An implementation of the Quickhull convex hull construction algorithm"
  :homepage "https://shirakumo.github.io/quickhull/"
  :bug-tracker "https://github.com/shirakumo/quickhull/issues"
  :source-control (:git "https://github.com/shirakumo/quickhull.git")
  :serial T
  :components ((:file "package")
               (:file "support")
               (:file "quickhull"))
  :depends-on (:3d-vectors
               :documentation-utils))
