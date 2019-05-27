(asdf:defsystem :muse-player
  :description "Music player"
  :author "Mihai Olteanu"
  :license "GPLv3"
  :version "0.1"
  :depends-on (:lastfm
               :drakma
               :plump
               :lquery
               :alexandria
               :fare-memoization
               :yason)
  :serial t
  :components ((:file "packages") 
               (:file "muse-player"))
  :in-order-to ((test-op (test-op "muse-player/tests"))))
