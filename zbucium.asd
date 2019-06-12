(asdf:defsystem :zbucium
  :description "last.fm music player with lyrics"
  :author "Mihai Olteanu"
  :license "GPLv3"
  :version "0.1"
  :depends-on (:lastfm
               :youtube
               :lyrics
               :drakma
               :bordeaux-threads
               :plump
               :lquery
               :alexandria
               :local-time
               :fare-memoization
               :generators
               :yason)
  :serial t
  :components ((:file "packages")
               (:file "zbucium")))
