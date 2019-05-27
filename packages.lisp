(defpackage :muse-player
  (:use :cl :plump :lquery :alexandria :lastfm)
  (:import-from :drakma :http-request)
  (:shadowing-import-from :yason :parse))


