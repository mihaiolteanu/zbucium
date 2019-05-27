(defpackage :mpv
  (:use :cl)
  (:import-from :uiop :run-program)
  (:export mpv-play
           quit-mpv))

(defpackage :muse-player
  (:use :cl :plump :lquery :alexandria :lastfm :mpv)
  (:import-from :drakma :http-request)
  (:shadowing-import-from :yason :parse)
  (:export play-artist-toptracks
           play-artist-similar
           play-tag-artists
           play-user-lovedtracks
           stop-player))
