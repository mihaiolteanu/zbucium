(defpackage :mpv
  (:use :cl)
  (:import-from :uiop :run-program)
  (:export mpv-play
           play/pause
           replay-song
           forward-song
           quit-mpv))

(defpackage :muse-player
  (:use :cl :plump :lquery :alexandria :bt :lastfm)
  (:import-from :drakma :http-request)
  (:import-from :bordeaux-threads :make-thread)
  (:import-from :bordeaux-threads :join-thread)
  (:shadowing-import-from :yason :parse)
  (:export play-artist-toptracks
           play-artist-similar
           play-tag-artists
           play-user-lovedtracks
           next-song
           what-is-playing
           replay-song
           forward-song
           play/pause
           stop-player))
