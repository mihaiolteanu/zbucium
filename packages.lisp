(defpackage :muse-player
  (:use :cl :plump :lquery :alexandria :generators
        :bt :lastfm :youtube :lyrics)
  (:import-from :drakma :http-request)
  (:import-from :bordeaux-threads :make-thread)
  (:import-from :bordeaux-threads :join-thread)
  (:shadowing-import-from :yason :parse)
  (:import-from :local-time :timestamp-to-unix)
  (:import-from :local-time :now)
  (:export play-song
           play-artist
           play-artist-album
           play-tag
           play-user-songs
           play-artist-similar-artists
           play-tag-similar-artists

           song-lyrics
           search-song

           what-is-playing
           pause
           play/pause
           replay
           seek
           percent-pos
           time-pos
           duration
           switch-to-browser
           turn-video-on
           next-song
           stop

           love-song
           unlove-song
           ))
