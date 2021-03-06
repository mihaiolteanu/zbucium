(defpackage :zbucium
  (:use :cl :plump :lquery :alexandria :generators
        :bt :lastfm :youtube :lyrics)
  (:import-from :drakma :http-request)
  (:import-from :bordeaux-threads :make-thread)
  (:import-from :bordeaux-threads :join-thread)
  (:shadowing-import-from :yason :parse)
  (:import-from :local-time :timestamp-to-unix)
  (:import-from :local-time :now)
  (:export ;; Functionality implemented by this library
           play-song
           play-artist
           play-album
           play-tag
           play-user-songs
           play-my-loved-songs
           play-artist-similar
           play-tag-similar
           what-is-playing
           what-is-playing-as-string
           song-lyrics
           love-song
           unlove-song
           next-song
           stop

           ;; Reexported from lyrics
           search-song

           ;; Reexported from youtube
           pause
           play/pause
           replay
           seek
           percent-pos
           time-pos
           duration
           switch-to-browser
           turn-video-on
           ))
