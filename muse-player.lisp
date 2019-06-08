(in-package :muse-player)

(defun play-song (name)
  (play name))

(let ((still-playing nil)
      (artist nil)
      (song nil))
  
  (defun set-playing-song (artist-and-song)
    (setf artist (first artist-and-song))
    (setf song (second artist-and-song)))

  (defun what-is-playing ()
    (list artist song))

  (defun start-playing ()
    (setf still-playing T))

  (defun stop ()
    (setf still-playing nil)
    (set-playing-song nil)
    (youtube:quit))
  
  (defun play-simple (songs-generator)
    (start-playing)
    (make-thread
     (lambda ()
       (loop for artist-and-song = (next songs-generator)
               then (next songs-generator)
             ;; Make sure there is a way to stop this endless loop.
             while still-playing
             do (progn
                  (set-playing-song artist-and-song)                  
                  (play-song artist song)
                  ;; Wait for this song to end before playing the next one.
                  )))))

  (defun play-song (artist song)
    "Play a single song, by either trying to get the youtube url from last.fm,
     or if that fails, call youtube-dl directly with a search string."
    (youtube:play
     (or (song-youtube-url artist song)
         (concatenate 'string artist " " song)))
    ;; Save the lyrics for each song that is being played.
    (make-thread (lambda ()
                   (lyrics artist song))))

  (defun play-artist (artist nsongs random)
    (play-simple (artist-songs artist nsongs random)))

  (defun play-tag (tag nsongs random)
    (play-simple (tag-songs tag nsongs random)))

  (defun play-user-songs (username nsongs random)
    (play-simple (user-songs username nsongs random)))

  (defun play-artist-similar-artists (artist nartists nsongs)
    (play-simple (artist-similar-artists-songs artist nartists nsongs)))

  (defun play-tag-similar-artists (tag nartsits nsongs)
    (play-simple (tag-similar-artists-songs tag nartists nsongs)))

  (defun song-lyrics ()
    (lyrics artist song))

  (defun love-song ()
    (track-love artist song))

  (defun unlove-song ()
    (track-unlove artist song)))

(defun next-song ()
  "Close the current youtube session, forcing the generator to take and play the
next song."
  (youtube:quit))

;; (youtube:replay)
;; (what-is-playing)
;; (youtube:switch-to-browser)
;; (youtube:quit)
;; (stop)
;; (play/pause)
;; (play-artist "anathema" 3 T)
;; (play-artist-similar-artists "anathema" "3" "3")
;; (play-tag "doom metal" "10" T)

;; (lastfm:random-artist-song "anathema" 20)
