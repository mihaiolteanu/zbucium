(in-package :muse-player)

(defun play-song (name)
  (play name))

(let ((still-playing nil)
      (playing-song nil))
  
  (defun set-playing-song (artist-and-song)
    (setf playing-song artist-and-song))

  (defun what-is-playing ()
    playing-song)

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
       (loop for song = (next songs-generator)
               then (next songs-generator)
             while still-playing
             do (progn
                  (set-playing-song song)
                  (youtube:play
                   (concatenate 'string (first song) " " (second song))))))))

  (defun play-artist (artist nsongs random)
    (play-simple (artist-songs artist nsongs random)))

  (defun play-tag (tag nsongs random)
    (play-simple (tag-songs tag nsongs random)))

  (defun play-artist-similar-artists (artist nartists nsongs)
    (play-simple (artist-similar-artists-songs artist nartists nsongs))))

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
