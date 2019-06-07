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
  
  (defun play-simple (songs-generator play-item-fn)
    (start-playing)
    (loop for song = (next songs-generator)
            then (next songs-generator)
          while still-playing
          do (let ((artist-and-song (funcall play-item-fn song)))
               (set-playing-song artist-and-song)
               (youtube:play
                (concatenate 'string (first artist-and-song)
                             " " (second artist-and-song))))))

  (defun play-artist (artist nsongs random)
    (play-simple (artist-songs artist nsongs random)
                 (lambda (song)
                   (list artist song))))

  (defun play-tag (tag nsongs random)
    (play-simple (tag-songs tag nsongs random)
                 (lambda (song)
                   (list (first song) (second song)))))

  (defun play-artist-similar-artists (artist nartists nsongs)
    (play-simple (artist-similar-artists-songs artist nartists nsongs)
                 (lambda (song)
                   (list (first song) (second song))))))

(defun replay () (youtube:replay))
(defun switch-to-browser () (youtube:switch-to-browser))
(defun skip-song () (youtube:quit))
(defun play/pause () (youtube:play/pause))
(defun seek (seconds) (youtube:seek seconds))

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
