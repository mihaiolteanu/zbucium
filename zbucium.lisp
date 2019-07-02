(in-package :zbucium)

(defparameter *still-playing* nil
  "Set to true when player started, set to nil when stopped.")

(defparameter *playing-thread* nil
  "Playing a song or list of songs starts a new thread since we also want to
  return from the call while the player is running. ")

(defparameter *artist* nil
  "The name of the currently playing artist.")

(defparameter *song* nil
  "The name of the currently playing song.")

(defconstant +scrobble-timeout+ 60
  "Time to wait, in seconds, before scrobbling the current playing song.")

(defun set-playing-song (artist-and-song)
    (setf *artist* (first artist-and-song))
    (setf *song* (second artist-and-song)))

(defun what-is-playing ()
  (list *artist* *song*))

(defun what-is-playing-as-string ()
  "Return a printable representation of the artist and song currently playing. If the player is stopped, return an empty string."
  (when *still-playing*
    (format nil "~a - ~a"
            (string-capitalize *artist*)
            (string-capitalize *song*))))

(defun start-playing ()
  (setf *still-playing* T))

(defun stop ()
  (when *still-playing*
    (setf *still-playing* nil)
    (youtube:quit)
    (join-thread *playing-thread*)
    (set-playing-song nil)))

(defun play-simple (songs-generator)
  "The main functionality. Take a new song from the generator, save the lyrics
for it, scrobble it if needed and either find a youtube url to play or create a
string that can be searched by youtube-dl. Repeat after the song finishes or was
stopped by the user. End the loop only when the player is stopped by the user."
  (stop)
  (start-playing)
  (setf *playing-thread*
        (make-thread
         (lambda ()
           (loop for artist-and-song = (next songs-generator)
                   then (next songs-generator)
                 ;; Make sure there is a way to stop this endless loop.
                 while *still-playing*
                 do (progn
                      (set-playing-song artist-and-song)
                      (let ((initial-artist *artist*) ;Setup for scrobbling
                            (initial-song *song*))
                        (make-thread
                         (lambda ()
                           ;; Save the lyrics for each song that is being played.
                           (lyrics *artist* *song*)
                           ;; If, after waking up, the same song is playing,
                           ;; scrobble it.
                           (sleep +scrobble-timeout+)
                           (when (and (string-equal initial-artist *artist*)
                                      (string-equal initial-song *song*))
                             (track-scrobble *artist* *song*
                                             (timestamp-to-unix (now)))))))
                      (youtube:play
                       (or (song-youtube-url *artist* *song*)
                           (concatenate 'string *artist* " " *song*)))
                      (set-playing-song nil)
                      ;; Wait for this song to end before playing the next one.
                      )))
         :name "zbucium playing thread")))

;; Exported interfaces. Most of these functions simply create lastfm generators
;; that are passed to play-simple.
(defun play-song (artist song)
  "Play and replay a single song"
  (play-simple (make-generator (:final-value nil)
                 (yield (list artist song)))))

(defun play-artist (artist nsongs random)
  (play-simple (artist-songs artist nsongs random)))

(defun play-album (artist album)
  (play-simple (album-songs artist album)))

(defun play-tag (tag nsongs random)
  (play-simple (tag-songs tag nsongs random)))

(defun play-user-songs (username nsongs random)
  (play-simple (user-songs username nsongs random)))

(defun play-my-loved-songs (nsongs random)
  (play-simple (my-loved-songs nsongs random)))

(defun play-artist-similar (artist nartists nsongs)
  (play-simple (artist-similar-artists-songs artist nartists nsongs)))

(defun play-tag-similar (tag nartists nsongs)
  (play-simple (tag-similar-artists-songs tag nartists nsongs)))

(defun song-lyrics ()
  (when *still-playing*
    (lyrics *artist* *song*)))

(defun love-song ()
  (when *still-playing*
    (track-love *artist* *song*)))

(defun unlove-song ()
  (when *still-playing*
    (track-unlove *artist* *song*)))

(defun next-song ()
  "Close the current youtube session, forcing the generator to take and play the
next song."
  (youtube:quit))
