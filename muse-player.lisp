(in-package :muse-player)

(defparameter *mpvsocket* "/tmp/mpvsocket")
(defparameter *mpv-command*
  (concatenate 'string "mpv --input-ipc-server=" *mpvsocket* " ~a"))

(defun mpv-play (artist track)
  (let ((youtube-url (get-url artist track)))
    (when youtube-url
    (uiop:run-program
     (format nil *mpv-command* youtube-url)))))

(defun get-url (artist song)
  "Since there is no youtube link available through the last.fm API,
try and get it from the last.fm song's page."
  (let* ((url (format nil "https://www.last.fm/music/~a/_/~a"
                      (substitute #\+ #\Space artist)
                      (substitute #\+ #\Space song)))
         (request (http-request url))
         (links ($ (inline (plump:parse request))
                  "[data-playlink-affiliate]" (attr "data-youtube-url") )))
    (if (> (length links) 0)
        ;; Two identical links are available on the page.
        (aref links 0)
        ;; This song has no youtube link information.
        nil)))

(fmemo:memoize 'get-url)

(defun play-artist-toptracks (artist &key (limit "20") (random nil) (oneshot nil))
  (play (mapcar (lambda (track)
                  (list artist track))
                (lastfm-get :artist.gettoptracks artist limit))
        (lambda (artist+track)
          (mpv-play artist (second artist+track)))
        random
        :oneshot oneshot))

(defun play-artist-similar (artist &key (limit "3"))
  (play (lastfm-get :artist.getsimilar artist limit)
        (lambda (artist)
          (play-artist-toptracks artist
                                 :limit limit
                                 :random t
                                 :oneshot t))
        t))

(defun play-tag-artists (tag &optional (limit "3"))
  (play (lastfm-get :tag.gettopartists tag limit)
        (lambda (artist)
          (play-artist-toptracks artist
                                 :limit limit
                                 :random t
                                 :oneshot t))
        t))

(defun play-user-lovedtracks (user &optional (limit "3") (random t))
  (play (lastfm-get :user.getlovedtracks user limit)
        (lambda (artist+song)
          (mpv-play (first artist+song)
                    (second artist+song)))
        random))

(let ((keep-playing nil)
      (playing-item nil))
  (defun play (playlist fn random &key (oneshot nil))
    (setf keep-playing t)
    (loop for play-item in (if random
                               (shuffle playlist)
                               playlist)
          when keep-playing
            do (progn (setf playing-item play-item)
                      (funcall fn play-item))
          when oneshot
            do (return)))

  (defun what-is-playing ()
    playing-item)

  (defun stop-playing ()
    (setf keep-playing nil)
    (setf playing-item nil)))

(defun mpv-command (&rest args)
  (parse
   (with-output-to-string (out)
     (uiop:run-program
      (format nil "echo '{\"command\": [~{\"~a\"~^, ~}]}' | socat - ~A"
              args *mpvsocket*)
      :output out))))

(defun set-mpv-property (property value)
  (mpv-command "set_property" property value))

(defun get-mpv-property (property)
  (gethash "data" (mpv-command "get_property" property)))

(defun toggle-play ()
  "Toggle playing status"
  (mpv-command "cycle" "pause"))

(defun pause ()
  "Make sure the player is paused"
  (unless (get-mpv-property "pause")
    (toggle-play)))

(defun quit-mpv ()
  (mpv-command "quit" 0))

(defun stop-player ()
  (stop-playing)
  (quit-mpv))
