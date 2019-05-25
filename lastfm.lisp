(ql:quickload :drakma)
(ql:quickload :plump)
(ql:quickload :lquery)
(ql:quickload :alexandria)
(ql:quickload :fare-memoization)
(ql:quickload :bt-semaphore)
(ql:quickload :yason)

(defpackage :lastfm
  (:use :cl :drakma :plump :lquery
        :alexandria :bt)
  (:import-from :yason :parse))

(in-package :lastfm)

(defun config (&key api-key shared-secret username mpvsocket)
  (defparameter *api-key* api-key)
  (defparameter *shared-secret* shared-secret)
  (defparameter *username* username)
  (defparameter *mpvsocket* mpvsocket))

(load #P"~/.config/.lastfm.lisp")

(defparameter *base-url* "http://ws.audioscrobbler.com/2.0/")
(defparameter *services*  
  '((:artist.getinfo      (artist)       "bio summary")
    (:artist.getsimilar   (artist limit) "artist name")
    (:artist.gettoptags   (artist)       "tag name")
    (:artist.gettopalbums (artist limit) "album > name")
    (:artist.gettoptracks (artist limit) "track > name")
    (:artist.search       (artist limit) "artist name")
    (:album.getinfo       (artist album) "track > name")
    (:tag.getinfo         (tag)          "summary")
    (:tag.gettoptracks    (tag limit)    "artist > name, track > name")
    (:tag.gettopartists   (tag limit)    "artist name")
    (:user.getlovedtracks (user limit)   "artist > name, track > name"))

  "List of all the Web Services supported by the Last.Fm API (see
  https://www.last.fm/api):
- The first field of each service denotes the API method.
- The second field is a list of all the parameters supported by this method.
- The last field is a string used to extract the relevant information
  from the xml response received from last.fm for this method.")

(defun service-method (service) (first service))
(defun parameters (service) (second service))
(defun query-string (service) (third service))
(defun multi-query-p (service) (find #\, (query-string service)))

(defun request-url (service param-values)
  "Build and request a last.fm service"
  (http-request *base-url*
     :parameters
     `(("api_key" . ,*api-key*)
       ("method" .  ,(symbol-name (service-method service)))
       ;; Build alists by matching up the service's
       ;; parameters with the user supplied param-values.
       ,@(mapcar (lambda (m v)
                   (cons (sb-unicode:lowercase (symbol-name m)) v))
                 (parameters service)
                 param-values))))

(defun lastfm-get (what &rest param-values)
  (let ((service (find what *services* :key #'first)))
    (when service
      (let* ((request (request-url service param-values))
             ;; Tell plump to parse the request as an xml
             (*tag-dispatchers* *xml-tags*)
             (query (query-string service))
             (result ($ (inline (parse request))
                       query (text))))
        ;; For top tracks, for example, the result vector contains
        ;; the artist name in its first half and the song name in its second
        (if (multi-query-p service)
            (let ((len (length result)))
              (map 'list (lambda (p1 p2)
                           (list p1 p2))
                   (subseq result 0 (/ len 2))
                   (subseq result (/ len 2) len)))
            (map 'list #'identity result))))))

(defparameter *mpv-command*
  (concatenate 'string "mpv --input-ipc-server=" *mpvsocket* " ~a"))
(defparameter *playing* nil)

(defun mpv-play (youtube-url)
  (when youtube-url
    (setf *playing-thread*
        (uiop:run-program
         (format nil *mpv-string* youtube-url)))))

(defun get-url (artist song)
  "Since there is no youtube link available through the last.fm API,
try and get it from the last.fm song's page."
  (let* ((url (format nil "https://www.last.fm/music/~a/_/~a"
                      (substitute #\+ #\Space artist)
                      (substitute #\+ #\Space song)))
         (request (http-request url))
         (links ($ (inline (parse request))
                  "[data-playlink-affiliate]" (attr "data-youtube-url") )))
    (if (> (length links) 0)
        ;; Two identical links are available on the page.
        (aref links 0)
        ;; This song has no youtube link information.
        nil)))

(defun play-artist-toptracks (artist &optional (ntracks "20") (nplays 20) (random nil))
  "Play the first 20 tracks for the given artist a maximum of 20 times.
For nplays=1, this means play a random track from this artists 20 best tracks."
  (setf *playing* t)
  (let ((tracks (lastfm-get :artist.gettoptracks artist ntracks)))
    (loop for track in (if random
                           (shuffle tracks)
                           tracks)
          for plays from 1 upto nplays
          when *playing*
            do (mpv-play (get-url artist track)))))

(defun play-artist-similar (artist &optional (limit "3"))
  (let ((artists (lastfm-get :artist.getsimilar artist limit)))
    (dolist (artist (shuffle artists))
      (play-artist-toptracks artist "20" 1 t))))

(defun play-tag-artists (tag &optional (nartists "3") (nplays 20))
  (setf *playing* t)
  (let* ((artists (lastfm-get :tag.gettopartists tag nartists))
         (len (parse-integer nartists)))
    (loop for artist = (nth (random len) artists)
            then (nth (random len) artists)
          for plays from 0 upto nplays
          when *playing*
            do (play-artist-toptracks artist "20" 1 t))))

(defun play-user-lovedtracks (user &optional (nsongs "3") (random t))
  (setf *playing* t)
  (let ((tracks (lastfm-get :user.getlovedtracks user nsongs)))
    (loop for track in (if random
                           (shuffle tracks)
                           tracks)
          when *playing*
            do (mpv-play (get-url (first track) (second track))))))

(fmemo:memoize 'get-url)
(fmemo:memoize 'lastfm-get)


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
  (setf *playing* nil)
  (quit-mpv))

