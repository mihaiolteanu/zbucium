(ql:quickload :drakma)
(ql:quickload :plump)
(ql:quickload :lquery)

(defpackage :lastfm
  (:use :cl :drakma :plump :lquery))

(in-package :lastfm)

(defmacro credentials (&rest entries)
  (dolist (entry entries)
    (ccase (first entry)
      (api-key       (defparameter *api-key* (second entry)))
      (shared-secret (defparameter *shared-secret* (second entry))) 
      (username      (defparameter *username* (second entry))))))

(load #P"~/.config/.lastfm.lisp")

(defparameter *base-url* "http://ws.audioscrobbler.com/2.0/")
(defparameter *services*  
  '((:artist.getinfo      (artist)       "bio summary")
    (:artist.getsimilar   (artist limit) "artist name")
    (:artist.gettoptags   (artist)       "tag name")
    (:artist.gettopalbums (artist limit) "album name")
    (:artist.gettoptracks (artist limit) "track > name")
    (:artist.search       (artist limit) "artist name")
    (:album.getinfo       (artist album) "track > name")
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
              (map 'vector (lambda (p1 p2)
                             (concatenate 'string p1 " - " p2))
                   (subseq result 0 (/ len 2))
                   (subseq result (/ len 2) len)))
            result)
        ))))

