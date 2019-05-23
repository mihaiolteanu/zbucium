(ql:quickload :dexador)
(ql:quickload :plump)
(ql:quickload :lquery)

(defmacro credentials (&rest entries)
  (dolist (entry entries)
    (ccase (first entry)
      (api-key (defparameter *api-key* (second entry)))
      (shared-secret (defparameter *shared-secret* (second entry))) 
      (username (defparameter *username* (second entry))))))

(load #P"~/.config/.lastfm.lisp")

(defparameter *base-url*
  (format nil "http://ws.audioscrobbler.com/2.0/?api_key=~a" *api-key*))

(defparameter *methods*
  '((:artist.getinfo       (artist) "bio summary")
    (:artist.getsimilar    (artist limit) "artist name")
    (:artist.gettoptags    (artist))
    (:artist.gettopalbums  (artist limit) "album name")
    (:artist.gettoptracks  (artist limit) "track > name")
    (:artist.search        (artist limit) "artist name")
    (:album.getinfo        (artist album))
    (:tag.gettoptracks     (tag limit))
    (:tag.gettopartists    (tag limit) "artist name")))

(defun method-name (method)
  (first method))

(defun options (method)
  (second method))

(defun query-string (method)
  (third method))

(defun append-method (url method)
  (concatenate 'string url
               "&method="
               (symbol-name (method-name method))))

(defun append-options (url list)
  "Add each option in the given list to the last.fm query "
  (concatenate 'string url (forma:t nil "~{&~a=~~a~}" list)))

(defun fill-options (url options)
  (apply #'format `(nil ,url ,@options)))

(defun build-request-url (method options)
  (fill-options (append-options (append-method *base-url* method)
                                (options method))
                options))

(defun lastfm-get (what &rest options)
  (let ((method (find what *methods* :key #'first)))
    (when method
      (let* ((html (dex:get (build-request-url method options)))
             (plump:*tag-dispatchers* plump:*xml-tags*)
             (parsed (plump:parse html))
             (query (query-string method)))
        (lquery:$ parsed query (text))))))

(lastfm-get :tag.gettopartists "doom+metal" 5)
(lastfm-get :artist.getinfo "anathema")
(lastfm-get :artist.gettopalbums "anathema" 10)
(lastfm-get :artist.getsimilar "anathema" "")
(lastfm-get :artist.gettoptracks "anathema" 10)
(lastfm-get :artist.search "void of" 10)

