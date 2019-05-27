(in-package :mpv)

(defun mpv-config (&key (socket "/tmp/lastfm-player-socket"))
  (defparameter *socket* socket))

(load #P"~/.config/.mpv.lisp")

(defun mpv-play (path)
  "Youtube or local path to audio/video file."
  (when path
    (run-program
     (format nil "mpv --input-ipc-server=~a ~a"
             *socket* path))))

(defun mpv-command (&rest args)
  (yason:parse
   (with-output-to-string (out)
     (run-program
      (format nil "echo '{\"command\": [~{\"~a\"~^, ~}]}' | socat - ~A"
              args *socket*)
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
