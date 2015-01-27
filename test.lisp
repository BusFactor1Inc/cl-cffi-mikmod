(require 'cl-cffi-mikmod)
(in-package :cl-cffi-mikmod)


;; a basic example of sound playing taken from the mikmod docs
(defun test ()
  (mikmod-registeralldrivers)
  (setf *md-mode* (logior *md-mode* dmode-soft-sndfx))
  (setf *md-mode* (logior *md-mode* dmode-16bits))
  (mikmod-init "")
  (let ((sample (sample-load "test.wav")))
    (mikmod-setnumvoices -1 2) ; not sure what the -1 is for
    (mikmod-enableoutput)

    (let ((voice-id (sample-play sample 0 0)))
      (loop
	 (mikmod-update)
	 (when (= 1 (voice-stopped voice-id)) (return))
	 (sleep .01)))
    (sample-free sample)
    (mikmod-exit)))

(defun player-test ()
  (mikmod-registeralldrivers)
  (mikmod-registerallloaders)
  (setf *md-mode* (logior *md-mode* dmode-soft-music))
  (mikmod-init "")
  (let ((module (player-load "Untitled.xm" 64 0)))
    (player-start module)
    (loop (print (player-active) (mikmod-update)) (sleep 0.01))))
    
  
