;;
;; mikmod.lisp
;;
;; A higher level interface to mikmod than cl-cffi-mikmod.
;;
;; License: BSD
;;
;; Burton Samograd
;; burton.samograd@gmail.com
;; 2015

(eval-when (:compile-toplevel :load-toplevel)
  (require 'cffi)
  (require 'cl-cffi-mikmod)
  (require 'bordeaux-threads))

(defpackage :mikmod
  (:use :common-lisp :cffi :cl-cffi-mikmod :bordeaux-threads)
  (:export "INIT" "SHUTDOWN"
	   "LOAD-SAMPLE" "PLAY-SAMPLE" "STOP-SAMPLE"
	   "LOAD-MODULE" "PLAY-MODULE" "STOP-MODULE"))
(in-package :mikmod)

(defvar *drivers-registered* nil)
(defvar *update-thread* nil)
(defparameter *update-delay* 0.01 "Time to sleep between calls
to mikmod-update. Try decreasing if you encounter skipping during
playback..")
(defvar *sample-table* (make-hash-table :test #'equal))
(defvar *module-table* (make-hash-table :test #'equal))

(defun init (&optional (num-voices 32))
  "Initialize cl-cffi-mikmod and start the driver thread."

  (unless *drivers-registered*
    (cl-cffi-mikmod::mikmod-registeralldrivers)
    (cl-cffi-mikmod::mikmod-registerallloaders)
    (setf *drivers-registered* t))

  (assert (= 1 (cl-cffi-mikmod::mikmod-initthreads)))

  (setf cl-cffi-mikmod::*md-mode* (logior cl-cffi-mikmod::*md-mode* cl-cffi-mikmod::dmode-soft-sndfx))
  (setf cl-cffi-mikmod::*md-mode* (logior cl-cffi-mikmod::*md-mode* cl-cffi-mikmod::dmode-soft-music))
  (setf cl-cffi-mikmod::*md-mode* (logior cl-cffi-mikmod::*md-mode* cl-cffi-mikmod::dmode-16bits))
  (cl-cffi-mikmod::mikmod-init "")

  (cl-cffi-mikmod::mikmod-setnumvoices -1 num-voices)
  (cl-cffi-mikmod::mikmod-enableoutput)

  (unless *update-thread*
    (setf *update-thread* 
	  (bordeaux-threads:make-thread (lambda () (loop
						      (cl-cffi-mikmod::mikmod-update)
						      (sleep *update-delay*)))
					:name "mikmod:*update-thread*"))))

(defun shutdown ()
  "Stop the mikmod driver thread."
  (when *update-thread*
    (bordeaux-threads:destroy-thread *update-thread*)
    (setf *update-thread* nil)))

(defun load-sample (filename)
  (cl-cffi-mikmod::sample-load filename))

(defun play-sample (sample &key (volume 255) (pan 128) (freq 44100) #|(freq-scale 1)|#)
  "Loads and plays sample. Returns the voice-id of the played sample."

  (assert *update-thread* nil "Call mikmod:init before trying to play samples.")

  (let ((voice-id (cl-cffi-mikmod::sample-play sample 0 0)))
    (cl-cffi-mikmod::voice-setvolume voice-id volume)
    (cl-cffi-mikmod::voice-setpanning voice-id pan)
    (cl-cffi-mikmod::voice-setfrequency voice-id freq)
    #| I'd like this to work but the sample cstruct doesn't seem to give correct values yet.
    (unless (= freq-scale 1)
    (cl-cffi-mikmod::voice-setfrequency voice-id
    (cffi:with-foreign-slots ((speed)
    sample cl-cffi-mikmod::sample)
    (* speed freq-scale))))
    |#
    (values voice-id)))

(defun stop-sample (voice-id)
  (cl-cffi-mikmod::voice-stop voice-id))

(defun free-sample (sample)
  (cl-cffi-mikmod::sample-free sample))

(defun load-module (filename)
  (cl-cffi-mikmod::player-load filename 64 0))

(defun play-module (module &key (restart t))
  "Loads and plays a module."

  (assert *update-thread* nil "Call mikmod:init before trying to play modules.")
  (when restart
    (cl-cffi-mikmod::player-setposition 0))
  (cl-cffi-mikmod::player-start module))

(defun stop-module ()
  (cl-cffi-mikmod::player-stop))

(defun free-module (module)
  (cl-cffi-mikmod::player-free module))
