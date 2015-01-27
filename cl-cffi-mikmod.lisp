;; -*- Mode: lisp -*-
;;
;; cl-cffi-mikmod.lisp
;;
;; A Common Lisp CFFI Interface to the MikMod library, a high level
;; MOD and sound player interface for a variety of platforms.
;;
;;    http://mikmod.sourceforge.net/
;;
;; Curently only defines enough of the API to play sound effects
;; (note: it only loads mono sample WAV files, stereo gives 'unknown
;; sample format' error). See test.lisp for an example.
;;
;; License: BSD
;;
;; Burton Samograd
;; burton.samograd@gmail.com
;; 2015

(eval-when (:compile-toplevel :load-toplevel)
  (require 'cffi))

(defpackage :cl-cffi-mikmod
  (:use :common-lisp :cffi))
   
(in-package :cl-cffi-mikmod)
   
(define-foreign-library libmikmod
    (:unix (:or "libmikmod.so.3" "libmikmod.so"))
    (t (:default "libmikmod")))
   
(use-foreign-library libmikmod)

(defcvar "md_volume" :uchar) ; 0-128
(defcvar "md_musicvolume" :uchar) ; 0-128
(defcvar "md_sndfxvolume" :uchar) ; 0-128
(defcvar "md_reverb" :uchar) ; 0-15
(defcvar "md_pansep" :uchar) ; 0-128

(defconstant dmode-16bits 1)
(defconstant dmode-stereo 2)
(defconstant dmode-soft-sndfx 4)
(defconstant dmode-soft-music 8)
(defconstant dmode-hqmixer 16)
(defconstant dmode-float 32)
(defconstant dmode-surround 256) ; 0x100
(defconstant dmode-interp 512)   ; 0x200
(defconstant dmode-reverse-stereo 1024) ; 0x400
(defconstant dmode-simd-mixer 2048) ; 0x800
(defconstant dmode-noisereduction 4096) ; 01000

(defcvar "md_mode" :ushort) ; set using or'd dmode-* values above
(defcvar "md_mixfreq" :ushort)
(defcvar "md_device" :ushort)

(defcvar "MikMod_errno" :int)

(defctype bool :int)

;; Not Sure if this works. Values like speed are coming out are as
;; garbage so I wouldn't trust this definition yet.
(defcstruct sample
  (panning :short)
  (speed :int) 
  (volume :uchar)
  (inflags :ushort)
  (flags :ushort)
  (length :uint)
  (loopstart :uint)
  (loopend :uint)
  (susbegin :uint)
  (susend :uint)
  (global-volume :uchar)
  (vibarato-flags :uchar)
  (vibarato-type :uchar)
  (vibarato-sweep :uchar)
  (vibarato-depth :uchar)
  (vibrate :uchar)
  (samplename :string)
  (avibpos :ushort)
  (divfactor :uchar)
  (seekpos :uint)
  (handle :short)
  (on-free :pointer)
  (ctx :pointer))
  
(defctype sample-pointer (:pointer (:struct sample)))
(defctype module-pointer :pointer)
(defctype voice-id :char)

(defcfun "MikMod_RegisterAllDrivers" :void) ; Calling this more than once will hang your Lisp, probably.
(defcfun "MikMod_Init" bool (config :string))
(defcfun "MikMod_InitThreads" bool)
(defcfun "MikMod_Exit" :void)
(defcfun "MikMod_Reset" bool (arg :string))
(defcfun "MikMod_SetNumVoices" bool (a :int) (num-voices :int)) ; FIXME: arg name
(defcfun "MikMod_Active" BOOL)
(defcfun "MikMod_EnableOutput" BOOL)
(defcfun "MikMod_DisableOutput" :void)
(defcfun "MikMod_Update" :void)
(defcfun "MikMod_strerror" :string (mikmod-errno :int))


(defcfun "Sample_Load" sample-pointer (filename :string))
(defcfun "Sample_Play" voice-id (sample sample-pointer) (start :uint) (flags :uchar))
(defcfun "Sample_Free" :void (sample sample-pointer))

(defcfun "Voice_SetVolume" :void (voice-id voice-id) (volume :ushort))
(defcfun "Voice_GetVolume" :ushort (voicee-id voice-id))
(defcfun "Voice_SetFrequency" :void (voice-id voice-id) (freq :uint))
(defcfun "Voice_GetFrequency" :uint (voice-id voice-id))
(defcfun "Voice_SetPanning" :void (voice-id voice-id) (freq :uint))
(defcfun "Voice_GetPanning" :uint (voice-id voice-id))
(defcfun "Voice_Play" :void (voice-id voice-id) (sample sample-pointer) (unknown :uint))
(defcfun "Voice_Stop" :void (voice-id voice-id))
(defcfun "Voice_Stopped" bool (voice-id voice-id))
(defcfun "Voice_GetPosition" :int (voice-id voice-id))
(defcfun "Voice_RealVolume" :uint (voice-id voice-id))

(defcfun "MikMod_RegisterAllLoaders" :void)
(defcfun "Player_Load" module-pointer (filename :string) (a :int) (b bool))
(defcfun "Player_Start" :void (module module-pointer))
(defcfun "Player_Stop" :void)
(defcfun "Player_Free" :void (module module-pointer))
(defcfun "Player_Active" bool)
(defcfun "Player_SetPosition" :void (pos :ushort))
(defcfun "Player_NextPosition" :void (pos :ushort))
(defcfun "Player_PrevPosition" :void (pos :ushort))
(defcfun "Player_SetSpeed" :void (pos :ushort))
(defcfun "Player_SetTempo" :void (pos :ushort))
(defcfun "Player_SetVolume" :void (pos :short))
(defcfun "Player_TogglePause" :void)
(defcfun "Player_Paused" bool)
(defcfun "Player_GetModule" module-pointer)

