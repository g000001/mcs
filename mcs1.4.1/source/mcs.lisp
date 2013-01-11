;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: "USER" -*-

;;;           Copyright  1990    BY
;;;           G M D  
;;;           Postfach 1240
;;;           D-5205 St. Augustin
;;;           FRG

;;; -----------------------------------------------------------------------------------
;;;
;;; description:    Start File of the Meta Class System MCS
;;;
;;; notes:          Version 1.3 (13.05.91)
;;;
;;; contact:        Juergen Kopp, Harry Bretthauer
;;;
;;; history:
;;;          date:      author:             comments:
;;;          27.04.90   Harry Bretthauer    initial version
;;;          22.05.90   Harry Bretthauer    Version 0.9
;;;          17.06.90   Harry Bretthauer    Version 0.91
;;;          10.07.90   Harry Bretthauer    Version 0.92
;;;          20.07.90   Juergen Kopp        Version 0.93
;;;                                         file laod-fns removed

;;;          18.10.90   Harry Bretthauer    Version 1.0
;;;          22.11.90   Juergen Kopp        exports
;;;          29.01.91   Harry Bretthauer    Version 1.1
;;;          01.02.91   Harry Bretthauer    Version 1.2 working
;;;                                         shadow for CLOS
;;;                                         shadow and define FUNCTION for :PROCYON
;;;          13.05.91   Juergen Kopp        Version 1.3 (docu)
;;;          14.05.91   kopp, bretth        *mcs-source-path-name* for :TI added
;;;                                         #-:TI added for file optimize
;;;          18.06.91                       version 1.3.1
;;;          09.07.91   kopp                patches for KCL
;;;          22.01.92   Harry Bretthauer    Version 1.3.2 
;;;                                         update for MACL 2.b, provide removed
;;;          26.04.92   Harry Bretthauer    Version 1.4 
;;;          17.09.92   Harry Bretthauer    update for AKCL 
;;;          16.11.92   Harry Bretthauer    Version 1.4.1 
;;;                                         bug in sorting multimethods fixed

;;; -----------------------------------------------------------------------------------

(eval-when (compile eval load) ;(:compile-toplevel :load-toplevel :execute)
  (when (fboundp 'user::defpackage)  ; is a Steele-II Lisp
    (if (find-package "COMMON-LISP")
      (unless (member "LISP" (package-nicknames (find-package "COMMON-LISP")) 
                      :test #'equal)
        (defpackage "COMMON-LISP"
          (:nicknames "CL" "LISP")))
      (unless (member "COMMON-LISP" (package-nicknames (find-package "LISP")) 
                      :test #'equal)
        ;(defpackage "LISP" (:nicknames "CL" "COMMON-LISP" ))
        ))
    (if (find-package "COMMON-LISP-USER")
      (unless (member "USER" (package-nicknames (find-package "COMMON-LISP-USER")) 
                      :test #'equal)
        (defpackage "COMMON-LISP-USER"
          (:nicknames "USER" "CL-USER")))
      (unless (member "COMMON-LISP-USER" (package-nicknames (find-package "USER")) 
                      :test #'equal)
        ;(defpackage "USER" (:nicknames "COMMON-LISP-USER" "CL-USER"))
        ))))
    

(in-package "USER")



;;; MCS Version Number:

(defconstant *mcs-version* "MCS 1.4.1 of 16.11.92")


;;; ***********************************************************************************
;;; The following four global variables have to be adapted by the user !!!!


;;; Define source and binary path-name, source-file and compiled-file extension.

(defvar *mcs-source-path-name*		;; where to find the sources of mcs
  #+(and :CCL (not :CCL-2)) (pathname-directory ccl::*loading-file-source-file*)
  #+:CCL-2 (subseq (namestring ccl::*loading-file-source-file*)
               0
               (- (length (namestring ccl::*loading-file-source-file*)) 8))
  #+:EXCL (subseq (namestring user::*source-pathname*)
               0
               (- (length (namestring user::*source-pathname*)) 8))
  #+:LUCID (subseq (namestring user::*source-pathname*)
               0
               (- (length (namestring user::*source-pathname*)) 8))
  #+:TI (subseq (namestring sys:fdefine-file-pathname)
                0
                (- (length (namestring sys:fdefine-file-pathname)) 3))
  #+:PROCYON ":mcs:source:"
  #+:AKCL (subseq (namestring system::*load-pathname*)
	        0
	        (- (length (namestring system::*load-pathname*)) 8))
  #+:CMU (subseq (namestring system::*load-pathname*)
	        0
	        (- (length (namestring system::*load-pathname*)) 8))
  #+(and CLISP UNIX) "xxx/source/"
  #+(and CLISP ATARI) "yyy\\SOURCE\\"
  )

(defvar *mcs-binary-path-name*		;; where to find/put the binaries of mcs
      (concatenate 'string
                   (subseq *mcs-source-path-name*
                           0 (- (length *mcs-source-path-name*) 7))
                   "binary"
                   (subseq *mcs-source-path-name*
                           (- (length *mcs-source-path-name*) 1)
                           (length *mcs-source-path-name*))))

(defvar *source-file-extension*
  #+:CCL ".lisp"
  #+:EXCL ".lisp"
  #+:LUCID ".lisp"
  #+:TI ".lisp"
  #+:PROCYON ".lisp"
  #+:AKCL ".lisp"
  #+:CMU ".lisp"
  ;; The following extensions for CLISP should work. 
  ;; Otherwise rename this extension and mcs source file extensions to
  ;; ".lsp" or ".LSP", respectively.
  #+(and CLISP UNIX) ".lisp"        ; ".lsp"
  #+(and CLISP (not UNIX)) ".LISP"  ; ".LSP"
  )

(defvar *compiled-file-extension*
  #+:CCL ".fasl"
  #+:EXCL ".fasl"
  #+:LUCID ".sbin"
  #+:TI ".xld"
  #+:PROCYON ".fasl"
  #+:AKCL ".o"
  #+:CMU ".sparcf"
  #+(and CLISP UNIX) ".fas"
  #+(and CLISP (not UNIX)) ".FAS"
  )

;;; ***********************************************************************************

;;; Load defpackage hack for a Steele-I Lisp:

(eval-when (compile eval load) ;(:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'defpackage)
    (load (concatenate 'string
                       *mcs-source-path-name*
                       "defpkg.lisp"))))

;;; Load MCS Package Definition:

(eval-when (compile eval load) ;(:compile-toplevel :load-toplevel :execute)
  (unless (find-package "MCS")
    (load (concatenate 'string
                       *mcs-source-path-name*
                       "mcs-pkg.lisp"))))

;(import 'lisp:nil (find-package "MCS"))       ; this is a hack for EXCL 4.0

;;; Enter MCS Package:

(in-package "MCS")


;;; -----------------------------------------------------------------------------------
;;; Dependencies of System Files and Redefine Files
;;; -----------------------------------------------------------------------------------

(defvar *mcs-system-files-and-dependencies*
  ;  File               force recompilation
  '((macros               () )
    (low-it               (macros) )
    (low                  (low-it) )
    (globals              (macros low) )
    (slot-val             (macros low globals) )
    (cl-core              (macros low globals) )
    (cl-boot              (macros cl-core globals) )
    (gfn-core             (macros globals) )
    (gfn-look             (macros globals) )
    (gfn-boot             (macros globals cl-boot gfn-core) )
    (access-m             (macros globals gfn-core gfn-boot) )
    (class-m              (macros globals gfn-core gfn-boot) )
    (system-m             (macros globals gfn-core gfn-boot) )
    (util                 (macros globals gfn-core gfn-boot) )
;    (gfn-comp             (gfn-look) )
    #+(and :ccl (not :ccl-2)) (mcsmenus      (macros globals gfn-core gfn-boot))
;    #-(or :EXCL
;          :TI) (optimize             (system-m gfn-comp class-m))
    (redefine             (macros low gfn-boot) )
    (patches              (macros))
    ) )

;;; -----------------------------------------------------------------------------------
;;; Utilities for loading and compiling
;;; -----------------------------------------------------------------------------------

(defun source-file (file-name)
  (concatenate 'string 
               user::*mcs-source-path-name*
               (string-downcase (string file-name)) 
               user::*source-file-extension*))

(defun binary-file (file-name)
  (concatenate 'string 
               user::*mcs-binary-path-name*
               (string-downcase (string file-name)) 
               user::*compiled-file-extension*))


(defun load-compile-files (files-and-dependencies)
  (dolist (file-spec files-and-dependencies)
    (let ((lisp-file (source-file (first file-spec)))
          (bin-file  (binary-file (first file-spec))))
      (when (and 
             ;; lisp-file exists:
             (lisp:probe-file lisp-file)
             (or 
              ;; bin-file does not exist:
              (not (lisp:probe-file bin-file))
              ;; lisp-file newer than bin-file:
              (< (lisp:file-write-date bin-file)
                 (lisp:file-write-date lisp-file))
              ;; lisp-file depends on a newer bin-file
              (dolist (file (second file-spec) '())
                (if (< (lisp:file-write-date bin-file)
                       (lisp:file-write-date (binary-file file)))
                  (return t))) ))
        (compile-file lisp-file :output-file bin-file))
      (load bin-file))))

(defun load-lisp-files (files-and-dependencies)
  (dolist (file-spec files-and-dependencies)
    (let ((lisp-file (source-file (first file-spec))))
      (load lisp-file))))


;;; -----------------------------------------------------------------------------------
;;; Load MCS
;;; -----------------------------------------------------------------------------------

(load-compile-files *mcs-system-files-and-dependencies*)


;;; Make the module public:

;(provide 'mcs)
(pushnew :MCS *features*)


;;; eof


