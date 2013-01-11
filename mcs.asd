;;;; mcs.asd -*- Mode: Lisp;-*- 

(cl:in-package :asdf)

(defsystem :mcs
  :serial t
  :depends-on (:fiveam)
  :components ((:file "mcs-pkg")
               (:file "macros")
               (:file "low-it")
               (:file "low")
               (:file "globals")
               (:file "slot-val")
               (:file "cl-core")
               (:file "cl-boot")
               (:file "gfn-core")
               (:file "gfn-look")
               (:file "gfn-boot")
               (:file "access-m")
               (:file "class-m")
               (:file "system-m")
               (:file "util")
               ;;    (gfn-comp             :depends-on (gfn-look) )
               #+(and :ccl (not :ccl-2)) (mcsmenus (macros globals gfn-core gfn-boot))
               ;;    #-(or :EXCL
               ;;          :TI) (optimize (system-m gfn-comp class-m))
               (:file "redefine")
               (:file "patches")))


(defmethod perform ((o test-op) (c (eql (find-system :mcs))))
  (load-system :mcs)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :mcs.internal :mcs))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

;;; eof
