;;; This is a partial implementation as needed by MCS!

(in-package "USER")

(defmacro defpackage (name &rest options)
  (let ((size (cdr (assoc  ':nicknames options :test #'eq)))
        (use (cdr (assoc  ':use options :test #'eq)))
        (nicknames (cdr (assoc  ':nicknames options :test #'eq)))
        (imports (cddr (assoc  ':import-from options :test #'eq)))
        (exports (cdr (assoc  ':export options :test #'eq))))
    `(eval-when (compile eval load)
       (unless (find-package "MCS")
         (make-package "MCS" 
                       :nicknames ',nicknames
                       :use '()))
       (import (mapcar #'(lambda (str)
                             (find-symbol str (find-package "LISP")))
                         ',imports)
               (find-package ',name))
       (export (mapcar #'(lambda (str)
                               (intern str (find-package "MCS")))
                         ',exports)
               (find-package "MCS")))))

;(export 'lisp::defpackage (find-package "LISP"))