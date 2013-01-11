;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: user; Base: 10  -*-

;;;           Copyright 1990  BY
;;;           G M D  
;;;           Postfach 1240
;;;           D-5205 St. Augustin
;;;           FRG

;;;  AUTHOR:  Juergen Walther

;;;           exploring generic functions and classes in mcs and package exports
;;;           requires mcs + mcstools loaded before
;;;           uses following from mcs
;;;             mcs::obj-describe
;;;
;;;             mcs::class-name
;;;             mcs::class-slots
;;;             mcs::slot-definition-name
;;;             mcs::class-list             (mcstools)
;;;
;;;             mcs::generic-function-name
;;;             mcs::method-specializers
;;;             mcs::generic-function-methods
;;;             mcs::method-describe
;;;             mcs::gfn-list               (mcstools)

(in-package "USER")

(import '(mcs::method mcs::slot-definition))

;;; -----------------------------------------------------------------------------

(defobject *hiding-dialog* *dialog*)

(defobfun (window-close *hiding-dialog*) (&optional close)
  (if close (usual-window-close)
      (window-hide)))

;;; -----------------------------------------------------------------------------

(defobject *object-sequence-dialog* *sequence-dialog-item*)

(defobfun (set-table-sequence *object-sequence-dialog*) (sequence)
  (dolist (cell (selected-cells))                     ; deselect selected cells
    (cell-deselect (point-h cell) (point-v cell)))
  (usual-set-table-sequence sequence))                ; usual set-table-sequence

(defobfun (selection *object-sequence-dialog*) ()     ; get the selected object
  (cell-contents (car (selected-cells))))

;;; -----------------------------------------------------------------------------

(defobject *pkg-editable-text-dialog-item* *editable-text-dialog-item*)

(defobfun (exit-editable-text *pkg-editable-text-dialog-item*) (new-text-item)
  (declare (ignore new-text-item))
  (if (find-package (string-upcase (dialog-item-text))) t
      (progn (ed-beep) nil)))

;;; -----------------------------------------------------------------------------

(defobject *string-sequence-dialog* *sequence-dialog-item*)

(defun split-lines (string)
  (with-input-from-string (stream string)
    (do ((line (read-line stream nil nil) (read-line stream nil nil))
         (lines))
        ((null line) (nreverse lines))
      (push line lines))))
             
(defobfun (set-dialog-item-text *string-sequence-dialog*) (string)
  (dolist (cell (selected-cells))                     ; deselect selected cells
    (cell-deselect (point-h cell) (point-v cell)))
  (set-table-sequence (split-lines string)))

;;; -----------------------------------------------------------------------------

(defun print-gfn-name (gfn stream)
  (let* ((string (format nil "~S" (mcs::generic-function-name gfn)))
         (length (length string)))
    (do ((i 0 (1+ i)))
        ((eql i length))
      (ask stream (stream-tyo (elt string i))))
    gfn))

(defun print-method-signature (method stream)
  (let* ((string (format nil "~S" (mapcar #'mcs::class-name 
                                          (mcs::method-specializers method))))
         (length (length string)))
    (do ((i 0 (1+ i)))
        ((eql i length))
      (ask stream (stream-tyo (elt string i))))
    method))

;;; -----------------------------------------------------------------------------

(defvar *gfn-exploration-dialog* nil
  "The Generic Functions Exploration Dialog")

(defvar *menu-item-seperator* nil)

(defvar *generic-functions-menu-item*
  "The Generic Functions Exploration Dialog Menu Item")

(defun allocate-gfn-exploration-dialog ()
  (declare (object-variable my-dialog gfn-substring gfn-pkg gfn-edit-button
                            gfn-item-list method-item-list method-display-item))
  
  (setf *gfn-exploration-dialog*
        (oneof *hiding-dialog*
               :window-type :document
               :window-size #@(506 310)
               :window-position #@(2 40)
               :window-show nil
               :window-title "Generic Function Exploration Dialog"))
  
  (ask *gfn-exploration-dialog*
    (add-dialog-items
     
     (oneof *static-text-dialog-item*
            :dialog-item-text "Apropos"
            :dialog-item-position #@(4 5)
            :dialog-item-size #@(57 16))
     
     (have 'gfn-substring
           (oneof *editable-text-dialog-item*
                  :dialog-item-text "describe"
                  :dialog-item-position #@(66 5)
                  :dialog-item-size #@(168 16)
                  :allow-returns nil))
     
     (oneof *static-text-dialog-item*
            :dialog-item-text "Package"
            :dialog-item-position #@(256 5)
            :dialog-item-size #@(59 16))
     
     (have 'gfn-pkg
           (oneof *pkg-editable-text-dialog-item*
                  :dialog-item-text "user"
                  :dialog-item-position #@(325 5)
                  :dialog-item-size #@(70 16)
                  :allow-returns nil))
     
     (oneof *button-dialog-item*
            :dialog-item-text "Find"
            :dialog-item-enabled-p t
            :dialog-item-position #@(410 5)
            :dialog-item-action
            #'(lambda ()
                (catch :cancel
                  (ask my-dialog 
                    (let* ((pkg (or (find-package (string-upcase (ask gfn-pkg (dialog-item-text))))
                                    (progn (ed-beep)(throw :cancel nil))))
                           (str (ask gfn-substring (dialog-item-text)))
                           (obj-list (or (mcs::gfn-list str pkg t)
                                         (progn (ed-beep)(ed-beep)(throw :cancel nil)))))
                      (ask gfn-item-list                     ; set the gfn list
                        (set-table-sequence obj-list))
                      (ask gfn-edit-button                   ; disable edit button
                        (dialog-item-disable))
                      (ask method-item-list                  ; blank the method list
                        (set-table-sequence nil))
                      (ask method-display-item               ; blank the method display
                        (set-dialog-item-text "")))))))
     
     (have 'gfn-edit-button
           (oneof *button-dialog-item*
                  :dialog-item-text "Edit"
                  :dialog-item-enabled-p nil
                  :dialog-item-position #@(460 5)
                  :dialog-item-action
                  #'(lambda ()
                      (catch :cancel
                        (ask my-dialog 
                          (ccl::edit-definition 
                           (mcs::generic-function-name
                            (ask gfn-item-list (selection)))))))))
     
     (have 'gfn-item-list
           (oneof *object-sequence-dialog*
                  :table-hscrollp nil
                  :table-sequence nil
                  :table-print-function #'print-gfn-name
                  :dialog-item-action
                  (nfunction
                   dialog-item-action
                   (lambda ()
                     (let* ((gfn (selection))
                            (pkg (find-package 
                                  (string-upcase 
                                   (ask my-dialog
                                     (ask gfn-pkg (dialog-item-text))))))
                            (gfn-symbol (symbol-name (mcs::generic-function-name gfn))))
                       (ask my-dialog 
                         (ask method-item-list                          ; set the method list
                           (set-table-sequence (mcs::generic-function-methods gfn)))
                         (if (or (get (find-symbol gfn-symbol 'keyword) 'ccl::%source-files)
                                 (get (find-symbol gfn-symbol pkg) 'ccl::%source-files))
                           (ask gfn-edit-button (dialog-item-enable))
                           (ask gfn-edit-button (dialog-item-disable))) ; enable/disable edit button
                         (ask method-display-item                       ; blank the method display
                           (set-dialog-item-text "")))
                       (usual-dialog-item-action))))
                  :dialog-item-size #@(250 96)
                  :dialog-item-font '("Monaco" 9)
                  :cell-size #@(246 12)
                  :dialog-item-position #@(2 27)))
     
     (have 'method-item-list
           (oneof *object-sequence-dialog*
                  :table-hscrollp nil
                  :table-sequence nil
                  :table-print-function #'print-method-signature
                  :dialog-item-action
                  (nfunction
                   dialog-item-action
                   (lambda ()
                     (let* ((method (selection))
                            (description (with-output-to-string (*standard-output*)
                                           (mcs::method-describe method))))
                       (ask my-dialog
                         (ask method-display-item              ; set the method display
                           (set-dialog-item-text description)))
                       (usual-dialog-item-action))))
                  :dialog-item-size #@(250 96)
                  :dialog-item-font '("Monaco" 9)
                  :cell-size #@(246 12)
                  :dialog-item-position #@(254 27)))
     
     (have 'method-display-item
           (oneof *string-sequence-dialog*
                  :table-sequence nil
                  :table-hscrollp nil
                  :dialog-item-font '("Monaco" 9)
                  :cell-size #@(490 12)
                  :dialog-item-size #@(502 183)
                  :dialog-item-position #@(2 125))))

    (set-default-button (find-dialog-item "Find")))
  
  (setf *menu-item-seperator*
        (oneof *menu-item*
               :menu-item-title "-"))
  
  (setf *generic-functions-menu-item*
        (oneof *menu-item*
               :menu-item-title "Generic Functions..."
               :menu-item-action '(ask *gfn-exploration-dialog* (window-select))))
  
  (ask *tools-menu* (add-menu-items 
                     *menu-item-seperator* 
                     *generic-functions-menu-item*)))

;;; for dumplisp -----------------------------------------------------------

(allocate-gfn-exploration-dialog)

(defun deallocate-gfn-exploration-dialog ()
  (ask *gfn-exploration-dialog* (window-close t))
  (ask *tools-menu* (remove-menu-items *generic-functions-menu-item* 
                                       *menu-item-seperator*))
  (setf *generic-functions-menu-item* nil
        *menu-item-seperator* nil))

(progn
  (push (symbol-function 'deallocate-gfn-exploration-dialog) 
        *save-exit-functions*)
  (setf *restore-lisp-functions*
        (append *restore-lisp-functions*
                (list (symbol-function 'allocate-gfn-exploration-dialog)))))


;;    -------       exploring classes in mcs    ------------------------------

(defun print-class-name (a_class stream)
  (let* ((string (format nil "~S" (mcs::class-name a_class)))
         (length (length string)))
    (do ((i 0 (1+ i)))
        ((eql i length))
      (ask stream (stream-tyo (elt string i))))
    a_class))

(defun print-slot-definition-name (slot-definition stream)
  (let* ((string (format nil "~S" (mcs::slot-definition-name slot-definition)))
         (length (length string)))
    (do ((i 0 (1+ i)))
        ((eql i length))
      (ask stream (stream-tyo (elt string i))))
    slot-definition))


;;; -----------------------------------------------------------------------------

(defvar *class-exploration-dialog* nil
  "The Class Exploration Dialog")

(defvar *class-menu-item* nil
  "The Class Exploration Dialog Menu Item")

(defun allocate-class-exploration-dialog ()
  (declare (object-variable my-dialog class-substring class-pkg class-edit-button
                            class-item-list class-slot-item-list 
                            class-slot-display-item))
  
  (setf *class-exploration-dialog*
        (oneof *hiding-dialog*
               :window-type :document
               :window-size #@(506 310)
               :window-position #@(2 40)
               :window-show nil
               :window-title "Class Exploration Dialog"))
  
  (ask *class-exploration-dialog*
    (add-dialog-items
     
     (oneof *static-text-dialog-item*
            :dialog-item-text "Apropos"
            :dialog-item-position #@(4 5)
            :dialog-item-size #@(57 16))
     
     (have 'class-substring
           (oneof *editable-text-dialog-item*
                  :dialog-item-text "standard"
                  :dialog-item-position #@(66 5)
                  :dialog-item-size #@(168 16)
                  :allow-returns nil))
     
     (oneof *static-text-dialog-item*
            :dialog-item-text "Package"
            :dialog-item-position #@(256 5)
            :dialog-item-size #@(59 16))
     
     (have 'class-pkg
           (oneof *pkg-editable-text-dialog-item*
                  :dialog-item-text "user"
                  :dialog-item-position #@(325 5)
                  :dialog-item-size #@(70 16)
                  :allow-returns nil))
     
     (oneof *button-dialog-item*
            :dialog-item-text "Find"
            :dialog-item-enabled-p t
            :dialog-item-position #@(410 5)
            :dialog-item-action
            #'(lambda ()
                (catch :cancel
                  (ask my-dialog 
                    (let* ((pkg (or (find-package (string-upcase (ask class-pkg (dialog-item-text))))
                                    (progn (ed-beep)(throw :cancel nil))))
                           (str (ask class-substring (dialog-item-text)))
                           (cl (or (mcs::class-list str pkg t)
                                   (progn (ed-beep)(ed-beep)(throw :cancel nil)))))
                      (ask class-item-list                       ; set the class list
                        (set-table-sequence cl))
                      (ask class-edit-button                     ; disable class edit button
                        (dialog-item-disable))
                      (ask class-slot-item-list                  ; blank the slot list
                        (set-table-sequence nil))
                      (ask class-slot-display-item               ; blank the slot display
                        (set-dialog-item-text "")))))))

     (have 'class-edit-button
           (oneof *button-dialog-item*
                  :dialog-item-text "Edit"
                  :dialog-item-enabled-p nil
                  :dialog-item-position #@(460 5)
                  :dialog-item-action
                  #'(lambda ()
                      (catch :cancel
                        (ask my-dialog 
                          (ccl::edit-definition 
                           (mcs::class-name 
                            (ask class-item-list (selection)))))))))

     (have 'class-item-list
           (oneof *object-sequence-dialog*
                  :table-hscrollp nil
                  :table-sequence nil
                  :table-print-function #'print-class-name
                  :dialog-item-action
                  (nfunction
                   dialog-item-action
                   (lambda ()
                     (let ((a_class (selection)))
                       (ask my-dialog 
                         (ask class-slot-item-list                    ; set the slot list
                           (set-table-sequence (mcs::class-slots a_class)))
                         (if (get (find-symbol 
                                   (symbol-name (mcs::class-name a_class))
                                   (find-package (string-upcase (ask class-pkg (dialog-item-text))))) 
                                  'ccl::%source-files)
                           (ask class-edit-button (dialog-item-enable))
                           (ask class-edit-button (dialog-item-disable))) ; enable/disable the class edit button
                         (ask class-slot-display-item                     ; blank the slot display
                           (set-dialog-item-text "")))
                       (usual-dialog-item-action))))
                  :dialog-item-size #@(250 96)
                  :dialog-item-font '("Monaco" 9)
                  :cell-size #@(246 12)
                  :dialog-item-position #@(2 27)))
     
     (have 'class-slot-item-list
           (oneof *object-sequence-dialog*
                  :table-hscrollp nil
                  :table-sequence nil
                  :table-print-function #'print-slot-definition-name
                  :dialog-item-action
                  (nfunction
                   dialog-item-action
                   (lambda ()
                     (let ((description (with-output-to-string (*standard-output*)
                                          (mcs::obj-describe (selection)))))
                       (ask my-dialog
                         (ask class-slot-display-item               ; set the method display
                           (set-dialog-item-text description)))
                       (usual-dialog-item-action))))
                  :dialog-item-size #@(250 96)
                  :dialog-item-font '("Monaco" 9)
                  :cell-size #@(246 12)
                  :dialog-item-position #@(254 27)))
     
     (have 'class-slot-display-item
           (oneof *string-sequence-dialog*
                  :table-sequence nil
                  :table-hscrollp nil
                  :dialog-item-font '("Monaco" 9)
                  :cell-size #@(490 12)
                  :dialog-item-size #@(502 183)
                  :dialog-item-position #@(2 125))))

    (set-default-button (find-dialog-item "Find")))

    (setf *class-menu-item* 
        (oneof *menu-item*
               :menu-item-title "Classes..."
               :menu-item-action '(ask *class-exploration-dialog* (window-select))))

    (ask *tools-menu* (add-menu-items *class-menu-item*)))

;;; for dumplisp -----------------------------------------------------------

(allocate-class-exploration-dialog)

(defun deallocate-class-exploration-dialog ()
  (ask *class-exploration-dialog* (window-close t))
  (ask *tools-menu* (remove-menu-items *class-menu-item*))
  (setf *class-menu-item* nil))

(progn
  (push (symbol-function 'deallocate-class-exploration-dialog) 
        *save-exit-functions*)
  (setf *restore-lisp-functions*
        (append *restore-lisp-functions*
                (list (symbol-function 'allocate-class-exploration-dialog)))))


;;; for package export exploration ------------------------------------------


(defvar *pkg-export-dialog* nil
  "The Package Exports Exploration Dialog")

(defvar *pkg-exports-menu-item*
  "The Package Exports Exploration Dialog Menu Item")

(defun external-symbols-list (str pkg sort)
  (let (sl)
    (do-external-symbols (s pkg)
      (if (search str (symbol-name s)) (push s sl)))
    (if sort (sort sl #'string<) sl)))

(defun package-description-string (pkg)
  (format nil
          "Current-Package: ~A~@
           Package: ~A~@
           Nicknames: ~{~%   ~A~}~@
           Use-List: ~{~%   ~A~}~@
           Used-By-List: ~{~%   ~A~}~@
           Shadowing-Symbols: ~{~%   ~A~}"
          (package-name *package*)
          (package-name pkg)
          (mapcar #'package-name (package-nicknames pkg))
          (mapcar #'package-name (package-use-list pkg))
          (mapcar #'package-name (package-used-by-list pkg))
          (package-shadowing-symbols pkg)))

(defun allocate-pkg-export-dialog ()
  (declare (object-variable my-dialog symbol-substring export-pkg inspect-symbol-button
                            export-symbol-list-item package-description-item 
                            symbol-description-item))
  
  (setf *pkg-export-dialog*
        (oneof *hiding-dialog*
               :window-type :document
               :window-size #@(506 310)
               :window-position #@(2 40)
               :window-show nil
               :window-title "Package Exports Exploration Dialog"))
  
  (ask *pkg-export-dialog*
    (add-dialog-items
     
     (oneof *static-text-dialog-item*
            :dialog-item-text "Apropos"
            :dialog-item-position #@(4 5)
            :dialog-item-size #@(57 16))
     
     (have 'symbol-substring
           (oneof *editable-text-dialog-item*
                  :dialog-item-text ""
                  :dialog-item-position #@(66 5)
                  :dialog-item-size #@(168 16)
                  :allow-returns nil))
     
     (oneof *static-text-dialog-item*
            :dialog-item-text "Package"
            :dialog-item-position #@(256 5)
            :dialog-item-size #@(59 16))
     
     (have 'export-pkg
           (oneof *pkg-editable-text-dialog-item*
                  :dialog-item-text "mcs"
                  :dialog-item-position #@(320 5)
                  :dialog-item-size #@(70 16)
                  :allow-returns nil))
     
     (oneof *button-dialog-item*
            :dialog-item-text "Find"
            :dialog-item-enabled-p t
            :dialog-item-position #@(398 5)
            :dialog-item-action
            #'(lambda ()
                (catch :cancel
                  (ask my-dialog 
                    (let* ((pkg (or (find-package (string-upcase (ask export-pkg (dialog-item-text))))
                                    (progn (ed-beep)(throw :cancel nil))))
                           (str (string-upcase (ask symbol-substring (dialog-item-text))))
                           (symbol-list (or (external-symbols-list str pkg t)
                                            (progn (ed-beep)(ed-beep)(throw :cancel nil)))))
                      (ask export-symbol-list-item (set-table-sequence symbol-list))
                      (ask inspect-symbol-button (dialog-item-disable))
                      (ask package-description-item (set-dialog-item-text (package-description-string pkg)))
                      (ask symbol-description-item (set-dialog-item-text "")))))))
     
     (have 'inspect-symbol-button
           (oneof *button-dialog-item*
                  :dialog-item-text "Inspect"
                  :dialog-item-enabled-p nil
                  :dialog-item-position #@(440 5)
                  :dialog-item-action
                  #'(lambda ()
                      (catch :cancel
                        (ask my-dialog 
                          (inspect (ask export-symbol-list-item 
                                     (cell-contents (car (selected-cells))))))))))
     
     (have 'export-symbol-list-item
           (oneof *sequence-dialog-item*
                  :table-hscrollp nil
                  :table-sequence nil
                  :dialog-item-action
                  (nfunction
                   dialog-item-action
                   (lambda ()
                     (let ((symbol (cell-contents (car (selected-cells)))))
                       (ask my-dialog 
                         (ask inspect-symbol-button (dialog-item-enable))
                         (ask symbol-description-item
                           (set-dialog-item-text (with-output-to-string (*standard-output*)
                                                   (describe symbol))))))
                     (usual-dialog-item-action)))
                  :dialog-item-size #@(250 96)
                  :dialog-item-font '("Monaco" 9)
                  :cell-size #@(246 12)
                  :dialog-item-position #@(2 27)))
     
     (have 'package-description-item
           (oneof *string-sequence-dialog*
                  :table-hscrollp nil
                  :table-sequence nil
                  :dialog-item-size #@(250 96)
                  :dialog-item-font '("Monaco" 9)
                  :cell-size #@(246 12)
                  :dialog-item-position #@(254 27)))
     
     (have 'symbol-description-item
           (oneof *string-sequence-dialog*
                  :table-sequence nil
                  :table-hscrollp nil
                  :dialog-item-font '("Monaco" 9)
                  :cell-size #@(490 12)
                  :dialog-item-size #@(502 183)
                  :dialog-item-position #@(2 125))))

    (set-default-button (find-dialog-item "Find")))
  
  (setf *pkg-exports-menu-item*
        (oneof *menu-item*
               :menu-item-title "Package Exports..."
               :menu-item-action '(ask *pkg-export-dialog* (window-select))))
  
  (ask *tools-menu* (add-menu-items *pkg-exports-menu-item*)))

;;; for dumplisp -----------------------------------------------------------

(allocate-pkg-export-dialog)

(defun deallocate-pkg-export-dialog ()
  (ask *pkg-export-dialog* (window-close t))
  (ask *tools-menu* (remove-menu-items *pkg-exports-menu-item*))
  (setf *pkg-exports-menu-item* nil))

(progn
  (push (symbol-function 'deallocate-pkg-export-dialog) 
        *save-exit-functions*)
  (setf *restore-lisp-functions*
        (append *restore-lisp-functions*
                (list (symbol-function 'allocate-pkg-export-dialog)))))


;;; eof

