;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: "USER" -*-

;;;           Copyright  1990    BY
;;;           G M D  
;;;           Postfach 1240
;;;           D-5205 St. Augustin
;;;           FRG

;;; -----------------------------------------------------------------------------------
;;;
;;; description:    Import/Export Specification of the Meta Class System MCS
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

;;; -----------------------------------------------------------------------------------

;;; Import/Export Specification of the Meta Class System MCS

(user::defpackage "MCS"
  (:size 1024)
  (:use)
  (:import-from "LISP"
                
                ;; This imports are computed reading all MCS files (180 symbols).
                ;; Note, CL exports 982 symbols.
                
                "&AUX" 
                "&KEY" 
                "&OPTIONAL" 
                "&REST" 
                "*FEATURES*" 
                "*PACKAGE*" 
                "+" 
                "-" 
                "/" 
                "1+" 
                "1-" 
                "<" 
                "=" 
                ">" 
                ">=" 
                "ACONS" 
                "AND" 
                "APPEND" 
                "APPLY" 
                "ARRAY" 
                "ARRAY-TOTAL-SIZE" 
                "ASSOC" 
                "BIT-VECTOR" 
                "BOUNDP" 
                "BUTLAST" 
                "CAAR" 
                "CADDR" 
                "CADR" 
                "CAR" 
                "CASE" 
                "CDAR" 
                "CDDR" 
                "CDR" 
                "CHAR-EQUAL" 
                "CHARACTER" 
                "COERCE" 
                "COMPILE" 
                "COMPILE-FILE" 
                "COMPLEX" 
                "CONCATENATE" 
                "COND" 
                "CONS" 
                "CONSP" 
                "COPY-SEQ" 
                "DECF" 
                "DECLARE" 
                "DEFMACRO" 
                "DEFSETF" 
                "DEFSTRUCT" 
                "DEFTYPE" 
                "DEFUN" 
                "DEFVAR" 
                "DELETE" 
                "DESCRIBE" 
                "DO" 
                "DOCUMENTATION" 
                "DOLIST" 
                "DOTIMES" 
                "EQ" 
                "EQL" 
                "EQUAL" 
                "ERROR" 
                "EVAL" 
                "FBOUNDP" 
                "FILE-WRITE-DATE" 
                "FILL-POINTER" 
                "FIND-SYMBOL" 
                "FIRST" 
                "FLOAT" 
                "FMAKUNBOUND" 
                "FORMAT" 
                "FOURTH" 
                "FUNCALL" 
                "FUNCTION" 
                "FUNCTIONP" 
                "GENSYM" 
                "GET" 
                "GETF" 
                "GETHASH" 
                "HASH-TABLE" 
                "IDENTITY" 
                "IF" 
                "IGNORE" 
                "IN-PACKAGE" 
                "INCF" 
                "INTEGER" 
                "INTERN" 
                "KEYWORDP" 
                "LABELS" 
                "LAMBDA" 
                "LAST" 
                "LENGTH" 
                "LET" 
                "LIST" 
                "LIST*" 
                "LIST-LENGTH" 
                "LISTP" 
                "LOAD"
                "LOCALLY"
                "LOOP" 
                "MACROLET"
                "MAKE-ARRAY" 
                "MAKE-HASH-TABLE" 
                "MAKE-LIST" 
                "MAPC" 
                "MAPCAN" 
                "MAPCAR" 
                "MAPHASH" 
                "MAX" 
                "MEMBER" 
                "NAMESTRING" 
                "NCONC" 
                "NIL"       ; this is a hack for EXCL 4.0
                "NOT" 
                "NREVERSE" 
                "NSUBST" 
                "NTH" 
                "NULL" 
                "NUMBER" 
                "OPTIMIZE" 
                "OR" 
                "PACKAGE" 
                "POP" 
                "POSITION" 
                "PPRINT" 
                "PRINT" 
                "PROBE-FILE" 
                "PROGN" 
                "PUSH" 
                "PUSHNEW" 
                "QUOTE" 
                "RATIO" 
                "RATIONAL" 
                "REMHASH" 
                "REMOVE" 
                "REMOVE-DUPLICATES" 
                "REST" 
                "RETURN" 
                "REVERSE" 
                "SAFETY" 
                "SATISFIES" 
                "SEARCH" 
                "SECOND" 
                "SEQUENCE" 
                "SETF" 
                "SETQ" 
                "SORT" 
                "SPECIAL" 
                "SPEED" 
                ;"STANDARD" 
                ;"STANDARD-CLASS" 
                ;"STANDARD-GENERIC-FUNCTION" 
                ;"STANDARD-METHOD" 
                ;"STANDARD-OBJECT" 
                "STREAM" 
                "STRING" 
                "STRING-CAPITALIZE" 
                "STRING-DOWNCASE" 
                "STRING-UPCASE" 
                "STRING<=" 
                "STRING=" 
                "STRINGP" 
                "SUBSEQ" 
                "SUBTYPEP" 
                "SVREF" 
                "SYMBOL" 
                "SYMBOL-FUNCTION" 
                "SYMBOL-NAME" 
                "SYMBOL-PACKAGE" 
                "SYMBOL-VALUE" 
                "SYMBOLP" 
                "T" 
                "THIRD" 
                "TYPE" 
                "TYPE-OF" 
                "TYPECASE" 
                "TYPEP" 
                "UNLESS" 
                "VECTOR" 
                "WARN" 
                "WHEN")
  
  (:export 

   ;; ---------------------------------------------------------------------------------   
   ;;  Application Programmer Interface
   ;; ---------------------------------------------------------------------------------   

   ;; expressions
   "DEFCLASS"
   "DEFABSTRACT"
   "DEFMIXIN"
   "DEFCONSTRUCTOR"
   "DEFGENERIC"
   "DEFMETHOD"
   "CALL-NEXT-METHOD"
   "NEXT-METHOD-P"
   
   ;; object operations
   
   "INITIALIZE-INSTANCE"
   "REINITIALIZE-INSTANCE"
   "SLOT-VALUE"
   "SLOT-BOUNDP"
   "SLOT-MAKUNBOUND"
   "OBJ-DESCRIBE"
   "PRINT-OBJECT"
   "OBJ-TYPEP"
   "OBJ-TYPE-OF"
   
   ;; creating instances
   
   "MAKE-INSTANCE"
   ;; other symbols
   
   "TYPEP"
   ; :reader
   ; :writer
   ; :accessor
   ; :initform
   ; :initarg
   ; :type
   ; :documentation
   ; :method-class
   ; :method-combination
   ; :before
   ; :after
   ; :around
   
   ;; -----------------------------------------------------------------------------------   
   ;;  System Programmer Interface
   ;;  additional to the Application Programmer Interface
   ;; -----------------------------------------------------------------------------------   
   
   ;; expressions
   
   "DEFREADER"
   "DEFWRITER"
   
   ;; objects
   
   "CLASS-OF"
   "SLOT-EXISTS-P"
   "CHANGE-CLASS"
   "OBJ-COPY"
   
   ;; names of built-in classes 
   
   "T"
   "NUMBER"  "COMPLEX"  "RATIONAL" "RATIO"  "INTEGER"  "FLOAT"
   "CHARACTER"
   "SYMBOL"  "NULL"
   "ARRAY"  "SEQUENCE"  "VECTOR"  "STRING"  "BIT-VECTOR"  "LIST"  "CONS"
   
   "HASH-TABLE"		; not documented
   "STREAM"		; not documented
   "FUNCTION"		; not documented
   
   ;; names of defined classes
   
   "OBJECT"
   
   "ABSTRACT"          "INSTANTIABLE"
   "SINGLE-INHERITED"  "MULTIPLE-INHERITED"
   "DEFINED"           "BUILT-IN"
   
   "CLASS"
   "ABSTRACT-BUILT-IN-CLASS"
   "BUILT-IN-CLASS"
   "MIXIN-CLASS"
   "ABSTRACT-BASE-CLASS"
   "BASE-CLASS"
   
   "REDEFINABLE-MIXIN-CLASS"
   "REDEFINABLE-ABSTRACT-BASE-CLASS"
   "REDEFINABLE-BASE-CLASS"
   "REDEFINABLE"
   "REDEFINABLE-INSTANTIABLE"
   
   "SLOT-DEFINITION"
   "GENERIC-FUNCTION"  "READER"         "WRITER"
   "METHOD"            "READER-METHOD"  "WRITER-METHOD"
   "METHOD-COMBINATION"
   
   "STANDARD-MIXIN-CLASS"
   "STANDARD-ABSTRACT-BASE-CLASS"
   "STANDARD-BASE-CLASS"
   
   
   ;; classes and slot definitions
   
   ;; initarg names
   "NAME"          "SUPERCLASSES"       "SLOTS"                    "INITARGS"
   "INITFORM"      "INITFUNCTION"       "SLOT-CLASS"               "TYPE"
   
   ;; slot names
   "DIRECT-SUPERCLASSES"
   "DIRECT-SUBCLASSES"
   "DIRECT-SLOTS"
   "DIRECT-METHODS"
   
   ;; operations on class names
   "FIND-CLASS"
   "ENSURE-CLASS"
   "OBJ-SUBTYPEP"
   
   ;; predicates
   "ABSTRACT-P"  "CLASS-P"  "MIXIN-P"  "METACLASS-P"
   
   ;; accessors
   "CLASS-DIRECT-METHODS"
   "CLASS-DIRECT-SUBCLASSES"
   "CLASS-DIRECT-SUPERCLASSES"
   "CLASS-NAME"
   "CLASS-PRECEDENCE-LIST"
   "CLASS-DIRECT-SLOTS"
   "CLASS-SLOTS"
   "CLASS-DIRECT-INITARGS"
   "CLASS-INITARGS"
   "FIND-SLOT"
   "CLASS-PROTOTYPE"

   "SLOT-DEFINITION-INITFORM"
   "SLOT-DEFINITION-INITFUNCTION"
   "SLOT-DEFINITION-NAME"
   "SLOT-DEFINITION-TYPE"
         
   ;;creating instances
   "ALLOCATE-INSTANCE"
   
   ;;creating accessors
   "MAKE-READER"
   "MAKE-WRITER"
   
   ;; inheritance
   "FINALIZE-INHERITANCE"
   "COMPUTE-CLASS-PRECEDENCE-LIST"
   "COMPUTE-EFFECTIVE-SLOT-DEFINITION"
   "COMPUTE-SLOTS"
   "COMPUTE-INITARGS"
   "DIRECT-SLOT-DEFINITION-CLASS"
   "EFFECTIVE-SLOT-DEFINITION-CLASS"
   "VALIDATE-SUPERCLASS"
   
   ;; exceptions
   "SLOT-MISSING"
   "SLOT-UNBOUND"
      
   ;; generic functions and methods
   
   ;; predicates
   "GENERIC-P"
   "METHOD-P"

   ;; initarg names
   "QUALIFIERS"    "LAMBDA-LIST"        "SPECIALIZERS"             "FUNCTION"
   "METHOD-CLASS"  "METHOD-COMBINATION" "DISCRIMINATING-FUNCTION"

   ;; access functions
   "FIND-METHOD"
   "GENERIC-FUNCTION-LAMBDA-LIST"
   "GENERIC-FUNCTION-METHOD-CLASS"
   "GENERIC-FUNCTION-METHODS"
   "GENERIC-FUNCTION-NAME"

   "METHOD-FUNCTION"
   "METHOD-GENERIC-FUNCTION"
   "METHOD-LAMBDA-LIST"
   "METHOD-NAME"
   "METHOD-QUALIFIERS"
   "METHOD-SPECIALIZERS"
   
   ;; updating methods
   "ADD-METHOD"
   "REMOVE-METHOD"
   
   ;; applicable methods
   "COMPUTE-APPLICABLE-METHODS"
   
   ;; operaions on gfn names
   "FIND-GFN"
   
   ;; exception
   "NO-APPLICABLE-METHOD"
   "NO-NEXT-METHOD"
            
   ;; other symbols
   
   "STANDARD"
   "<UNBOUND>"
   
   ;; -----------------------------------------------------------------------------------
   ;;  Programming Environment
   ;; -----------------------------------------------------------------------------------   
  
   ;; expressions
   "UNDEFGENERIC"
   "UNDEFMETHOD"
   
   ;; object operations
   "OBJ-DOCUMENTATION"
   "APROPOS-PROTOCOL"
   "OBJECT-PROTOCOL"
   
   ;; class operations
   "CLASS-DIRECT-PROTOCOL"
   "CLASS-PROTOCOL"
   
   ;; redefinition
   "REDEFINE-MODE"
         
   ;; CLOS compatibility
   "STANDARD-OBJECT"
   "STANDARD-CLASS"
   "STANDARD-METHOD"
   "STANDARD-GENERIC-FUNCTION"
   "STANDARD-SLOT-DEFINITION"
   
   ;; Help Macros
   
   "DOPLIST"))


;;; eof


