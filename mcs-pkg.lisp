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

(cl:in-package :cl-user)

(defpackage "MCS"
  (:size 1024)
  (:use)
  (:import-from "COMMON-LISP"
                
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
   
   "DOPLIST")
  ;; Common Lisp
  (:import-from :cl
                :&allow-other-keys :&aux :&body :&environment :&key :&optional :&rest :&whole
                :* :** :*** :*break-on-signals* :*compile-file-pathname*
                :*compile-file-truename* :*compile-print* :*compile-verbose* :*debug-io*
                :*debugger-hook* :*default-pathname-defaults* :*error-output* :*features*
                :*gensym-counter* :*load-pathname* :*load-print* :*load-truename*
                :*load-verbose* :*macroexpand-hook* :*modules* :*package* :*print-array*
                :*print-base* :*print-case* :*print-circle* :*print-escape* :*print-gensym*
                :*print-length* :*print-level* :*print-lines* :*print-miser-width*
                :*print-pprint-dispatch* :*print-pretty* :*print-radix* :*print-readably*
                :*print-right-margin* :*query-io* :*random-state* :*read-base*
                :*read-default-float-format* :*read-eval* :*read-suppress* :*readtable*
                :*standard-input* :*standard-output* :*terminal-io* :*trace-output* :+ :++
                :+++ :- :/ :// :/// :/= :1+ :1- :< :<= := :> :>= :abort :abs :acons :acos
                :acosh :adjoin :adjust-array :adjustable-array-p :alpha-char-p :alphanumericp
                :and :append :apply :apropos :apropos-list :aref :arithmetic-error
                :arithmetic-error-operands :arithmetic-error-operation :array :array-dimension
                :array-dimension-limit :array-dimensions :array-displacement
                :array-element-type :array-has-fill-pointer-p :array-in-bounds-p :array-rank
                :array-rank-limit :array-row-major-index :array-total-size
                :array-total-size-limit :arrayp :ash :asin :asinh :assert :assoc :assoc-if
                :assoc-if-not :atan :atanh :atom :base-char :base-string :bignum :bit :bit-and
                :bit-andc1 :bit-andc2 :bit-eqv :bit-ior :bit-nand :bit-nor :bit-not :bit-orc1
                :bit-orc2 :bit-vector :bit-vector-p :bit-xor :block :boole :boole-1 :boole-2
                :boole-and :boole-andc1 :boole-andc2 :boole-c1 :boole-c2 :boole-clr :boole-eqv
                :boole-ior :boole-nand :boole-nor :boole-orc1 :boole-orc2 :boole-set
                :boole-xor :boolean :both-case-p :boundp :break :broadcast-stream
                :broadcast-stream-streams :butlast :byte :byte-position :byte-size :caaaar
                :caaadr :caaar :caadar :caaddr :caadr :caar :cadaar :cadadr :cadar :caddar
                :cadddr :caddr :cadr :call-arguments-limit :call-method :car :case :catch
                :ccase :cdaaar :cdaadr :cdaar :cdadar :cdaddr :cdadr :cdar :cddaar :cddadr
                :cddar :cdddar :cddddr :cdddr :cddr :cdr :ceiling :cell-error :cell-error-name
                :cerror :char :char-code :char-code-limit :char-downcase :char-equal
                :char-greaterp :char-int :char-lessp :char-name :char-not-equal
                :char-not-greaterp :char-not-lessp :char-upcase :char/= :char< :char<= :char=
                :char> :char>= :character :characterp :check-type :cis :clear-input
                :clear-output :close :clrhash :code-char :coerce :compilation-speed :compile
                :compile-file :compile-file-pathname :compiled-function :compiled-function-p
                :compiler-macro :compiler-macro-function :complement :complex :complexp
                :compute-restarts :concatenate :concatenated-stream
                :concatenated-stream-streams :cond :condition :conjugate :cons :consp
                :constantly :constantp :continue :control-error :copy-alist :copy-list
                :copy-pprint-dispatch :copy-readtable :copy-seq :copy-structure :copy-symbol
                :copy-tree :cos :cosh :count :count-if :count-if-not :ctypecase :debug :decf
                :declaim :declaration :declare :decode-float :decode-universal-time
                :defconstant :define-compiler-macro :define-condition
                :define-method-combination :define-modify-macro :define-setf-expander
                :define-symbol-macro :defmacro :defpackage :defparameter :defsetf :defstruct
                :deftype :defun :defvar :delete :delete-duplicates :delete-file :delete-if
                :delete-if-not :delete-package :denominator :deposit-field :describe
                :describe-object :destructuring-bind :digit-char :digit-char-p :directory
                :directory-namestring :disassemble :division-by-zero :do :do* :do-all-symbols
                :do-external-symbols :do-symbols :documentation :dolist :dotimes :double-float
                :double-float-epsilon :double-float-negative-epsilon :dpb :dribble
                :dynamic-extent :ecase :echo-stream :echo-stream-input-stream
                :echo-stream-output-stream :ed :eighth :elt :encode-universal-time
                :end-of-file :endp :enough-namestring :ensure-directories-exist
                :ensure-generic-function :eq :eql :equal :equalp :error :etypecase :eval
                :eval-when :evenp :every :exp :export :expt :extended-char :fboundp :fceiling
                :fdefinition :ffloor :fifth :file-author :file-error :file-error-pathname
                :file-length :file-namestring :file-position :file-stream :file-string-length
                :file-write-date :fill :fill-pointer :find :find-all-symbols :find-if
                :find-if-not :find-package :find-restart :find-symbol :finish-output :first
                :fixnum :flet :float :float-digits :float-precision :float-radix :float-sign
                :floating-point-inexact :floating-point-invalid-operation
                :floating-point-overflow :floating-point-underflow :floatp :floor :fmakunbound
                :force-output :format :formatter :fourth :fresh-line :fround :ftruncate :ftype
                :funcall :function :function-keywords :function-lambda-expression :functionp
                :gcd :gensym :gentemp :get :get-decoded-time :get-dispatch-macro-character
                :get-internal-real-time :get-internal-run-time :get-macro-character
                :get-output-stream-string :get-properties :get-setf-expansion
                :get-universal-time :getf :gethash :go :graphic-char-p :handler-bind
                :handler-case :hash-table :hash-table-count :hash-table-p
                :hash-table-rehash-size :hash-table-rehash-threshold :hash-table-size
                :hash-table-test :host-namestring :identity :if :ignorable :ignore
                :ignore-errors :imagpart :import :in-package :incf :inline :input-stream-p
                :inspect :integer :integer-decode-float :integer-length :integerp
                :interactive-stream-p :intern :internal-time-units-per-second :intersection
                :invalid-method-error :invoke-debugger :invoke-restart
                :invoke-restart-interactively :isqrt :keyword :keywordp :labels :lambda
                :lambda-list-keywords :lambda-parameters-limit :last :lcm :ldb :ldb-test
                :ldiff :least-negative-double-float :least-negative-long-float
                :least-negative-normalized-double-float :least-negative-normalized-long-float
                :least-negative-normalized-short-float :least-negative-normalized-single-float
                :least-negative-short-float :least-negative-single-float
                :least-positive-double-float :least-positive-long-float
                :least-positive-normalized-double-float :least-positive-normalized-long-float
                :least-positive-normalized-short-float :least-positive-normalized-single-float
                :least-positive-short-float :least-positive-single-float :length :let :let*
                :lisp-implementation-type :lisp-implementation-version :list :list*
                :list-all-packages :list-length :listen :listp :load
                :load-logical-pathname-translations :load-time-value :locally :log :logand
                :logandc1 :logandc2 :logbitp :logcount :logeqv :logical-pathname
                :logical-pathname-translations :logior :lognand :lognor :lognot :logorc1
                :logorc2 :logtest :logxor :long-float :long-float-epsilon
                :long-float-negative-epsilon :long-site-name :loop :loop-finish :lower-case-p
                :machine-instance :machine-type :machine-version :macro-function :macroexpand
                :macroexpand-1 :macrolet :make-array :make-broadcast-stream
                :make-concatenated-stream :make-condition :make-dispatch-macro-character
                :make-echo-stream :make-hash-table :make-instances-obsolete :make-list
                :make-load-form :make-load-form-saving-slots :make-package
                :make-pathname :make-random-state :make-sequence :make-string
                :make-string-input-stream :make-string-output-stream :make-symbol
                :make-synonym-stream :make-two-way-stream :makunbound :map :map-into :mapc
                :mapcan :mapcar :mapcon :maphash :mapl :maplist :mask-field :max :member
                :member-if :member-if-not :merge :merge-pathnames :method-combination-error
                :min :minusp :mismatch :mod :most-negative-double-float :most-negative-fixnum
                :most-negative-long-float :most-negative-short-float
                :most-negative-single-float :most-positive-double-float :most-positive-fixnum
                :most-positive-long-float :most-positive-short-float
                :most-positive-single-float :muffle-warning :multiple-value-bind
                :multiple-value-call :multiple-value-list :multiple-value-prog1
                :multiple-value-setq :multiple-values-limit :name-char :namestring :nbutlast
                :nconc :nil :nintersection :ninth :not :notany :notevery :notinline :nreconc
                :nreverse :nset-difference :nset-exclusive-or :nstring-capitalize
                :nstring-downcase :nstring-upcase :nsublis :nsubst :nsubst-if :nsubst-if-not
                :nsubstitute :nsubstitute-if :nsubstitute-if-not :nth :nth-value :nthcdr :null
                :number :numberp :numerator :nunion :oddp :open :open-stream-p :optimize :or
                :otherwise :output-stream-p :package :package-error :package-error-package
                :package-name :package-nicknames :package-shadowing-symbols :package-use-list
                :package-used-by-list :packagep :pairlis :parse-error :parse-integer
                :parse-namestring :pathname :pathname-device :pathname-directory
                :pathname-host :pathname-match-p :pathname-name :pathname-type
                :pathname-version :pathnamep :peek-char :phase :pi :plusp :pop :position
                :position-if :position-if-not :pprint :pprint-dispatch
                :pprint-exit-if-list-exhausted :pprint-fill :pprint-indent :pprint-linear
                :pprint-logical-block :pprint-newline :pprint-pop :pprint-tab :pprint-tabular
                :prin1 :prin1-to-string :princ :princ-to-string :print :print-not-readable
                :print-not-readable-object :print-unreadable-object :probe-file :proclaim
                :prog :prog* :prog1 :prog2 :progn :program-error :progv :provide :psetf :psetq
                :push :pushnew :quote :random :random-state :random-state-p :rassoc :rassoc-if
                :rassoc-if-not :ratio :rational :rationalize :rationalp :read :read-byte
                :read-char :read-char-no-hang :read-delimited-list :read-from-string
                :read-line :read-preserving-whitespace :read-sequence :reader-error :readtable
                :readtable-case :readtablep :real :realp :realpart :reduce :rem :remf :remhash
                :remove :remove-duplicates :remove-if :remove-if-not :remprop :rename-file
                :rename-package :replace :require :rest :restart :restart-bind :restart-case
                :restart-name :return :return-from :revappend :reverse :room :rotatef :round
                :row-major-aref :rplaca :rplacd :safety :satisfies :sbit :scale-float :schar
                :search :second :sequence :serious-condition :set :set-difference
                :set-dispatch-macro-character :set-exclusive-or :set-macro-character
                :set-pprint-dispatch :set-syntax-from-char :setf :setq :seventh :shadow
                :shadowing-import :shared-initialize :shiftf :short-float :short-float-epsilon
                :short-float-negative-epsilon :short-site-name :signal :signed-byte :signum
                :simple-array :simple-base-string :simple-bit-vector :simple-bit-vector-p
                :simple-condition :simple-condition-format-arguments
                :simple-condition-format-control :simple-error :simple-string :simple-string-p
                :simple-type-error :simple-vector :simple-vector-p :simple-warning :sin
                :single-float :single-float-epsilon :single-float-negative-epsilon :sinh
                :sixth :sleep :software-type :software-version :some :sort :space :special
                :special-operator-p :speed :sqrt :stable-sort :standard-char :standard-char-p
                :step :storage-condition :store-value :stream :stream-element-type
                :stream-error :stream-error-stream :stream-external-format :streamp :string
                :string-capitalize :string-downcase :string-equal :string-greaterp
                :string-left-trim :string-lessp :string-not-equal :string-not-greaterp
                :string-not-lessp :string-right-trim :string-stream :string-trim
                :string-upcase :string/= :string< :string<= :string= :string> :string>=
                :stringp :structure :structure-class :structure-object :style-warning :sublis
                :subseq :subsetp :subst :subst-if :subst-if-not :substitute :substitute-if
                :substitute-if-not :subtypep :svref :sxhash :symbol :symbol-function
                :symbol-macrolet :symbol-name :symbol-package :symbol-plist :symbol-value
                :symbolp :synonym-stream :synonym-stream-symbol :t :tagbody :tailp :tan :tanh
                :tenth :terpri :the :third :throw :time :trace :translate-logical-pathname
                :translate-pathname :tree-equal :truename :truncate :two-way-stream
                :two-way-stream-input-stream :two-way-stream-output-stream :type :type-error
                :type-error-datum :type-error-expected-type :type-of :typecase :typep
                :unbound-slot :unbound-slot-instance :unbound-variable :undefined-function
                :unexport :unintern :union :unless :unread-char :unsigned-byte :untrace
                :unuse-package :unwind-protect :update-instance-for-different-class
                :update-instance-for-redefined-class :upgraded-array-element-type
                :upgraded-complex-part-type :upper-case-p :use-package :use-value
                :user-homedir-pathname :values :values-list :variable :vector :vector-pop
                :vector-push :vector-push-extend :vectorp :warn :warning :when
                :wild-pathname-p :with-accessors :with-compilation-unit
                :with-condition-restarts :with-hash-table-iterator :with-input-from-string
                :with-open-file :with-open-stream :with-output-to-string
                :with-package-iterator :with-simple-restart :with-slots
                :with-standard-io-syntax :write :write-byte :write-char :write-line
                :write-sequence :write-string :write-to-string :y-or-n-p :yes-or-no-p :zerop)
  (:export
   :&allow-other-keys :&aux :&body :&environment :&key :&optional :&rest :&whole
   :* :** :*** :*break-on-signals* :*compile-file-pathname*
   :*compile-file-truename* :*compile-print* :*compile-verbose* :*debug-io*
   :*debugger-hook* :*default-pathname-defaults* :*error-output* :*features*
   :*gensym-counter* :*load-pathname* :*load-print* :*load-truename*
   :*load-verbose* :*macroexpand-hook* :*modules* :*package* :*print-array*
   :*print-base* :*print-case* :*print-circle* :*print-escape* :*print-gensym*
   :*print-length* :*print-level* :*print-lines* :*print-miser-width*
   :*print-pprint-dispatch* :*print-pretty* :*print-radix* :*print-readably*
   :*print-right-margin* :*query-io* :*random-state* :*read-base*
   :*read-default-float-format* :*read-eval* :*read-suppress* :*readtable*
   :*standard-input* :*standard-output* :*terminal-io* :*trace-output* :+ :++
   :+++ :- :/ :// :/// :/= :1+ :1- :< :<= := :> :>= :abort :abs :acons :acos
   :acosh :add-method :adjoin :adjust-array :adjustable-array-p
   :allocate-instance :alpha-char-p :alphanumericp :and :append :apply :apropos
   :apropos-list :aref :arithmetic-error :arithmetic-error-operands
   :arithmetic-error-operation :array :array-dimension :array-dimension-limit
   :array-dimensions :array-displacement :array-element-type
   :array-has-fill-pointer-p :array-in-bounds-p :array-rank :array-rank-limit
   :array-row-major-index :array-total-size :array-total-size-limit :arrayp :ash
   :asin :asinh :assert :assoc :assoc-if :assoc-if-not :atan :atanh :atom
   :base-char :base-string :bignum :bit :bit-and :bit-andc1 :bit-andc2 :bit-eqv
   :bit-ior :bit-nand :bit-nor :bit-not :bit-orc1 :bit-orc2 :bit-vector
   :bit-vector-p :bit-xor :block :boole :boole-1 :boole-2 :boole-and :boole-andc1
   :boole-andc2 :boole-c1 :boole-c2 :boole-clr :boole-eqv :boole-ior :boole-nand
   :boole-nor :boole-orc1 :boole-orc2 :boole-set :boole-xor :boolean :both-case-p
   :boundp :break :broadcast-stream :broadcast-stream-streams :built-in-class
   :butlast :byte :byte-position :byte-size :caaaar :caaadr :caaar :caadar
   :caaddr :caadr :caar :cadaar :cadadr :cadar :caddar :cadddr :caddr :cadr
   :call-arguments-limit :call-method :call-next-method :car :case :catch :ccase
   :cdaaar :cdaadr :cdaar :cdadar :cdaddr :cdadr :cdar :cddaar :cddadr :cddar
   :cdddar :cddddr :cdddr :cddr :cdr :ceiling :cell-error :cell-error-name
   :cerror :change-class :char :char-code :char-code-limit :char-downcase
   :char-equal :char-greaterp :char-int :char-lessp :char-name :char-not-equal
   :char-not-greaterp :char-not-lessp :char-upcase :char/= :char< :char<= :char=
   :char> :char>= :character :characterp :check-type :cis :class-name
   :clear-input :clear-output :close :clrhash :code-char :coerce
   :compilation-speed :compile :compile-file :compile-file-pathname
   :compiled-function :compiled-function-p :compiler-macro
   :compiler-macro-function :complement :complex :complexp
   :compute-applicable-methods :compute-restarts :concatenate
   :concatenated-stream :concatenated-stream-streams :cond :condition :conjugate
   :cons :consp :constantly :constantp :continue :control-error :copy-alist
   :copy-list :copy-pprint-dispatch :copy-readtable :copy-seq :copy-structure
   :copy-symbol :copy-tree :cos :cosh :count :count-if :count-if-not :ctypecase
   :debug :decf :declaim :declaration :declare :decode-float
   :decode-universal-time :defconstant :defgeneric
   :define-compiler-macro :define-condition :define-method-combination
   :define-modify-macro :define-setf-expander :define-symbol-macro :defmacro
   :defmethod :defpackage :defparameter :defsetf :defstruct :deftype :defun
   :defvar :delete :delete-duplicates :delete-file :delete-if :delete-if-not
   :delete-package :denominator :deposit-field :describe :describe-object
   :destructuring-bind :digit-char :digit-char-p :directory :directory-namestring
   :disassemble :division-by-zero :do :do* :do-all-symbols :do-external-symbols
   :do-symbols :documentation :dolist :dotimes :double-float
   :double-float-epsilon :double-float-negative-epsilon :dpb :dribble
   :dynamic-extent :ecase :echo-stream :echo-stream-input-stream
   :echo-stream-output-stream :ed :eighth :elt :encode-universal-time
   :end-of-file :endp :enough-namestring :ensure-directories-exist
   :ensure-generic-function :eq :eql :equal :equalp :error :etypecase :eval
   :eval-when :evenp :every :exp :export :expt :extended-char :fboundp :fceiling
   :fdefinition :ffloor :fifth :file-author :file-error :file-error-pathname
   :file-length :file-namestring :file-position :file-stream :file-string-length
   :file-write-date :fill :fill-pointer :find :find-all-symbols 
   :find-if :find-if-not :find-method :find-package :find-restart :find-symbol
   :finish-output :first :fixnum :flet :float :float-digits :float-precision
   :float-radix :float-sign :floating-point-inexact
   :floating-point-invalid-operation :floating-point-overflow
   :floating-point-underflow :floatp :floor :fmakunbound :force-output :format
   :formatter :fourth :fresh-line :fround :ftruncate :ftype :funcall :function
   :function-keywords :function-lambda-expression :functionp :gcd
   :generic-function :gensym :gentemp :get :get-decoded-time
   :get-dispatch-macro-character :get-internal-real-time :get-internal-run-time
   :get-macro-character :get-output-stream-string :get-properties
   :get-setf-expansion :get-universal-time :getf :gethash :go :graphic-char-p
   :handler-bind :handler-case :hash-table :hash-table-count :hash-table-p
   :hash-table-rehash-size :hash-table-rehash-threshold :hash-table-size
   :hash-table-test :host-namestring :identity :if :ignorable :ignore
   :ignore-errors :imagpart :import :in-package :incf :initialize-instance
   :inline :input-stream-p :inspect :integer :integer-decode-float
   :integer-length :integerp :interactive-stream-p :intern
   :internal-time-units-per-second :intersection :invalid-method-error
   :invoke-debugger :invoke-restart :invoke-restart-interactively :isqrt :keyword
   :keywordp :labels :lambda :lambda-list-keywords :lambda-parameters-limit :last
   :lcm :ldb :ldb-test :ldiff :least-negative-double-float
   :least-negative-long-float :least-negative-normalized-double-float
   :least-negative-normalized-long-float :least-negative-normalized-short-float
   :least-negative-normalized-single-float :least-negative-short-float
   :least-negative-single-float :least-positive-double-float
   :least-positive-long-float :least-positive-normalized-double-float
   :least-positive-normalized-long-float :least-positive-normalized-short-float
   :least-positive-normalized-single-float :least-positive-short-float
   :least-positive-single-float :length :let :let* :lisp-implementation-type
   :lisp-implementation-version :list :list* :list-all-packages :list-length
   :listen :listp :load :load-logical-pathname-translations :load-time-value
   :locally :log :logand :logandc1 :logandc2 :logbitp :logcount :logeqv
   :logical-pathname :logical-pathname-translations :logior :lognand :lognor
   :lognot :logorc1 :logorc2 :logtest :logxor :long-float :long-float-epsilon
   :long-float-negative-epsilon :long-site-name :loop :loop-finish :lower-case-p
   :machine-instance :machine-type :machine-version :macro-function :macroexpand
   :macroexpand-1 :macrolet :make-array :make-broadcast-stream
   :make-concatenated-stream :make-condition :make-dispatch-macro-character
   :make-echo-stream :make-hash-table :make-instances-obsolete
   :make-list :make-load-form :make-load-form-saving-slots 
   :make-package :make-pathname :make-random-state :make-sequence :make-string
   :make-string-input-stream :make-string-output-stream :make-symbol
   :make-synonym-stream :make-two-way-stream :makunbound :map :map-into :mapc
   :mapcan :mapcar :mapcon :maphash :mapl :maplist :mask-field :max :member
   :member-if :member-if-not :merge :merge-pathnames :method :method-combination
   :method-combination-error :method-qualifiers :min :minusp :mismatch :mod
   :most-negative-double-float :most-negative-fixnum :most-negative-long-float
   :most-negative-short-float :most-negative-single-float
   :most-positive-double-float :most-positive-fixnum :most-positive-long-float
   :most-positive-short-float :most-positive-single-float :muffle-warning
   :multiple-value-bind :multiple-value-call :multiple-value-list
   :multiple-value-prog1 :multiple-value-setq :multiple-values-limit :name-char
   :namestring :nbutlast :nconc :next-method-p :nil :nintersection :ninth
   :no-applicable-method :no-next-method :not :notany :notevery :notinline
   :nreconc :nreverse :nset-difference :nset-exclusive-or :nstring-capitalize
   :nstring-downcase :nstring-upcase :nsublis :nsubst :nsubst-if :nsubst-if-not
   :nsubstitute :nsubstitute-if :nsubstitute-if-not :nth :nth-value :nthcdr :null
   :number :numberp :numerator :nunion :oddp :open :open-stream-p :optimize :or
   :otherwise :output-stream-p :package :package-error :package-error-package
   :package-name :package-nicknames :package-shadowing-symbols :package-use-list
   :package-used-by-list :packagep :pairlis :parse-error :parse-integer
   :parse-namestring :pathname :pathname-device :pathname-directory
   :pathname-host :pathname-match-p :pathname-name :pathname-type
   :pathname-version :pathnamep :peek-char :phase :pi :plusp :pop :position
   :position-if :position-if-not :pprint :pprint-dispatch
   :pprint-exit-if-list-exhausted :pprint-fill :pprint-indent :pprint-linear
   :pprint-logical-block :pprint-newline :pprint-pop :pprint-tab :pprint-tabular
   :prin1 :prin1-to-string :princ :princ-to-string :print :print-not-readable
   :print-not-readable-object :print-object :print-unreadable-object :probe-file
   :proclaim :prog :prog* :prog1 :prog2 :progn :program-error :progv :provide
   :psetf :psetq :push :pushnew :quote :random :random-state :random-state-p
   :rassoc :rassoc-if :rassoc-if-not :ratio :rational :rationalize :rationalp
   :read :read-byte :read-char :read-char-no-hang :read-delimited-list
   :read-from-string :read-line :read-preserving-whitespace :read-sequence
   :reader-error :readtable :readtable-case :readtablep :real :realp :realpart
   :reduce :reinitialize-instance :rem :remf :remhash :remove :remove-duplicates
   :remove-if :remove-if-not :remove-method :remprop :rename-file :rename-package
   :replace :require :rest :restart :restart-bind :restart-case :restart-name
   :return :return-from :revappend :reverse :room :rotatef :round :row-major-aref
   :rplaca :rplacd :safety :satisfies :sbit :scale-float :schar :search :second
   :sequence :serious-condition :set :set-difference
   :set-dispatch-macro-character :set-exclusive-or :set-macro-character
   :set-pprint-dispatch :set-syntax-from-char :setf :setq :seventh :shadow
   :shadowing-import :shared-initialize :shiftf :short-float :short-float-epsilon
   :short-float-negative-epsilon :short-site-name :signal :signed-byte :signum
   :simple-array :simple-base-string :simple-bit-vector :simple-bit-vector-p
   :simple-condition :simple-condition-format-arguments
   :simple-condition-format-control :simple-error :simple-string :simple-string-p
   :simple-type-error :simple-vector :simple-vector-p :simple-warning :sin
   :single-float :single-float-epsilon :single-float-negative-epsilon :sinh
   :sixth :sleep :slot-boundp :slot-exists-p :slot-makunbound :slot-missing
   :slot-unbound :software-type :software-version :some :sort :space
   :special :special-operator-p :speed :sqrt :stable-sort :standard
   :standard-char :standard-char-p :standard-class :standard-generic-function
   :standard-method :standard-object :step :storage-condition :store-value
   :stream :stream-element-type :stream-error :stream-error-stream
   :stream-external-format :streamp :string :string-capitalize :string-downcase
   :string-equal :string-greaterp :string-left-trim :string-lessp
   :string-not-equal :string-not-greaterp :string-not-lessp :string-right-trim
   :string-stream :string-trim :string-upcase :string/= :string< :string<=
   :string= :string> :string>= :stringp :structure :structure-class
   :structure-object :style-warning :sublis :subseq :subsetp :subst :subst-if
   :subst-if-not :substitute :substitute-if :substitute-if-not :subtypep :svref
   :sxhash :symbol :symbol-function :symbol-macrolet :symbol-name :symbol-package
   :symbol-plist :symbol-value :symbolp :synonym-stream :synonym-stream-symbol :t
   :tagbody :tailp :tan :tanh :tenth :terpri :the :third :throw :time :trace
   :translate-logical-pathname :translate-pathname :tree-equal :truename
   :truncate :two-way-stream :two-way-stream-input-stream
   :two-way-stream-output-stream :type :type-error :type-error-datum
   :type-error-expected-type :type-of :typecase :typep :unbound-slot
   :unbound-slot-instance :unbound-variable :undefined-function :unexport
   :unintern :union :unless :unread-char :unsigned-byte :untrace :unuse-package
   :unwind-protect :update-instance-for-different-class
   :update-instance-for-redefined-class :upgraded-array-element-type
   :upgraded-complex-part-type :upper-case-p :use-package :use-value
   :user-homedir-pathname :values :values-list :variable :vector :vector-pop
   :vector-push :vector-push-extend :vectorp :warn :warning :when
   :wild-pathname-p :with-accessors :with-compilation-unit
   :with-condition-restarts :with-hash-table-iterator :with-input-from-string
   :with-open-file :with-open-stream :with-output-to-string
   :with-package-iterator :with-simple-restart :with-slots
   :with-standard-io-syntax :write :write-byte :write-char :write-line
   :write-sequence :write-string :write-to-string :y-or-n-p :yes-or-no-p :zerop))


;;; eof


