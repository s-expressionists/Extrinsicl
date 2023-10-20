(in-package #:extrinsicl)

(defparameter *simple-function-aliases*
  '(;; 3 Evaluation and Compilation
    ;; 4 Types and Classes
    type-of type-error-datum type-error-expected-type
    ;; 5 Data and Control Flow
    function-lambda-expression functionp compiled-function-p
    not eq eql equal equalp identity complement constantly
    values values-list
    ;; 6 Iteration
    ;; 7 Objects
    function-keywords allocate-instance reinitialize-instance shared-initialize
    update-instance-for-different-class update-instance-for-redefined-class
    slot-boundp slot-exists-p slot-makunbound slot-missing slot-unbound slot-value
    method-qualifiers no-applicable-method no-next-method remove-method
    compute-applicable-methods
    find-method add-method initialize-instance class-name (setf class-name)
    class-of unbound-slot-instance
    ;; 8 Structures
    copy-structure
    ;; 9 Conditions
    cell-error-name invalid-method-error method-combination-error
    simple-condition-format-control simple-condition-format-arguments
    break compute-restarts find-restart invoke-restart invoke-restart-interactively
    restart-name abort continue muffle-warning store-value use-value
    ;; 10 Symbols
    symbolp keywordp make-symbol gentemp symbol-name symbol-package
    ;; 11 Packages
    ;; many of the functions not listed here implicitly read *package*
    package-shadowing-symbols
    package-use-list package-used-by-list packagep package-error-package
    ;; 12 Numbers
    = /= < > <= >= max min minusp plusp zerop floor ffloor ceiling fceiling
    truncate ftruncate round fround sin cos tan asin acos atan
    sinh cosh tanh asinh acosh atanh * + - / 1+ 1- abs evenp oddp exp expt gcd lcm
    log mod rem signum sqrt isqrt random-state-p numberp cis complex complexp
    conjugate phase realpart imagpart
    realp numerator denominator rational rationalize
    rationalp ash integer-length integerp parse-integer boole
    logand logandc1 logandc2 logeqv logior lognand lognor lognot logorc1 logorc2 logxor
    logbitp logcount logtest byte byte-size byte-position deposit-field dpb ldb ldb-test
    mask-field decode-float scale-float float-radix float-sign float-digits
    float-precision integer-decode-float float floatp
    arithmetic-error-operation arithmetic-error-operands
    ;; 13 Characters
    char= char/= char< char> char<= char>= char-equal char-not-equal
    char-lessp char-greaterp char-not-greaterp char-not-lessp character characterp
    alpha-char-p alphanumericp digit-char digit-char-p graphic-char-p standard-char-p
    char-upcase char-downcase upper-case-p lower-case-p both-case-p char-code char-int
    code-char char-name name-char
    ;; 14 Conses
    cons consp atom rplaca rplacd
    car cdr caar cadr cdar cddr caaar caadr cadar cadar caddr cdaar cdadr cddar cdddr
    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr cdadar
    cdaddr cddaar cddadr cdddar cddddr
    copy-tree copy-list list list* list-length listp make-list
    first second third fourth fifth sixth seventh eighth ninth tenth
    nth endp null nconc append revappend nreconc butlast nbutlast last ldiff tailp
    nthcdr rest acons copy-alist pairlis get-properties getf
    ;; 15 Arrays
    adjustable-array-p aref array-dimension array-dimensions array-element-type
    array-has-fill-pointer-p array-displacement array-in-bounds-p array-rank
    array-row-major-index array-total-size arrayp fill-pointer row-major-aref
    simple-vector-p svref vector vector-pop vector-push vector-push-extend
    vectorp bit sbit bit-and bit-andc1 bit-andc2 bit-eqv bit-ior bit-nand bit-nor
    bit-not bit-orc1 bit-orc2 bit-xor bit-vector-p simple-bit-vector-p
    ;; 16 Strings
    simple-string-p char schar string string-upcase string-downcase
    string-capitalize nstring-upcase nstring-downcase nstring-capitalize
    string-trim string-left-trim string-right-trim string= string/=
    string< string> string<= string>= string-equal string-not-equal
    string-lessp string-greaterp string-not-greaterp string-not-lessp stringp
    ;; 17 Sequences
    copy-seq elt fill subseq length reverse nreverse replace
    ;; 18 Hash Tables
    make-hash-table ; assuming environment eq is host eq, etc.
    hash-table-p hash-table-count hash-table-rehash-size hash-table-rehash-threshold
    hash-table-size hash-table-test gethash remhash clrhash sxhash
    ;; 19 Filenames
    pathname pathnamep pathname-host pathname-device pathname-directory
    pathname-name pathname-type pathname-version
    load-logical-pathname-translations logical-pathname-translations
    logical-pathname namestring file-namestring directory-namestring host-namestring
    wild-pathname-p pathname-match-p translate-logical-pathname translate-pathname
    ;; 20 Files
    directory probe-file ensure-directories-exist truename file-author
    file-write-date rename-file delete-file file-error-pathname
    ;; 21 Streams
    input-stream-p output-stream-p interactive-stream-p open-stream-p
    stream-element-type streamp read-byte write-byte read-sequence write-sequence
    file-length file-position file-string-length stream-external-format close
    synonym-stream-symbol broadcast-stream-streams make-broadcast-stream
    make-two-way-stream two-way-stream-input-stream two-way-stream-output-stream
    two-way-stream-input-stream two-way-stream-output-stream
    echo-stream-input-stream echo-stream-output-stream make-echo-stream
    concatenated-stream-streams make-concatenated-stream get-output-stream-string
    make-string-input-stream make-string-output-stream stream-error-stream
    ;; 22 Printer
    ;; we include print-object on the theory that it's not called by the user;
    ;; as such it's only called from within the normal print functions, which have
    ;; taken care of binding the printer variables appropriately.
    print-object print-not-readable-object
    ;; 23 Reader
    readtable-case readtablep
    ;; 24 System Construction
    ;; 25 Environment
    decode-universal-time encode-universal-time get-universal-time get-decoded-time
    sleep describe-object get-internal-real-time get-internal-run-time room
    dribble lisp-implementation-type lisp-implementation-version
    short-site-name long-site-name machine-instance machine-type machine-version
    software-type software-version user-homedir-pathname))

(defun install-simple-function-aliases (client environment)
  (loop for name in *simple-function-aliases*
        for f = (fdefinition name)
        do (setf (clostrum:fdefinition client environment name) f)))

(defparameter *simple-constant-aliases*
  '(;; 3 Evaluation and Compilation
    ;; 4 Types and Classes
    ;; 5 Data and Control Flow
    nil t
    ;; 6 Iteration, 7 Objects, 8 Structures, 9 Conditions, 10 Symbols, 11 Packages
    ;; 12 Numbers
    pi boole-1 boole-2 boole-and boole-andc1 boole-andc2 boole-c1 boole-c2 boole-clr
    boole-eqv boole-ior boole-nand boole-nor boole-orc1 boole-orc2 boole-set boole-xor
    most-positive-fixnum most-negative-fixnum most-positive-short-float
    least-positive-short-float least-positive-normalized-short-float
    most-positive-double-float least-positive-double-float
    least-positive-normalized-double-float most-positive-long-float least-positive-long-float
    least-positive-normalized-long-float most-positive-single-float
    least-positive-single-float least-positive-normalized-single-float
    most-negative-short-float least-negative-short-float
    least-negative-normalized-short-float most-negative-double-float
    least-negative-double-float least-negative-normalized-double-float
    most-negative-long-float least-negative-long-float
    least-negative-normalized-long-float most-negative-single-float
    least-negative-single-float least-negative-normalized-single-float
    short-float-epsilon short-float-negative-epsilon single-float-epsilon
    single-float-negative-epsilon double-float-epsilon
    double-float-negative-epsilon long-float-epsilon long-float-negative-epsilon
    ;; 13 Characters
    char-code-limit
    ;; 14 Conses
    ;; 15 Arrays
    array-dimension-limit array-rank-limit array-total-size-limit
    ;; 25 Environment
    internal-time-units-per-second
    ))

(defun install-simple-constant-aliases (client environment)
  (loop for name in *simple-constant-aliases*
        for v = (symbol-value name)
        do (clostrum:make-constant client environment name v)))

(defparameter *simple-class-aliases*
  '(;; 3 Evaluation and Compilation
    ;; 4 Types and Classes
    function generic-function standard-generic-function class built-in-class
    structure-class standard-class method standard-method structure-object
    standard-object method-combination t type-error simple-type-error
    ;; 5 Data and Control Flow
    control-error program-error undefined-function
    ;; 6 Iteration
    ;; 7 Objects
    unbound-slot
    ;; 8 Structures
    ;; 9 Conditions
    condition warning style-warning serious-condition error cell-error
    parse-error storage-condition simple-error simple-condition simple-warning
    restart
    ;; 10 Symbols
    symbol unbound-variable
    ;; 11 Packages
    package package-error
    ;; 12 Numbers
    number complex real float rational ratio integer random-state
    arithmetic-error division-by-zero floating-point-invalid-operation
    floating-point-inexact floating-point-overflow floating-point-underflow
    ;; 13 Characters
    character
    ;; 14 Conses
    list null cons
    ;; 15 Arrays
    array vector bit-vector
    ;; 16 Strings
    string
    ;; 17 Sequences
    sequence
    ;; 18 Hash Tables
    hash-table
    ;; 19 Filenames
    pathname logical-pathname
    ;; 20 Files
    file-error
    ;; 21 Streams
    stream broadcast-stream concatenated-stream echo-stream file-stream
    string-stream synonym-stream two-way-stream stream-error end-of-file
    ;; 22 Printer
    print-not-readable
    ;; 23 Reader
    readtable reader-error
    ;; 24 System Construction, Environment
    ))

(defun install-simple-class-aliases (client environment)
  (loop for name in *simple-class-aliases*
        for class = (find-class name)
        do (setf (clostrum:find-class client environment name) class)))
