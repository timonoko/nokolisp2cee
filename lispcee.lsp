
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (22 / 6 - 1994) (9 : 41 : 59 21))
(defq *package* LISPCEE)

(defun {-numbs-thnx
 (n args l)
 (` progn
  (print ', n)
  (printc 40)
  ,
  ({-want-number (pop args))
  ,@
  (map args
   (function
    (lambda
     (x)
     (list 'progn '(print ',) ({-want-number x)))))
  (printc 41)))

(defun {-all-numbers
 (x)
 (if
  (atom x)
  (or (null x) (numberp x))
  (and ({-all-numbers (car x)) ({-all-numbers (cdr x)))))

(defun {-case-1
 ({-x)
 (` progn
  ({-tabi)
  (print 'case)
  (sp)
  ,
  ({-want-number {-x)
  (print ':)))

(defun {-Clear-PF
 (x)
 (print ({-validna x))
 (when
  (member x {-PASKA-F:T)
  (setq {-PASKA-F:T (del-member x {-PASKA-F:T))
  (setq {-FUNCS (del-member (assoc x {-FUNCS) {-FUNCS))))

(defun {-any-ref-in
 (x y)
 (cond
  ((identp y) (eq x y))
  ((atom y) nil)
  ((or ({-any-ref-in x (car y)) ({-any-ref-in x (cdr y))))))

(defun {-loc-gensym ()
 (compress
  (nconc
   (explode '__)
   (explode (setq {-loc-seed (add1 (or {-loc-seed 0)))))))

(defq {-MAX-LINES-IN-C-FILE 2000)

(defun {-flat-arglist
 ({-x)
 (map {-x
  (function (lambda (x) (if (atom x) x (cadr x))))))

(defun {-ext-func
 (x)
 (print 'extern)
 (sp)
 (print
  (case
   (caddr x)
   (integer 'int)
   (boolean 'char)
   (t '_node)))
 (sp)
 (print ({-validna (car x)))
 (printc 40)
 ({-nodelist (reverse (cadr x)) t)
 (printc 41)
 (print ';)
 (cr))

(defun {-func-dec ()
 (mapc {-FUNCS
  (function
   (lambda ({-x) (unless (assoc (car {-x) {-INITFUNCS) ({-ext-func {-x))))))

(defun {-includet ()
 (#include 'lispinit.h)
 (#include {-INCLUDE-FILE)
 (progn (cr) (prints '#ifndef 'vax))
 (#include 'lispinit.dec)
 ({-func-dec)
 (progn (cr) (print '#endif)))

(defun #include
 (x)
 (cr)
 (print '#include)
 (sp)
 (print '")
 (print x)
 (print '")
 (cr))

(defq LISPCEE
 ({-numbs-thnx {-all-numbers {-case-1 {-Clear-PF {-any-ref-in {-loc-gensym
  {-MAX-LINES-IN-C-FILE {-flat-arglist {-ext-func {-func-dec
  {-includet #include LISPCEE definition-of koe koe-1 koe-2
  koe3 lispcee roska {-MACRO {-arith {-autost {-cdr-assoc {-chain-retuexpr
  {-compare {-compope {-eka {-funcall {-function {-gensym {-if
  {-in-the-middle {-include-file {-init {-init-const {-let
  {-let-functions {-lispinit {-localvar {-macrofy {-main
  {-make-const {-maybetab {-more-init {-nodelist {-numope {-pi-list
  {-print-; {-print-} {-progn {-quote {-repeat {-repeat-times {-return
  {-save-definition {-setq {-strai-arg {-strai-arg2 {-tab
  {-tabi {-tnil {-typed {-valid-str {-validna {-want-boolean {-want-number
  {-while {_makenumber))

(defun definition-of
 (x)
 (let
  ((y (assoc x *COMPILED-FUNCTIONS*)))
  (if y
   (if
    (eq (cadr y) '*IN-FILE*)
    (let
     ((file (open (caddr y))) (inp (in)))
     (LSEEK file 0 (cadddr y))
     (in file)
     (prog1 (read) (close file) (in inp)))
    (cadr y))
   (eval x))))

(defun koe (x y) (repeat (case x (1 2))))

(defun koe-1
 (x)
 (cons 1
  (let
   ((v 1))
   ((function (lambda (y) y)) x)
   (koe-2 x))))

(defun koe-2 (x) (koe-1 x))

(defun koe3
 (z x y)
 (defun koe4 (x . y) (setq z (car y)))
 (koe4 1 2 3 4)
 (car z))

(defun lispcee
 ({-x {-OUTFILE {-NO-DTA-FILE)
 (unless
  (atom lispcee)
  (compile-all 'lispcee)
  (when {-OUTFILE
   ({-save-definition 'DISKDEFI.$$$)
   (setq BOOT nil)
   (setq LISPCEE nil)))
 (if {-OUTFILE
  (progn
   (setq {-FBODY (explode {-OUTFILE))
   (if
    (member 46 {-FBODY)
    (setq {-FBODY (reverse (cdr (member 46 {-FBODY)))))
   (setq {-INCLUDE-FILE (compress (append {-FBODY (explode '.inc))))
   (setq {-DATA-FILE (compress (append {-FBODY (explode '.dta))))
   (setq {-OUTFILE (compress (append {-FBODY (explode '.c))))
   (out (setq {-OUTFILE (create {-OUTFILE))))
  (progn (setq {-DATA-FILE 'dta.dta) (setq {-INCLUDE-FILE 'inc.inc)))
 ({-init)
 ({-includet)
 (unless
  (symbolp {-x)
  (setq {-x (reverse {-x))
  (while
   (cdr {-x)
   (eval ({-function (car {-x) (definition-of (pop {-x)))))
  (setq {-x (car {-x)))
 (eval ({-function {-x (definition-of {-x)))
 (cr)
 ({-init-const)
 ({-main {-x)
 (when {-OUTFILE (close {-OUTFILE) (out 0) ({-include-file {-INCLUDE-FILE))
 {-ERRORS)

(defun roska
 (x)
 (cond
  ((setq temp
    (or (member 'DATA x) (member 'INITIAL x)))
   temp)
  (t x)))

(defq {-MACRO
 ((list
   (x . y)
   (if y
    (` cons , x (list ,@ y))
    (` cons , x nil)))
  (zerop (x) (` eqn 0 , x))
  (compile () nil)
  (mapc
   (x y)
   (if
    (eq (car y) 'quote)
    (rplaca y 'function))
   (` Mapc , y , x))
  (map
   (x y)
   (if
    (eq (car y) 'quote)
    (rplaca y 'function))
   (` mapcar , y , x))
  (cdxr (x y) (` nthcdr , x , y))
  (cr () '(printc 13))
  (sp () '(printc 32))
  (not
   (x)
   (if
    (member (car x) '(not null))
    (cadr x)
    (` eq , x ())))
  (null (x) (` not , x))
  (cadddr (x) (` car (cdddr , x)))
  (cdddr (x) (` cdr (cdr (cdr , x))))
  (cddr (x) (` cdr (cdr , x)))
  (caddr (x) (` car (cddr , x)))
  (cadr (x) (` car (cdr , x)))
  (cdar (x) (` cdr (car , x)))
  (caar (x) (` car (car , x)))
  (prog1 (x . y) (` let ((_wp , x)) ,@ y _wp))
  (sub1 (x) (` - , x 1))
  (1- (x) (` - , x 1))
  (1+ (x) (` + , x 1))
  (add1 (x) (` + , x 1))
  (and
   (x . y)
   (if y (` if , x (and ,@ y) ()) x))
  (or
   (x . y)
   (if y
    (if
     (atom x)
     (` if , x , x (or ,@ y))
     (` let
      ((_waste_bas , x))
      (if _waste_bas _waste_bas (or ,@ y))))
    x))
  (cond x
   (if x
    (if
     (eq (caar x) 't)
     (` progn ,@ (cdar x))
     (if
      (cdar x)
      (` if ,
       (caar x)
       (progn ,@ (cdar x))
       (cond ,@ (cdr x)))
      (` or , (caar x) (cond ,@ (cdr x)))))
    ()))
  (repeat-until (x . y) (` while (not , x) ,@ y))
  (echo (x) (` setq _echo , x))))

(defun {-arith
 ((x y z) {-retuexpr)
 (` progn
  (printc 40)
  ,
  ({-want-number y)
  (print ', ({-numope x))
  ({-maybetab)
  ,
  ({-want-number z)
  (printc 41)))

(defun {-autost ()
 (mapc *COMPILED-FUNCTIONS*
  (function
   (lambda (x) (rplaca (cdr x) (definition-of (car x))))))
 (setq autost {-autost2)
 (autost))

(defun {-cdr-assoc
 (x y)
 (while
  (and y (not (equal x (cdar y))))
  (pop y))
 y)

(defun {-chain-retuexpr
 ({-x)
 (if {-retuexpr
  (prog1 (append {-retuexpr (list {-x)) (setq {-retuexpr nil))
  (list {-x)))

(defun {-compare
 ((x y z) {-retuexpr)
 (` progn ,
  ({-want-number y)
  (print ', ({-compope x))
  ,
  ({-want-number z)))

(defun {-compope
 (x)
 (case x
  (lessp '<)
  (greaterp '>)
  (eqn '==)
  (t nil)))

(defun {-eka
 ({-x {-retuexpr {-WANT-EXPRESSION)
 (setq {-x ({-typed {-x))
 (setq {-x
  (case
   (car {-x)
   (integer ({_makenumber (cadr {-x)))
   (boolean ({-tnil (cadr {-x)))
   (t {-x)))
 (setq {-x
  (if {-retuexpr
   (if
    (atom (car {-retuexpr))
    ({-retuexpr {-x)
    (progn
     (dolist ({-f (reverse {-retuexpr)) (setq {-x ({-f {-x)))
     {-x))
   {-x)))

(defun {-funcall
 ({-x {-args)
 (if
  (and (atom (car {-args)) (null (cdr {-args)) (cadr {-x))
  ({-eka
   (cadr {-x)
   ({-chain-retuexpr
    (` lambda
     (x)
     (list
      'progn
      '({-Clear-PF ', ({-validna (car {-x)))
      '(printc 40)
      x
      '(printc 41))))
   {-WANT-EXPRESSION)
  (` progn
   ({-Clear-PF ', (pop {-x))
   (printc 40)
   ,@
   (let
    (({-tab (if {-tab (1+ {-tab) 1)) (tulos))
    (while {-args
     (push
      (cond
       ((eq (caar {-args) '*)
        (if
         (= (getchar ({-localvar (cadr (car {-args))) 1) (char *))
         (` print ', ({-validna (cadr (car {-args))))
         (` progn
          (print '&)
          (print ', ({-localvar (cadr (car {-args)))))))
       ((eq (caar {-args) '/*list*/)
        (prog1
         ({-eka (cons 'list {-x) nil '{-WANT-EXPRESSION)
         (setq {-x nil)))
       ({-x ({-eka (pop {-x) nil '{-WANT-EXPRESSION))
       (t '(print 'NIL)))
      tulos)
     (pop {-args)
     (if {-args (push '(progn (printc 44) ({-maybetab)) tulos)))
    tulos)
   (printc 41))))

(defun {-function
 ({-name {-x)
 (when
  (and {-OUTFILE (> {-LINES {-MAX-LINES-IN-C-FILE))
  (setq {-LINES 0)
  (cr)
  (close {-OUTFILE)
  (rplaca (last {-FBODY) (1+ (car (last {-FBODY))))
  (out
   (setq {-OUTFILE (create (compress (append {-FBODY (explode '.c))))))
  (cr)
  ({-includet))
 (unless
  (eq (car {-x) 'lambda)
  (setq {-x
   (if
    (eq (car {-x) 'program)
    (` lambda ()
     ((function (lambda ,@ (cdr {-x)))
      ,@
      (map
       (cadr {-x)
       (function
        (lambda
         (x)
         (` progn
          (cr)
          (print ', x)
          (print '?:)
          (read)))))))
    (list 'lambda () {-x))))
 (let
  (({-temp))
  (setq {-x
   (` lambda ,
    ({-strai-arg (cadr {-x))
    ,@
    (if {-temp
     (` (let , {-temp ,@ (cddr {-x)))
     (cddr {-x)))))
 (let*
  (({-loc-seed {-loc-seed)
   ({-IN-THE-MIDDLE (cons {-name {-IN-THE-MIDDLE))
   ({-orig-args
    (let
     (({-x2 (cadr {-x)) ({-y))
     (while {-x2
      (cond
       ((atom {-x2)
        (push (list '/*list*/ {-x2) {-y)
        (setq {-x2 nil))
       ((atom (car {-x2)) (push (pop {-x2) {-y))
       (t
        (pop {-x2)
        (push (` ** BAD LAMBDA , (cadr {-x) **) {-ERRORS)
        (push (gensym 'arg) {-y))))
     (reverse {-y)))
   ({-flat-args ({-flat-arglist {-orig-args))
   ({-lo-var
    (let
     ((tulos nil) (except {-flat-args))
     (mapc {-LOCALVAR
      (function
       (lambda
        (x)
        (cond
         ((member (car x) except))
         ((member (car x) tulos))
         ((or {-LOCAL-FUNCS ({-any-ref-in (car x) (cddr {-x)))
          (push (car x) tulos))))))
     tulos))
   ({-LOCALVAR)
   ({-all-args))
  (mapc {-flat-args (function (lambda ({-x) (push (list {-x) {-LOCALVAR))))
  (mapc {-orig-args (function (lambda ({-x) (push {-x {-all-args))))
  (mapc {-lo-var
   (function
    (lambda
     ({-x)
     (push (list '* {-x) {-all-args)
     (push (cons {-x (list '* {-x)) {-LOCALVAR))))
  (push (list {-name (reverse {-all-args)) {-FUNCS)
  (` let
   (({-tab (1+ {-tab)))
   (cr)
   ({-tabi)
   (print '_node)
   (sp)
   (unless (zerop (out)) (echo t))
   (print ', ({-validna {-name))
   ({-pi-list ', (reverse (append {-flat-args {-lo-var)))
   ({-tabi)
   (echo nil)
   ({-nodelist ', (map (append {-flat-args {-lo-var) {-localvar))
   ,
   (let
    (({-temp))
    ({-in-the-middle (cddr {-x))
    (if {-temp
     (` let
      (({-temp ', {-temp))
      (print '{)
      (while {-temp
       (cr)
       (tab 8)
       (print '_node)
       (sp)
       (print ({-validna (pop {-temp)))
       (print ())
       (print ';))
      (cr)
      ,
      ({-return (` progn , nil ,@ (cddr {-x)))
      ({-print-}))
     ({-return (` progn , nil ,@ (cddr {-x))))))))

(defun {-gensym
 (x)
 (unless x (setq x '_X_))
 (compress
  (nconc
   (explode '_)
   (nconc (explode x) (explode (setq {-gensym-seed (add1 {-gensym-seed)))))))

(defun {-if
 ({-x {-y {-z)
 (if
  (member (car {-x) '(not null))
  ({-if (cadr {-x) {-z {-y)
  (prog1
   (` let
    (({-tab (1+ {-tab)))
    (print 'if)
    (printc 40)
    ,
    (let (({-retuexpr)) ({-want-boolean {-x))
    (printc 41)
    ({-tabi)
    (setq {-last-} nil)
    ,
    ({-eka {-y {-retuexpr)
    ,@
    (if
     (or {-z {-retuexpr)
     (`
      (setq {-tab (1- {-tab))
      (if {-last-} (setq {-last-} nil) ({-print-;))
      ({-tabi)
      (print 'else)
      (sp)
      ,
      ({-eka {-z {-retuexpr))))
   (setq {-retuexpr nil))))

(defun {-in-the-middle
 (x)
 (if
  (atom x)
  (if
   (member x (cdr {-IN-THE-MIDDLE))
   (push-once x {-temp)
   (let
    ((temp (assoc x {-LOCAL-FUNCS)))
    (if temp ({-in-the-middle (cdr temp)))))
  (progn ({-in-the-middle (car x)) ({-in-the-middle (cdr x)))))

(defun {-include-file
 (file)
 (if file (out (create file)))
 (if {-GLOBALVAR ({-nodelist {-GLOBALVAR))
 (cr)
 (print '_node)
 (tab 8)
 (print '_const_)
 (print '[)
 (print (1+ (length {-CONST)))
 (print '])
 (print ';)
 (cr)
 (when file (close (out)) (out 0)))

(defun {-init ()
 (setq {-LINES 0)
 (setq {-ERRORS nil)
 (setq {-GLOBINIT '(_echo nxtch _waste_bas))
 (setq {-GLOBALVAR nil)
 (setq {-CONST nil)
 (setq {-gensym-seed 1)
 (setq {-tab 1)
 (setq {-VALIDNA
  (quote
   ((getchar . _getchar)
    (plm-item . plmitem)
    (read . _lisp_read)
    (length . _list_length)
    (open . _open_file)
    (create . _create_file)
    (close . _close_file)
    (in . _in_file)
    (dispose . _dispose))))
 (setq {-INITFUNCS
  (quote
   ((nth (x y))
    (nthcdr (x y))
    (dispose (x))
    (out (x))
    (in (x))
    (plm-item ())
    (tab (x))
    (compress (x))
    (explode (x))
    (getchar (x y))
    (close (x))
    (create (x))
    (open (x))
    (subst (x y z))
    (delete (x y))
    (length (x) integer)
    (nconc (x y))
    (last (x))
    (append (x y))
    (reverse (x y))
    (print (x))
    (assoc (x y))
    (member (x y))
    (equal (x y))
    (cons (x y))
    (car (x))
    (cdr (x))
    (atom (x) boolean)
    (numberp (x) boolean)
    (identp (x) boolean)
    (rplaca (x y))
    (rplacd (x y))
    (read ())
    (readc () integer))))
 (setq {-FUNCS {-INITFUNCS))

(defun {-init-const ()
 (prints '_initconst ())
 (cr)
 (sp)
 (print '{)
 (progn
  (prints '_initint () ';)
  (cr)
  (unless {-NO-DTA-FILE
   (print '_in_file)
   (printc 40)
   (print '_open_file)
   (printc 40)
   ({-make-const {-DATA-FILE)
   (printc 41)
   (printc 41)
   ({-print-;)
   (cr)))
 (if (and {-OUTFILE (not {-NO-DTA-FILE)) (setq {-DATA-FILE (create {-DATA-FILE)))
 (cr)
 (let
  ((x (reverse {-CONST)) (n 1))
  (while x
   (sp)
   (print '_const_)
   (print '[)
   (print n)
   (print '])
   (print '=)
   ({-make-const (car x))
   (pop x)
   ({-print-;)
   (cr)
   (setq n (add1 n))))
 (let
  (({-x {-GLOBALVAR))
  (while {-x
   (sp)
   (print ({-validna (car {-x)))
   (print '=)
   ({-make-const (eval (car {-x)))
   (pop {-x)
   ({-print-;)
   (cr)))
 (unless {-NO-DTA-FILE
  (({-eka '(progn (close (in)) (in 0)))))
 ({-print-;)
 (print '})
 (if (and {-OUTFILE (not {-NO-DTA-FILE)) (close {-DATA-FILE))
 (cr))

(defun {-let
 ({-x)
 (let*
  ((valut
    (mapcar
     (function
      (lambda (x) ({-eka (` setq __saatana , (cadr x)))))
     (car {-x)))
   ({-LOCALVAR {-LOCALVAR)
   (varit
    (mapcar
     (function
      (lambda
       ((x . rest))
       (let
        ((y (compress (nconc (explode x) (explode 'L_)))))
        (push (cons x y) {-LOCALVAR)
        ({-validna y))))
     (car {-x))))
  (` let
   (({-tab (1+ {-tab)))
   (print '{)
   (sp)
   ,
   (if varit
    (` progn
     ({-nodelist ', varit)
     ,@
     (mapcar
      (function
       (lambda
        (var val)
        (` progn , (subst '__saatana var val) ({-print-;) ({-tabi))))
      varit valut)))
   ,
   ({-progn (cdr {-x))
   ({-print-}))))

(defun {-let-functions
 ({-x)
 (let*
  (({-FLET-FUNCS)
   ({-LOCAL-FUNCS
    (nconc
     (mapcar
      (function
       (lambda
        ({-y)
        (let
         (({-nimi ({-validna ({-gensym (car {-y)))))
         (push {-nimi {-FLET-FUNCS)
         (set {-nimi (cadr {-y))
         (cons (car {-y) {-nimi))))
      (car {-x))
     {-LOCAL-FUNCS)))
  (prog1
   ({-typed (cons 'progn (cdr {-x)))
   (mapc {-FLET-FUNCS (function (lambda ({-x) (set {-x nil)))))))

(defun {-lispinit ()
 ({-init)
 (compile-all '{-eka)
 (echo t)
 (out (create 'lispinit.c))
 (#include 'lispinit.inc)
 (mapc {-more-init
  (function
   (lambda
    (x)
    (eval ({-function (car x) (cadr x)))
    (cr))))
 (cr)
 (close (out))
 (out 0)
 (echo nil))

(defun {-localvar
 (x)
 (let
  ((y (assoc x {-LOCALVAR)))
  ({-validna
   (cond
    ((cdr y) (cdr y))
    ((car y) x)
    ((member x {-GLOBINIT) x)
    ((member x {-GLOBALVAR) x)
    (t (push x {-GLOBALVAR) x)))))

(defun {-macrofy
 (lambh)
 (list
  'quote
  (cons
   (if
    (eq (car lambh) 'macro)
    'nslambda
    'nlambda)
   (cdr lambh))))

(defun {-main
 ({-x)
 (cr)
 (prints 'main '(argc , argv))
 (cr)
 (prints
  'int
  'argc
  ';
  'char
  '*argv[]
  ';)
 (cr)
 (print '{)
 (sp)
 (prints '_initconst () ';)
 (cr)
 (progn
  (print ({-validna {-x))
  (printc 40)
  (let
   ((i))
   (setq {-x (cadr (assoc {-x {-FUNCS)))
   (setq i (length {-x))
   (while {-x
    (prints
     '_arg
     (list 'argc ', 'argv ', i))
    (pop {-x)
    (setq i (1- i))
    (if {-x (print ',))))
  (printc 41))
 ({-print-;)
 (cr)
 (prints 'return '(0) '; '})
 (cr))

(defun {-make-const
 (x)
 (cond
  ((null x) (print 'NIL))
  ((eq x quote) ({-make-const 'QUOTE))
  ((eq x t) ({-make-const 'T))
  ((symbolp x)
   (print '_makestring)
   (printc 40)
   (print '")
   ({-valid-str x)
   (print '")
   (printc 41))
  ((numberp x)
   (print '_makenumber)
   (printc 40)
   (print x)
   (printc 41))
  ((atom x) (print 'NIL/*?*/))
  ({-NO-DTA-FILE
   ({-maybetab)
   (print 'cons)
   (printc 40)
   ({-make-const (cdr x))
   (print ',)
   ({-maybetab)
   ({-make-const (car x))
   (printc 41))
  (t
   (when {-OUTFILE
    (let
     ((oo (out)))
     (out {-DATA-FILE)
     (cr)
     (pprint x)
     (cr)
     (out oo)))
   (print ({-validna 'read))
   (print ()))))

(defun {-maybetab ()
 (when (> (tab) 50) ({-tabi) (sp) (sp)))

(defq {-more-init
 ((equal
   (lambda
    (x y)
    (cond
     ((eq x y) t)
     ((null x) ())
     ((null y) ())
     ((numberp x) (if (numberp y) (= x y)))
     ((numberp y) ())
     ((identp x) ())
     ((identp y) ())
     (t
      (and
       (equal (car x) (car y))
       (equal (cdr x) (cdr y)))))))
  (member
   (lambda
    (x y)
    (if
     (null y)
     ()
     (if (equal x (car y)) y (member x (cdr y))))))
  (assoc
   (lambda
    (x y)
    (if
     (null y)
     ()
     (if
      (equal x (caar y))
      (car y)
      (assoc x (cdr y))))))
  (print
   (lambda
    (x)
    (cond
     ((null x) (printc 40) (printc 41))
     ((numberp x)
      ({-cee (prints '_putint '(_numval (x)))))
     ((identp x)
      ({-cee (prints '_putstr '(_strval (x)))))
     (t
      (printc 40)
      (let
       ((x x))
       (while x
        (print (pop x))
        (if x
         (if
          (atom x)
          (progn
           (printc 32)
           (printc 46)
           (printc 32)
           (print x)
           (setq x ()))
          (if
           (greaterp (tab) 70)
           (progn (cr) (tab 8))
           (sp))))))
      (printc 41)))
    x))
  (reverse
   (lambda
    (x y)
    (cond
     (x (reverse (cdr x) (cons (car x) y)))
     (t y))))
  (append
   (lambda
    (x y)
    (if x (cons (car x) (append (cdr x) y)) y)))
  (last (lambda (x) (while (cdr x) (pop x)) x))
  (nconc (lambda (x y) (rplacd (last x) y) x))
  (delete
   (lambda
    (x y)
    (cond
     ((atom y) y)
     ((null y) y)
     ((equal (car y) x) (delete x (cdr y)))
     (t
      (cons (delete x (car y)) (delete x (cdr y)))))))
  (subst
   (lambda
    (x y z)
    (cond
     ((null z) z)
     ((equal x z) y)
     ((atom z) z)
     (t
      (cons
       (subst x y (car z))
       (subst x y (cdr z)))))))
  (nth (lambda (x y) (car (nthcdr x y))))))

(defun {-nodelist
 (x no-;)
 (when x
  (let
   (({-tab (+ 5 {-tab)))
   (while x
    ({-tabi)
    (print '_node)
    (sp)
    (print ({-validna (pop x)))
    (if no-; (if x (print ',)) ({-print-;))))
  ({-tabi)))

(defun {-numope
 (x)
 (case x
  (logand '&)
  (logor '|)
  (logxor '^)
  (remainder '%)
  (plus '+)
  (difference '-)
  (times '*)
  (quotient '/)
  (t nil)))

(defun {-pi-list
 (x)
 (printc 40)
 (while x
  (print ({-validna (pop x)))
  (if x (print ',))
  ({-maybetab))
 (printc 41))

(defun {-print-; () (setq {-last-} nil) (print ';))

(defun {-print-} () (setq {-last-} t) (print '}))

(defun {-progn
 ({-x)
 (` let
  (({-tab (1+ ({-tab))))
  ,@
  (let
   ((tulos nil))
   (while {-x
    (if
     (or
      (and (atom (car {-x)) (or (not {-retuexpr) (cdr {-x)))
      (member (caar {-x) '(comment %include)))
     (pop {-x)
     (if
      (eq (caar {-x) 'defun)
      (setq {-x
       (` (flet (, (cdr (car {-x))) ,@ (cdr {-x))))
      (push
       (` progn
        (setq {-last-} nil)
        ,
        ({-eka
         (pop {-x)
         (if (not {-x) (prog1 {-retuexpr (setq {-retuexpr nil))))
        (unless {-last-} ({-print-;))
        ({-tabi))
       tulos))))
   (reverse tulos))))

(defun {-quote
 (x)
 (let
  ((defined (member x {-CONST)))
  (if defined
   (` progn
    (print '_const_)
    (print '[)
    (print , (length defined))
    (print '])
    ,
    (if
     (and (atom x) (not (member x '(/* */))))
     (` prints '/* ', x '*/)))
   (progn (push x {-CONST) ({-quote x)))))

(defun {-repeat
 ({-y {-x)
 (while (cdr {-y) (push (pop {-y) {-x))
 (setq {-x (reverse {-x))
 (setq {-y (car {-y))
 (setq {-retuexpr nil)
 (` let
  (({-tab (1+ {-tab)))
  (print 'do)
  ({-tabi)
  (setq {-last-} nil)
  ,
  ({-eka (cons 'progn {-x))
  (if {-last-} (setq {-last-} nil) ({-print-;))
  ({-tabi)
  (print 'while)
  (printc 40)
  ,
  ({-want-boolean (list 'not {-y))
  (printc 41)
  (setq {-last-} nil)))

(defun {-repeat-times
 ({-x {-y)
 (` let
  (({-tab (1+ {-tab)))
  (print '{)
  (print 'int)
  (sp)
  (print '_i)
  ({-print-;)
  ({-tabi)
  (print 'for)
  (sp)
  (printc 40)
  (print '_i=0)
  ({-print-;)
  (print '_i<)
  ,
  ({-want-number {-x)
  ({-print-;)
  (print '_i++)
  (printc 41)
  ({-tabi)
  ,
  ({-eka (cons 'progn {-y) {-retuexpr)
  ,
  (unless (cdr {-y) '({-print-;))
  ({-print-})))

(defun {-return
 ({-y {-retuexpr)
 ({-eka {-y
  ({-chain-retuexpr
   (` lambda
    (x)
    (list
     'progn
     '(print 'return)
     '(sp)
     x)))))

(defun {-save-definition
 (filename)
 (eval
  (quote
   (let
    ((fil (create filename)))
    (out fil)
    (mapc *COMPILED-FUNCTIONS*
     (function
      (lambda
       (f)
       (let
        ((p (LSEEK fil 2)))
        (pprint (cadr f))
        (cr)
        (rplacd f (list '*IN-FILE* filename p))))))
    (close fil)
    (out 0)
    (setq {-autost2 autost)
    (setq autost {-autost)))))

(defun {-setq
 ({-x {-y)
 ({-eka {-y
  ({-chain-retuexpr
   (` lambda
    (x)
    (list
     'progn
     '(printc 40)
     '(print ', ({-localvar {-x))
     '(print '=)
     '(sp)
     x
     '(printc 41))))
  {-WANT-EXPRESSION))

(defun {-strai-arg
 (x)
 (if
  (atom x)
  x
  (if
   (atom (car x))
   (cons (car x) ({-strai-arg (cdr x)))
   (cons
    (let ((name (gensym))) ({-strai-arg2 (car x) name) name)
    ({-strai-arg (cdr x))))))

(defun {-strai-arg2
 (x nimi)
 (if
  (atom x)
  (if (symbolp x) (push (list x nimi) {-temp))
  (progn
   ({-strai-arg2 (car x) (list 'car nimi))
   ({-strai-arg2 (cdr x) (list 'cdr nimi)))))

(defq {-tab 1)

(defun {-tabi ()
 (if {-LINES (setq {-LINES (1+ {-LINES)))
 (cr)
 (tab {-tab))

(defun {-tnil
 ({-x)
 (` progn
  (print '_tnil)
  (printc 40)
  , {-x
  (printc 41)))

(defun {-typed
 ({-x)
 (cond
  ((null {-x) '(print 'NIL))
  ((member {-x '(nil NIL))
   '(print 'NIL))
  ((eq {-x t) '(print 'T))
  ((symbolp {-x) (` print ', ({-localvar {-x)))
  ((numberp {-x) (` integer (print , {-x)))
  ((eq {-x quote) ({-typed ''QUOTE))
  ((atom {-x)
   (push (` ** Error: what is , {-x) {-ERRORS)
   '(print 'NIL/*ERROR*/))
  ((eq (car {-x) '{-cee) (cadr {-x))
  ((member (car {-x) '(integer boolean))
   (list (car {-x) ({-typed (cadr {-x))))
  ((assoc (car {-x) {-MACRO)
   ({-typed
    (eval
     (cons
      (list quote
       (cons 'nlambda (cdr (assoc (car {-x) {-MACRO))))
      (cdr {-x)))))
  (({-numope (car {-x)) (` integer , ({-arith {-x)))
  (({-compope (car {-x)) (` boolean , ({-compare {-x)))
  ((assoc (car {-x) {-LOCAL-FUNCS)
   (let
    (({-name (cdr (assoc (car {-x) {-LOCAL-FUNCS))))
    (unless
     (assoc {-name {-FUNCS)
     (eval ({-function {-name (eval {-name)))
     (set {-name nil))
    ({-typed (cons {-name (cdr {-x)))))
  ((assoc (car {-x) {-FUNCS)
   (if
    (member {-x {-F-OK)
    (let
     ((temp (assoc (car {-x) {-FUNCS)))
     (setq {-F-OK (delete {-x {-F-OK))
     (if
      (caddr temp)
      (let
       (({-retuexpr))
       (` , (caddr temp) , ({-funcall {-x (cadr temp))))
      ({-funcall {-x (cadr temp))))
    (let
     ((letargs) (args) (allatoms t))
     (if
      (cddr {-x)
      (mapc
       (cdr {-x)
       (function
        (lambda
         (x)
         (if
          (or
           (null x)
           (numberp x)
           (member x '(t nil NIL))
           (eq (car x) quote))
          (push x args)
          (let
           ((a ({-loc-gensym)))
           (unless
            (or
             (identp x)
             (and
              (member
               (car x)
               '(car cdr caar cadddr caddr cadr cdar cdddr cddr not null))
              (atom (cadr x))))
            (setq allatoms nil))
           (push (list a x) letargs)
           (push a args)))))))
     ({-typed
      (prog1
       (if
        (or allatoms (null (cdr letargs)))
        {-x
        (progn
         (setq {-x
          (` ,
           (car {-x)
           ,@
           (reverse (cdr args))
           ,
           (cadr (car letargs))))
         (` let , (reverse (cdr letargs)) , {-x)))
       (push {-x {-F-OK))))))
  ((not (atom (car {-x)))
   ({-typed (cons ({-eka (car {-x)) (cdr {-x))))
  ((member (car {-x) '(let progn if while repeat-times repeat let-functions))
   (if {-WANT-EXPRESSION
    ({-eka (` (function (lambda () , {-x))))
    (case
     (car {-x)
     (let ({-let (cdr {-x)))
     (progn
      (if
       (cddr {-x)
       (` progn
        (print '{)
        (sp)
        ,
        ({-progn (cdr {-x))
        ({-print-}))
       ({-typed (cadr {-x))))
     (if ({-if (cadr {-x) (caddr {-x) (cadddr {-x)))
     (let-functions ({-let-functions (cdr {-x)))
     (t
      (if {-retuexpr
       (case
        (car {-x)
        (repeat
         ({-typed
          (` progn (repeat (setq {-repeat (progn ,@ (cdr {-x)))) {-repeat)))
        (t ({-typed (` progn , {-x ()))))
       (case
        (car {-x)
        (while ({-while (cadr {-x) (cddr {-x)))
        (repeat ({-repeat (cdr {-x)))
        (repeat-times ({-repeat-times (cadr {-x) (cddr {-x)))))))))
  ((member (car {-x) '(setq quote function))
   (case
    (car {-x)
    (setq ({-setq (cadr {-x) (caddr {-x)))
    '({-quote (cadr {-x))
    (function
     (let
      ((name ({-validna ({-gensym '_F))) ({-args))
      (push name {-PASKA-F:T)
      (eval ({-function name (cadr {-x)))
      ({-print-;)
      (cr)
      (cr)
      name))))
  ((member
    (car {-x)
    (quote
     (color display-mode draw get-pix get-pix nxtch point printc readcc set-pix set-pix set_cursor set_cursor)))
   (let
    (({-y
      (assoc
       (car {-x)
       (quote
        ((display-mode _setvideomode)
         (color _setcolor)
         (draw _lineto)
         (point _moveto)
         (set-pix _setpixel)
         (set_cursor _settextposition)
         (printc _putch))))))
    (if {-y
     ({-numbs-thnx (cadr {-y) (cdr {-x))
     (case
      (car {-x)
      (nxtch (` integer (print 'nxtch)))
      (readcc
       (quote
        (integer (progn (print 'getch) (print '())))))
      (get-pix (list 'integer ({-numbs-thnx '_getpixel (cdr {-x))))))))
  ((eq (car {-x) 'eq)
   (` boolean
    (progn ,
     ({-eka (cadr {-x) nil '{-WANT-EXPRESSION)
     (print '==)
     ,
     ({-eka (caddr {-x) nil '{-WANT-EXPRESSION))))
  ((and
    (symbolp (car {-x))
    (eq 'lambda (car (definition-of (car {-x)))))
   (let
    (({-LOCALVAR) ({-LOCAL-FUNCS))
    (eval ({-function (car {-x) (definition-of (car {-x)))))
   ({-typed {-x))
  ((and
    (eq (car {-x) 'case)
    (let
     ((y t))
     (mapc
      (cddr {-x)
      (function
       (lambda
        (x)
        (unless
         (or ({-all-numbers (car x)) (eq (car x) 't))
         (setq y nil)))))
     y))
   (if {-WANT-EXPRESSION
    ({-eka (` (function (lambda () , {-x))))
    (` progn
     (print 'switch)
     (printc 40)
     ,
     (let (({-retuexpr)) ({-want-number (cadr {-x)))
     (printc 41)
     (print '{)
     ,@
     (map
      (cddr {-x)
      (function
       (lambda
        ({-x)
        (` progn ,@
         (cond
          ((numberp (car {-x)) (list ({-case-1 (car {-x))))
          ((not (atom (car {-x))) (map (car {-x) {-case-1))
          (t '(progn ({-tabi) (print 'default:))))
         ,
         (let (({-retuexpr {-retuexpr)) ({-progn (cdr {-x)))
         (print 'break)
         ({-print-;)))))
     ,
     (setq {-retuexpr)
     (print '}))))
  ((and
    (symbolp (car {-x))
    (member (car (eval (car {-x))) '(mlambda macro)))
   ({-typed
    (eval (cons ({-macrofy (eval (car {-x))) (cdr {-x)))))
  (t
   (push (` ** Error: cant compile , {-x) {-ERRORS)
   '(print 'NIL/*ERROR*/))))

(defun {-valid-str
 (x)
 (setq x (explode x))
 (while x
  (if
   (member (car x) '(34 92))
   (print '\))
  (printc (pop x))))

(defun {-validna
 (name)
 (cond
  ((cdr (assoc name {-VALIDNA)))
  (({-cdr-assoc name {-VALIDNA) name)
  ((not (atom name))
   (let
    ((name2 (string-append (car name) ({-validna (cadr name)))))
    (push (cons name name2) {-VALIDNA)
    (push (cons name2 name2) {-VALIDNA)
    ({-validna name)))
  ((member name
    (quote
     (asm auto break case char const continue default do
      double else enum extern float for goto if int long
      register return short sizeof static struct switch typedef
      union unsigned void while far fortran huge near pascal)))
   (push
    (cons name (compress (cons (char _) (explode name))))
    {-VALIDNA)
   ({-validna name))
  (t
   (let
    ((val
      (function
       (lambda
        (x)
        (let
         ((ch (car x)))
         (cond
          ((null x) ())
          ((or
            (= ch 95)
            (< 47 ch 58)
            (< 96 ch 123)
            (< 64 ch 91))
           (cons ch (val (cdr x))))
          (t
           (cons 95
            (cons (plus 65 (remainder ch 24)) (val (cdr x))))))))))
     (name2))
    (setq name2 (val (explode name)))
    (if
     (greaterp (length name2) 20)
     (setq name2
      (nconc
       (reverse (cdxr (difference (length name2) 16) (reverse name2)))
       (explode (length name2)))))
    (setq name2 (compress name2))
    (push (cons name name2) {-VALIDNA)
    name2))))

(defun {-want-boolean
 ({-x)
 (let
  (({-WANT-EXPRESSION t))
  (setq {-x ({-typed {-x))
  (case
   (car {-x)
   (boolean (cadr {-x))
   (integer '(print 1))
   (t (` progn (print 'NIL!=) , {-x)))))

(defun {-want-number
 ({-x)
 (let
  (({-WANT-EXPRESSION t))
  (setq {-x ({-typed {-x))
  (cond
   ((numberp {-x) (` print , {-x))
   ((eq (car {-x) 'integer) (cadr {-x))
   (t
    (` progn
     (print '_numval)
     (printc 40)
     , {-x
     (printc 41))))))

(defun {-while
 ({-x {-y)
 (setq {-retuexpr nil)
 (` let
  (({-tab (1+ {-tab)))
  (print 'while)
  (printc 40)
  ,
  ({-want-boolean {-x)
  (printc 41)
  ({-tabi)
  ,
  ({-eka (cons 'progn {-y))))

(defun {_makenumber
 (x)
 (` progn
  (print '_makenumber)
  (printc 40)
  , x
  (printc 41)))
