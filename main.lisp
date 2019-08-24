(defpackage onlisp
  (:use :cl :alexandria)
  (:export #:map0-n  ;; some map functions
           #:map1-n
           #:mapa-b
           #:map->

           ;; in
           #:in
           #:inq
           #:in-if

           ;; loops macro
           #:while
           #:till
           #:for
           #:do-tuples/o
           #:do-tuples/c
           #:mvdo*
           #:mvpsetq
           #:mvdo

           ;; setf related
           #:toggle
           #:allf
           #:nilf
           #:tf
           #:concf
           #:conc1f
           #:concnew

           #:acond

           #:compose
           #:fn
           
           ))

(in-package :onlisp)



;;; chapter 4.5
;;; some map funcions
(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))



;;; chapter 11.3
(defmacro in (obj &rest choices)
  (let ((g (gensym)))
    `(let ((,g ,obj))
       (or ,@(mapcar (lambda (x) `(eql ,g ,x))
                     choices)))))

;; with-gensyms from alexandria
(defmacro in2 (obj &rest choices)
  (with-gensyms (g)
    `(let ((,g ,obj))
       (or ,@(mapcar (lambda (x) `(eql ,g ,x))
                     choices)))))

(defmacro inq (obj &rest choices)
  `(in ,obj ,@(mapcar (lambda (c) `',c) choices)))


(defmacro in-if (fn &rest choices)
  `(or ,@(mapcar (lambda (x) `(funcall ,fn ,x))
                 choices)))



;;; chapter 11.4
(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro till (test &body  body)
  `(do ()
       (,test)
     ,@body))

(defmacro for ((var start stop) &body body)
  (with-gensyms (gstop)
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))


(defmacro do-tuples/o (parms source &body body)
  (if parms
      (let ((src (gensym)))
        `(prog ((,src ,source))
            (mapc (lambda ,parms ,@body)
                  ,@(map0-n (lambda (n)
                              `(nthcdr ,n ,src))
                            (1- (length parms))))))))

;; Don't know why code on book is that complicated
;; And not understand it
;; Here is a limit in the macro, length of parms should <= length of source * 2
(defmacro do-tuples/c (parms source &body body)
  (if parms
      (let ((src (gensym)))
        `(prog ((,src ,source))
            (mapc (lambda ,parms ,@body)
                  ,@(map0-n (lambda (n)
                              `(append (nthcdr ,n ,src) (subseq ,src 0 ,n)))
                            (1- (length parms))))))))



;;; Chapter 11.5  mvdo*

;;; mvdo* example
;; (mvdo* ((x 1 (1+ x))
;;         ((y z) (values 0 0) (values z x)))
;;     ((> x 5) (list x y z))
;;   (princ (list x y z)))

;; macro expand to:

;; (LET ((X 1))
;;   (MULTIPLE-VALUE-BIND (Y Z)
;;       (VALUES 0 0)
;;     (PROG ()
;;        #:G593
;;        (IF (> X 5)
;;            (RETURN (PROGN (LIST X Y Z))))
;;        (PRINC (LIST X Y Z))
;;        (SETQ X (1+ X))
;;        (MULTIPLE-VALUE-SETQ (Y Z) (VALUES Z X))
;;        (GO #:G593))))



(defmacro mvdo* (parm-cl test-cl &body body)
  (mvdo-gen parm-cl parm-cl test-cl body)) ; Use function to help generate code

;; This function is mainly used as generate bind nested form, with recursive call self.
;; And pass rebind generate task to `mvdo-rebind-gen` function
(defun mvdo-gen (binds rebinds test body)
  (if (null binds)  ;; recursive function, incase this is the last call, so binds is null while rebinds is not.
      (let ((label (gensym)))
        `(prog () ;; Just like `do` macro, it expand mvdo* to `prog` form, use `go` to loop
            ,label
            (if ,(car test)  ;; test test-case here
                (return (progn ,@(cdr test)))) ;; use `progn` here in case of list of clauses
            ,@body ;; loop body
            ,@(mvdo-rebind-gen rebinds)  ;; As init binding has done, let's generate rebind form, use function to help
            (go ,label) ;; use `go label` to loop
            ))
      (let ((rec (mvdo-gen (cdr binds) rebinds test body)))  ;; recursive call self, put result in rec
        (let ((var/s (caar binds))
              (expr (cadar binds)))
          (if (atom var/s)            ;; check the init-form is single value or multi value
              `(let ((,var/s ,expr))  ;; single value case
                 ,rec)
              `(multiple-value-bind ,var/s ,expr  ;; multi value case
                 ,rec)
              )))))

;; This is also a recursive function. once handle a bind.
(defun mvdo-rebind-gen (rebinds)
  (cond ((null rebinds) nil)
        ((< (length (car rebinds)) 3) (mvdo-rebind-gen (cdr rebinds))) ;; the rebind don't have step-form, so just call next cyle
        (t (cons  ;; use `cons` to connect all generated-rebind-forms as a list
            (let ((var/s (caar rebinds))
                  (expr (caddar rebinds)))
              (if (atom var/s)  ;; check the init-form is single value or multi value  
                  `(setq ,var/s ,expr)
                  `(multiple-value-setq ,var/s ,expr)))
            (mvdo-rebind-gen (cdr rebinds)) ;; call self recursively
            ))))




;; chapter 11.5
;; mvpsetq

;; --- My own solution ---

;; Macro expand example:
;; (let ((w 0) (x 1) (y 2) (z 3))
;;   (mvpsetq (w x x) (values 'a 'b) (y z) (values w x))
;;   (list w x y z))

;; (LET ((W 0) (X 1) (Y 2) (Z 3))
;;   (MULTIPLE-VALUE-BIND (#:G592 #:G593 #:G594)
;;       (VALUES 'A 'B)
;;     (MULTIPLE-VALUE-BIND (#:G595 #:G596)
;;         (VALUES W X)
;;       (PSETQ W #:G592
;;              X #:G593
;;              X #:G594
;;              Y #:G595
;;              Z #:G596)))
;;   (LIST W X Y Z))

;; use gen-cl to generate expression, because I need use recursion for multiple-value-bind
;; gen-lst is used for collect generate gensyms for psetq use
(defmacro mvpsetq-1 (&rest args)  ;; Use *-1 name to escape confilit with latter book solution
  (let (gen-lst)
    (gen-cl args args gen-lst)))

;; helper func for mvpsetq
(defun gen-cl (bind-cls set-cls gen-lst)
  (if (null bind-cls)
      `(psetq ,@(shuffle-1 (flatten-1 (loop :for x :in set-cls :by #'cddr :collect x))  ;; the loop here is to collect ((w x) (values 'a 'b) (y z) (values w x)) -> ((w x) (y z))
                           gen-lst)) ;; generate`( psetq x1 v1 x2 v2)` expression
      (let ((g-lst (mapcar #'(lambda (x) (gensym))
                           (first bind-cls))))  ;; generate list of gensyms for multiple-value-bind in this recursive
        (if (listp (first bind-cls))  ;; let's decide use `multiple-value-bind` or `let` to bind
            `(multiple-value-bind ,g-lst
                 ,(second bind-cls)
               ,(gen-cl (cddr bind-cls) set-cls (append gen-lst g-lst)))  ;; recusively call self, accumlate genyms to gen-list, for latter psetq
            `(let (((first bind-cls) ,(second bind-cls)))
               ,(gen-cl (cddr bind-cls) set-cls (append gen-lst g-lst)))  ;; recusively call self, accumlate genyms to gen-list, for latter psetq
            )
        )))  


;; helper func, used to extract symbols need binding
;; ONLISP> (flatten-1 '(1 (2) 3 (4 (5) 6)))
;; (1 2 3 4 (5) 6)
(defun flatten-1 (l)
  (loop :with result := '()
        :for x :in l
        :if (listp x)
        :do (setf result (append result x))
        :else
        :do (setf result (append result (list x)))
        :finally (return result)))


;; helper func, used to create expression for psetq with a list of gensyms and a list of set symbols
;; ONLISP> (shuffle-1 '(1 2 3) '(a b c))
;; (1 A 2 B 3 C)
(defun shuffle-1 (l1 l2)  ;; name it shuffle-1 to escape shuffle fun in below, although they do same.
  (let (result)
    (mapcar #'(lambda (x y) (push x result) (push y result))
            l1
            l2)
    (nreverse result)))


;; --- My own solution end ---

;; --- Book solution ---



(defmacro mvpsetq (&rest args)
  (let* ((pairs (group args 2))  ;; Group args to ( ((w x) (values 'a 'b))   ( (y z) (values w x))  )
         (syms (mapcar #'(lambda (p)
                           (mapcar #'(lambda (x) (gensym)) (ensure-list (car p))))
                       pairs))) ;; Pregenerate '(gensym1 ...) for latter bind use
    (labels ((rec (ps ss)
               (if (null ps)
                   `(setq ,@(mapcan #'(lambda (p s) (shuffle-it (ensure-list (car p)) s))
                                    pairs syms))
                   (let ((body (rec (cdr ps) (cdr ss)))) ;; recursive call self, because we have Got the syms list , not like my-own solution, generate in each recursion.
                     (let ((var/s (caar ps))
                           (expr (cadar ps)))
                       (if (consp var/s)
                           `(multiple-value-bind ,(car ss) ,expr
                              ,body)
                           `(let ((,@(car ss) ,expr))
                              ,body)))))))
      (rec pairs syms))))

;; Rename to shuffle-it, avoid conflict with alexandria:shuffle
(defun shuffle-it (x y)
  (cond ((null x) y)
        ((null y) x)
        (t (list* (car x) (car y)
                  (shuffle-it (cdr x) (cdr y))))))

(defun group (source n)
  "Group source into each with n numbers of elements unless the last is not enough"
  (when (<= n 0) (error "length not greater than zero"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

;; --- Book solution end ---



;;; chapter 11.5 mvdo


;; mvdo example
;; (mvdo ((x 1 (1+ x))
;;        ((y z) (values 0 0) (values z x)))
;;     ((> x 5) (list x y z))
;;   (princ (list x y z)))

;; macro expand to:

;; (LET (#:G642 #:G643 #:G644)
;;   (MVPSETQ #:G642 1 (#:G643 #:G644) (VALUES 0 0))
;;   (PROG ((X #:G642) (Y #:G643) (Z #:G644))
;;      #:G641
;;      (PRINC (LIST X Y Z))
;;      (IF (> X 5)
;;          (RETURN (PROGN (LIST X Y Z))))
;;      (MVPSETQ X (1+ X) (Y Z) (VALUES Z X))
;;      (GO #:G641)))



;; 不同于mvdo*, mvdo需要一次性将参数初始form提取出来，采用mvpsetq初始赋值， 这样防止参数赋值前后顺序影响，
;; 另外, 由于含有多值赋值，所以需要用到上面定义的psetq的多值版本mvpsetq。
;; 同样的，原来的rebind form采用多个 setq/multiple-value-bind 前后依次更新值，现在也需要mvpsetq 来并行更新值了
;; 所以整体就没有递归调用生成了，因为不需要嵌套了


;; --- My solution ---
;; Use mvdo-1 name here to escape conflict with latter book solution
(defmacro mvdo-1 (parm-cl test-cl &body body)
  (let* ((label (gensym))
         (let-cl '()) ;; form like:  #:G593 #:G594 #:G595
         (mvsetq-cl '()) ;; form like:  #:G593 1 (#:G594 #:G595) (values 0 0)
         (prog-cl '()) ;; form like:  (X #:G593) (Y #:G594) (Z #:G595)
         )
    ;; Use a loop to generate three main clauses for macro
    ;; when generate gensym, the push it to different clauses according to different require
    ;; use loop with it's side effect
    (loop :for each :in parm-cl
          :do (if (listp (first each))
                  ;; ((y z) (values 0 0) -> (values z x)) (y z) -> ((y #:g1) (z #:g2))
                  (let ((var-g-pair-list (mapcar #'(lambda (x) (list x (gensym))) (first each))))
                    ;; construct let-cl
                    (mapcar #'(lambda (p) (push (second p) let-cl)) var-g-pair-list)
                    ;; construct mvsetq-cl
                    (push (mapcar #'second  var-g-pair-list) mvsetq-cl)
                    (push (second each) mvsetq-cl)
                    ;; construct prog-cl
                    (mapcar #'(lambda (p) (push p prog-cl)) var-g-pair-list)
                    )
                  (let ((g (gensym)))
                    ;; construct let-cl
                    (push g let-cl)
                    ;; construct mvsetq-cl
                    (push g mvsetq-cl)
                    (push (second each) mvsetq-cl)
                    ;; construct prog-cl
                    (push (list (first each) g) prog-cl)))
          :finally (progn
                     (setf let-cl (nreverse let-cl))
                     (setf mvsetq-cl (nreverse mvsetq-cl))
                     (setf prog-cl (nreverse prog-cl))))

    ;; just use the *-cl generated above
    `(let (,@let-cl)
       (mvpsetq ,@mvsetq-cl)
       (prog (,@prog-cl) ;; TODO init bind
          ,label
          (if ,(car test-cl)
              (return (progn ,@(cdr test-cl))))
          ,@body
          ;; construct rebind form here, because it has no relation to gensyms.
          (mvpsetq ,@(flatten-1
                      (mapcar #'(lambda (p)
                                  (when (third p)
                                    (list (first p)
                                          (third p))))
                              parm-cl)))
          (go ,label)))))

;; --- My solution end ---



;; --- Book solution ---
(defmacro mvdo (binds (test &rest result) &body body)
  (let ((label (gensym))
        (temps (mapcar #'(lambda (b)
                           (if (listp (car b))
                               (mapcar #'(lambda (x)
                                           (gensym))
                                       (car b))
                               (gensym)))
                       binds))) ;; temps like (#:G617 (#:G618 #:G619)), transfer binds to gensym symbols
    `(let ,(mappend #'ensure-list temps)
       (mvpsetq ,@(mapcan #'(lambda (b var)
                              (list var (cadr b)))
                          binds
                          temps))
       (prog ,(mapcar #'(lambda (b var) (list b var)) ;; mapcar 将下面两个列表一一配对
               (mappend #'ensure-list (mapcar 'car binds))  ;; 将所有的初始变量展平
               (mappend #'ensure-list temps)) ;; 将所有的temps展平
          ,label
          (if ,test
              (return (progn ,@result)))
          ,@body
          (mvpsetq ,@(mapcan #'(lambda (b)
                                 (if (third b)  ;; 判断是否有更新的表达式,来决定是否生成
                                     (list (car b)
                                           (third b))))
                             binds))
          (go ,label)))))


;; 宏心得:
;; 1.上面两处的mapcan的使用相当于 (flatten-1 (mapcar ... )). 更加简洁
;; mapcar 是将结果用list连接，而mapcan是用nonc连接结果

;; 2. (mappend #'ensure-list temps) 目的是想把temps“压扁”一层,
;; 相当于我这里定义的flatten-1, 将temps里的元素“上提”一层
;; CL-USER> (flatten-1 '((a b c) d (e (f g) h) ((i)) i))
;; (A B C D E (F G) H (I) I)
;; CL-USER> (mappend #'ensure-list '((a b c) d (e (f g) h) ((i)) i))
;; (A B C D E (F G) H (I) I)

;; 3. 上面的解决方案与我的方案相比，更加简洁。充分利用了mapcar的函数可以接受多个参数的特性，
;; 事先将gensyms生成好，但保持和binds同构，然后用mapcar/mapcan 接受同构的gensyms和binds
;; 而我的解决方案是产生gensyms的同时构造不同用到的地方的语句结构(上面的例子有let-cl, mvpsetq-cl, prog-cl)


;; --- Book solution end ---


;;; Chapter 12.1
(defmacro toggle (&rest args)
  `(progn
     ,@(mapcar #'(lambda (x)
                   `(toggle-2 ,x))
               args)))

(define-modify-macro toggle-2 () not)


;;; Chapter 12.2

;; Setf all args to val
(defmacro allf (val &rest args)
  (with-gensyms (v)
    `(let ((,v ,val))
       (setf ,@(mapcan #'(lambda (a)
                           `(,a ,v))
                       args)))))

;; Setf all args to nil
(defmacro nilf (&rest args)
  `(allf nil ,@args))

;; Setf all args to t
(defmacro tf (&rest args)
  `(allf t ,@args))

;; Like (setf x (nconc x y ...))
(define-modify-macro concf (obj) nconc)

;; Like `push` to last end of list
(defun conc1f/function (place obj)
  (nconc place (list obj)))
(define-modify-macro conc1f (obj) conc1f/function)

;; Like `pushnew` to last end of list
(defun concnew/function (place obj &rest args)
  (if (apply #'member obj place args)
      place
      (nconc place (list obj))))
(define-modify-macro concnew (obj &rest args) concnew/function)



;;; chapter 12.4

;;; --- Macro `_f` ---

(defmacro _f (op place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    `(let* (,@(mapcar #'list vars forms)
            (,(car var) (,op ,access ,@args)))
       ,set)))


;; Macro `conc1f` created with `_f`
;; name with `'` to avoid name conflict
(defmacro conc1f' (lst obj)
  `(_f nconc ,lst (list ,obj)))


;;; --- Macro `pull` ---

;; Use case
;; (let ((x '(1 2 (a b) 3)))
;;   (print (pull '(a b) (cdr x) :test #'equal))
;;   (print x)
;;   nil)
;; =>
;; (2 3) 
;; (1 2 3) 
;; NIL
(defmacro pull (obj place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (let ((g (gensym)))
      `(let* ((,g ,obj)
              ,@(mapcar #'list vars forms)
              (,(car var) (delete ,g ,access ,@args)))
         ,set))))

;; Macro expansion example1
;; (let ((x '(1 2 (a b) 3)))
;;   (pull '(a b) (cdr x) :test #'equal))

;; expand to

;; (LET ((X '(1 2 (A B) 3)))
;;   (LET* ((#:G611 '(A B))
;;          (#:X610 X)
;;          (#:NEW1 (DELETE #:G611 (CDR #:X610) :TEST #'EQUAL)))
;;     (SB-KERNEL:%RPLACD #:X610 #:NEW1)))

;; ;; Macro expansion example2
;; (let ((x '(1 2 (a b) 3)))
;;   (onlisp::pull 3 (cddr x) :test #'equal))

;; expand to

;; (LET ((X '(1 2 (A B) 3)))
;;   (LET* ((#:G617 3)
;;          (#:LIST (CDR X))
;;          (#:NEW (DELETE #:G617 (CDR #:LIST) :TEST #'EQUAL)))
;;     (SB-KERNEL:%RPLACD #:LIST #:NEW)))


;;; --- Macro `pull-if`---

;; use case
;; CL-USER> (let ((l '(1 2  3 4 5)))
;;            (pull-if #'oddp l)
;;            l)
;; =>
;; (2 4)

;; expand to
;; (LET ((L '(1 2 3 4 5)))
;;   (LET* ((#:G619 #'ODDP) (#:NEW1 (DELETE-IF #:G619 L)))
;;     (SETQ L #:NEW1))
;;   L)


(defmacro pull-if (test place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (let ((g (gensym)))
      `(let* ((,g ,test)
              ,@(mapcar #'list vars forms)
              (,(car var) (delete-if ,g ,access ,@args))) ;; the different with pull is here
         ,set))))


;;; --- Macro popn ---

;; Use case
;; (let ((l '(a b c d e f g)))
;;   (print (onlisp::popn 2 (cddr l)))
;;   (print l)
;;   nil)

;; =>

;; (C D) 
;; (A B E F G) 
;; NIL


(defmacro popn (n place)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (with-gensyms (gn glst)
      `(let* ((,gn ,n)
              ,@(mapcar #'list vars forms)
              (,glst ,access)
              (,(car var) (nthcdr ,gn ,glst)))
         (prog1 (subseq ,glst 0 ,gn)
           ,set)))))

;; Macro expansion

;; (LET ((L '(A B C D E F G)))
;;   (PRINT (LET* ((#:GN595 2)
;;                 (#:LIST (CDR L))
;;                 (#:GLST596 (CDR #:LIST))
;;                 (#:NEW (NTHCDR #:GN595 #:GLST596)))
;;            (PROG1 (SUBSEQ #:GLST596 0 #:GN595) (SB-KERNEL:%RPLACD #:LIST #:NEW))))
;;   (PRINT L)
;;   NIL)


;;; --- Macro sortf ---

(defmacro sortf (op &rest places)
  (let* ((meths (mapcar #'(lambda (p)
                            (multiple-value-list
                             (get-setf-expansion p)))
                        places))
         (temps (apply #'append (mapcar #'third meths))))
    `(let* ,(mapcar #'list
                    (mapcan #'(lambda (m)
                                (append (first m)
                                        (third m)))
                            meths)
                    (mapcan #'(lambda (m)
                                (append (second m)
                                        (list (fifth m))))
                            meths))
       ,@(mapcon #'(lambda (rest) ;; 1. use mapcon so `rest` arg will be (#:g1 #:g2 #:g3) (#:g2 #:g3) (#:g3) each time
                     (mapcar
                      ;; 3. so when the inner lambda get (#:g2 #:g3) passed, it will compare to `(car rest)` which is #:g1;
                      ;; when the inner lambda get (#:g3) passed, it will compare to `(car rest)` which is #:g2.
                      ;; So it creates three unless expression
                      #'(lambda (arg)
                          `(unless (,op ,(car rest) ,arg)
                             (rotatef ,(car rest) ,arg)))
                      (cdr rest)))  ;; 2. so (cdr list) passed to the `mapcar lambda` will be (#:g2 #:g3) (#:g3) ()
                 temps)
       ,@(mapcar #'fourth meths))))

;; (let ((x 1)
;;       (y 2)
;;       (z 3))
;;   (sortf > x y z)
;;   (list x y z))

;; => (3 2 1)

;; Macro expand result

;; (LET ((X 1) (Y 2) (Z 3))
;;   (LET* ((#:NEW1 X) (#:NEW1 Y) (#:NEW1 Z))
;;     (UNLESS (> #:NEW1 #:NEW1) (ROTATEF #:NEW1 #:NEW1))
;;     (UNLESS (> #:NEW1 #:NEW1) (ROTATEF #:NEW1 #:NEW1))
;;     (UNLESS (> #:NEW1 #:NEW1) (ROTATEF #:NEW1 #:NEW1))
;;     (SETQ X #:NEW1)
;;     (SETQ Y #:NEW1)
;;     (SETQ Z #:NEW1))
;;   (LIST X Y Z))


;;; Chapter 13 编译期计算

;; 对比 函数版本和宏版本

;; --- Function & Macro avg ---
(defun avg (&rest args)
  (/ (apply #'+ args) (length args)))

;;对 (length args) 的计算由运行期移到了编译期
(defmacro avg (&rest args)
  `(/ (+ ,@args) ,(length args)))

;; Macro expansion

;; CL-USER> (avg 1 2 3 4 5)

;; (/ (+ 1 2 3 4 5) 5)



;; --- Function & Macro most-of ---
(defun most-of (&rest args)
  (let ((all 0)
        (hits 0))
    (dolist (a args)
      (incf all)
      (if a (incf hits)))
    (> hits (/ all 2))))


(defmacro most-of (&rest args)
  (let ((need (floor (/ (length args) 2)))
        (hits (gensym)))
    `(let ((,hits 0))
       (or ,@(mapcar #'(lambda (a)
                         `(and ,a (> (incf ,hits) ,need)))
                     args)))))

;; Macroexpansion 
;; CL-USER> (most-of t t nil nil t)

;; (LET ((#:G584 0))
;;   (OR (AND T (> (INCF #:G584) 2))
;;       (AND T (> (INCF #:G584) 2))
;;       (AND NIL (> (INCF #:G584) 2))
;;       (AND NIL (> (INCF #:G584) 2))
;;       (AND T (> (INCF #:G584) 2))))

;; 利用了or和and的短路性质, ~most-of~ 宏只会求值需要的数量的参数.
;; 最理想情况下，只对刚过半的参数求值。


;; --- Function & Macro nthmost ---
(defun nthmost (n lst)
  (nth n (sort (copy-list lst) #'>)))

(defmacro nthmost-1 (n lst)
  (if (and (integerp n) (< n 20))
      (with-gensyms (glst gi)
        (let ((syms (map0-n #'(lambda (x)
                                (declare (ignore x))
                                (gensym))
                            n)))
          `(let ((,glst ,lst))
             (unless (< (length ,glst) ,(1+ n))
               ,@(gen-start glst syms)
               (dolist (,gi ,glst)
                 ,(nthmost-gen gi syms t))
               ,(car (last syms))))))
      `(nth ,n (sort (copy-list ,lst) #'>))))

(defun gen-start (glst syms)
  (reverse
   (maplist #'(lambda (syms)
                (let ((var (gensym)))
                  `(let ((,var (pop ,glst)))
                     ,(nthmost-gen var (reverse syms)))))
            (reverse syms))))

(defun nthmost-gen (var vars &optional long?)
  (if (null vars)
      nil
      (let ((else (nthmost-gen var (cdr vars) long?)))
        (if (and (not long?) (null else))
            `(setq ,(car vars) ,var)
            `(if (> ,var ,(car vars))
                 (setq ,@(mapcan #'list
                                 (reverse vars)
                                 (cdr (reverse vars)))
                       ,(car vars) ,var)
                 ,else)))))


;; CL-USER> (onlisp::nthmost-1 2 '(2 6 1 4 3 4))

;; (LET ((#:GLST625 '(2 6 1 4 3 4)))
;;   (UNLESS (< (LENGTH #:GLST625) 3)
;;     (LET ((#:G632 (POP #:GLST625)))
;;       (SETQ #:G627 #:G632))
;;     (LET ((#:G631 (POP #:GLST625)))
;;       (IF (> #:G631 #:G627)
;;           (SETQ #:G628 #:G627
;;                 #:G627 #:G631)
;;           (SETQ #:G628 #:G631)))
;;     (LET ((#:G630 (POP #:GLST625)))
;;       (IF (> #:G630 #:G627)
;;           (SETQ #:G629 #:G628
;;                 #:G628 #:G627
;;                 #:G627 #:G630)
;;           (IF (> #:G630 #:G628)
;;               (SETQ #:G629 #:G628
;;                     #:G628 #:G630)
;;               (SETQ #:G629 #:G630))))
;;     (DOLIST (#:GI626 #:GLST625)
;;       (IF (> #:GI626 #:G627)
;;           (SETQ #:G629 #:G628
;;                 #:G628 #:G627
;;                 #:G627 #:GI626)
;;           (IF (> #:GI626 #:G628)
;;               (SETQ #:G629 #:G628
;;                     #:G628 #:GI626)
;;               (IF (> #:GI626 #:G629)
;;                   (SETQ #:G629 #:GI626)
;;                   NIL))))
;;     #:G629))





;; chapter14

;; (acond ((testf-form1) (execute-from1))
;;       ((testf-form2) (execute-from2))
;;       ((testf-form3) (execute-from3)))

(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (sym (gensym)))
        `(let ((,sym ,(car cl1)))
           (if ,sym
               (let ((it ,sym)) ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))

;; (acond (nil (format t "result1 is ~a~%" it))
;;        ((- 3 2) (format t "result2 is ~a~%" it))
;;        (t (format t "default clause")))

;; 初步展开:
;; (LET ((#:G600 NIL))
;;   (IF #:G600
;;       (LET ((IT #:G600))
;;         (FORMAT T "result1 is ~a~%" IT))
;;       (ACOND ((- 3 2) (FORMAT T "result2 is ~a~%" IT))
;;              (T (FORMAT T "default clause")))))

;; 再次展开:
;; (LET ((#:G601 NIL))
;;   (IF #:G601
;;       (LET ((IT #:G601))
;;         (FORMAT T "result1 is ~a~%" IT))
;;       (LET ((#:G602 (- 3 2)))
;;         (IF #:G602
;;             (LET ((IT #:G602))
;;               (FORMAT T "result2 is ~a~%" IT))
;;             (ACOND (T (FORMAT T "default clause")))))))

;; it范围比较大的(可能不合适的)
;; (defmacro acond (&rest clauses)
;;   (if (null clauses)
;;       nil
;;       (let ((cl1 (car clauses)))
;;         `(let ((it ,(car cl1)))
;;            (if it
;;                (progn ,@(cdr cl1))
;;                (acond ,@(cdr clauses)))))))

;; (acond (nil (format t "result1 is ~a~%" it))
;;        ((- 3 2) (format t "result2 is ~a~%" it))
;;        (t (format t "default clause")))
;; ;; 展开成:
;; (LET ((IT NIL))
;;   (IF IT
;;       (PROGN (FORMAT T "result1 is ~a~%" IT))
;;       (LET ((IT (- 3 2)))
;;         (IF IT
;;             (PROGN (FORMAT T "result2 is ~a~%" IT))
;;             (ACOND (T (FORMAT T "default clause")))))))




;; chapter 14
(defmacro alambda (params &body body)
  `(labels ((self ,params ,@body))
     #'self))

(alambda (x) (if (= x 0) 1 (* x (self (1- x)))))


(defmacro ablock (tag &rest args)
  `(block ,tag
     ,(funcall (alambda (args)
                        (case (length args)
                          (0 nil)
                          (1 (car args))
                          (t `(let ((it ,(car args)))
                                ,(self (cdr args))))))
               args)))


;; (ablock outer
;;         (ablock inner
;;                 (return-from inner 1))
;;         (format t "Inner return is ~a~%" it)
;;         (format t "Last step return is ~a~%" it)
;;         "outer ends.")

;; (BLOCK OUTER
;;   (LET ((IT (BLOCK INNER (RETURN-FROM INNER 1))))
;;     (LET ((IT (FORMAT T "Inner return is ~a~%" IT)))
;;       (LET ((IT (FORMAT T "Last step return is ~a~%" IT)))
;;         "outer ends."))))


;;; chapter 15 返回函数的宏

;; compose from chapter5.4
(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

;; compose example
;; CL-USER> (funcall (compose #'1+ #'find-if) #'oddp '(2 3 4))
;; 4


;;; 通用的用于构造函数的宏fn
;;
;; fn是一个函数构造器，用宏来将作为参数的函数组装起来，具体怎么组装，取决于其中的某些参数
;; fn的参数是形如(operator arguments)的表达式
;; operator 可以是一个函数或者宏的名字，也可以是被区别对待的compose函数
;; arguments 可以是接受一个参数的函数或者宏的名字，或者是可作为fn参数的表达式
;;
;; fn宏生成的结果是一个函数，该函数的作用是将接受的参数作用于每个arguments函数上，最后将各个结果作为参数使用operator组装
;; 如果有些arguments函数本身就是一个形如(operator arguments)的格式，那么参数作用于其上时，也按照相同的规则。
;; 比如：
;; (fn (and intergerp oddp)) => #'(lambda (x) (and (intergerp x) (oddp x)))
;; 再比如：
;; (fn (list (1+ truncate))) => #'(lambda (#:g1)
;;                                  (list ((lambda (#:g2) (1+ (truncate #:g2))) #:g1)))
;; 特例，如果参数operator是compose, 那么生成的函数就是复合所有arguments函数的函数。
;; 和直接调用compose类似.
;; 比如
;; (fn (compose list 1+ truncate)) => #'(lambda (#:g1) (list (1+ (truncate #:g1))))
;;
;; 虽然这里将compose作为一种特殊情况处理，但并没有增加fn的功能，只是为了增加调用的可读性。如果把嵌套的参数传给fn就能形成函数的复合。
;; 其实上面的例子也就相当于
;; (fn (list (1+ truncate))) => #'(lambda (#:g1)
;;                                  (list ((lambda (#:g2) (1+ (truncate #:g2))) #:g1)))
;; 等价于
;; (fn (compose list 1+ truncate)) => #'(lambda (#:g1) (list (1+ (truncate #:g1))))
(defmacro fn (expr)
  `#',(rbuild expr))  ;; 通过调用rbuild函数来构造来生成函数expression

(defun rbuild (expr)
  (if (or (atom expr) (eq (car expr) 'lambda)) ;; 判断传入的表达式是个atom或者是个lambda表达式，则返回该表达式自身
      expr
      (if (eq (car expr) 'compose)  ;; 根据operator是不是特殊情况compose，调用不同的函数来构造表达式
          (build-compose (cdr expr))
          (build-call (car expr) (cdr expr)))))

(defun build-compose (fns)
  (let ((g (gensym)))
    `(lambda (,g)  ;; 外层的lambda是因为我们需要构造一个函数返回
       ,(labels ((rec (fns) ;; 这里定一个临时的递归函数rec，该函数接受一些函数列表，嵌套调用，最左边的函数包装在最外边调用，也就是最后调用，最后生成函数递归调用的表达式
                   (if fns  ;; 递归的临界值检查
                       `(,(rbuild (car fns))
                         ,(rec (cdr fns))) ;; 生成函数递归调用的表达式， (rbuild (car fns)) 是防止operator又是一个(operator arguments)的形式 
                       g))) ;; 临界值，复合函数最后的参数是最外面函数的参数
          (rec fns)))))

(defun build-call (op fns)
  (let ((g (gensym)))
    `(lambda (,g) ;; 外层的lambda是因为我们需要构造一个函数返回
       (,op ,@(mapcar #'(lambda (f)
                          `(,(rbuild f) ,g))  ;; 对每个fn都将参数g传入，最后的结果是list,使用,@展开，作为参数传给op。其中，防止fns里有嵌套的(operator arguments)形式，递归调用(rbuild f)
                      fns)))))


;;; 先回顾 chapter 5.5 在cdr上递归
;; 我们来看下面两个在list上以cdr递归的函数:
;; 第一个函数是求一个列表的长度 (实际上和内置length功能相同)
(defun our-length (lst)
  (if (null lst)
      0                             ;; 这里是最基础的值
      (1+ (our-length (cdr lst)))))  ;; 在cdr上递归，每次递归结果+1

;; 第二个函数是判断一个列表里的元素是否都是基数 (实际上和内置的every+oddp功能相同)
(defun our-every-oddp (lst)
  (if (null lst)
      t                                   ;; base case
      (and (oddp (car lst))               ;; 取出第一个元素判断是否是基数，
           (our-every-oddp (cdr lst)))))  ;; 然后在cdr上递归，将car的结果和cdr的结果用and连接

;; 甚至，让我们看看every函数
(defun our-every (fn lst)
  (if (null lst)
      t                                ;; base case
      (and (funcall fn (car lst))      ;; 取出第一个元素判断fn调用结果
           (our-every fn (cdr lst))))) ;; 然后在cdr上递归，将car的结果和cdr的结果用and连接


;; 这些函数无论从函数定义的表达式形状还是从逻辑（其实是一回事，毕竟逻辑由函数定义来展现）来看都有其共同之处：
;; 1. 他们都需要一个base case
;; 2. 他们都在cdr上做递归调用

;; 另外，前两个将函数逻辑将car上调用的逻辑 (our-length是不管什么，都认定为长度1，our-every-oddp是调用oddp)写死在函数实现里，
;; 而最后一个函数将逻辑以参数形式传入 (fn)。
;; 但这三者car 和 cdr 调用结果的连接逻辑都写死在了函数实现里 (第一个是+1,第二个和第三个是and)

;; 考虑到这些共同的模式，现在我们考虑构造一个通用的函数或者宏，用它来替代上面这些共同模式函数:
;; 1. base case            都不一样，由外界传入
;; 2. 在cdr上递归            是一个共同模式，写死在实现里
;; 3. car上的处理逻辑        不是共同模式，由外界传入
;; 4. 如何连接car和cdr的结果  不是共同模式， 由外界传入
(defun lrec (car-fn conc-fn base)
  (labels ((self (lst)       ;; 这里假设产生的函数只接受一个list参数，其他处理逻辑在生成函数的时候指定
             (if (null lst)
                 base        ;; base case
                 (funcall conc-fn
                          (funcall car-fn (car lst))
                          (self (cdr lst))))))  ;; 调用连接函数，将car的结果和cdr的结果连接
    #'self))  ;; 这里使用labels定义一个内置的函数self,在将其返回的原因是，在cdr上递归调用的时候，需要使用这个生成的self




;; 这样，our-length可以写成
(defun nil->0 (n) ;; 辅助函数
  (if n n 0))
;; 这个函数等价于our-length
(lrec (lambda (x) (declare (ignore x)))
      (lambda (x1 x2) (+ (nil->0 x1)
                    (nil->0 x2)))
      0)

;; 给他封装个名字
(defun our-length2 (lst)
  (funcall (lrec (lambda (x) (declare (ignore x)) 1)
                 (lambda (x1 x2) (+ (nil->0 x1)
                               (nil->0 x2)))
                 0)
           lst))

;; 这样our-every-oddp可以写成
(defun our-every-oddp2 (lst)
  (funcall (lrec (lambda (x) (oddp x))
                 (lambda (x1 x2) (and x1 x2))
                 t)
           lst))

;; 这样上面的our-every可以写成
;; 考虑到our-every的car-fn是以参数形式传入的
(defun our-every2 (fn lst)
  (funcall (lrec fn ;; 这里的fn就是car-fn
                 (lambda (x1 x2) (and x1 x2))
                 t)
           lst))

;; 利用lrec，我们再来定义以下函数
;; 复制列表的copy-list
(defun our-copy-list (lst)
  (funcall (lrec (lambda (x) x)
                 (lambda (x1 x2) (cons x1 x2))
                 nil)
           lst))

;; 移除重复元素的remove-duplicates
(defun our-remove-duplicates (lst)
  (funcall (lrec (lambda (x) x)
                 (lambda (x1 x2) (adjoin x1 x2))
                 nil)
           lst))
;; 另一个版本的find-if
(defun our-find-if (fn lst)
  (funcall (lrec (lambda (x) (and (funcall fn x) x))  ;; 这里用and是为了返回找到的这个元素，不然可以直接(funcall fn x)
                 (lambda (x1 x2) (if x1 x1 x2))
                 nil)
           lst))

;; 另一个版本的some
(defun our-some (fn lst)
  (funcall (lrec (lambda (x) (funcall fn x))
                 (lambda (x1 x2) (or x1 x2))
                 nil)
           lst))



;; 在onlisp书上5.5章节上提到的lrec和我这边的实现方式有些不同
;; 与我上面的版本相比，这个版本将如何处理递归放到一个函数里，而不像我逻辑上分成处理car的和处理conc的
;; 他是将conc-fn和car-fn合成一个函数rec传入
;; rec接收两个参数，第一个是(car lst), 第二个递归调用f, (funcall f)会返回递归调用之后项的结果
(defun lrec-onlisp-book-version (rec &optional base)
  (labels ((self (lst)
             (if (null lst)
                 base
                 (funcall rec (car lst)
                          #'(lambda ()
                              (self (cdr lst)))))))
    #'self))

;; 使用这个版本，our-length可以写成
(defun our-length3 (lst)
  (funcall (lrec-onlisp-book-version
            (lambda (x f)
              (declare (ignore x))
              (1+ (funcall f))) 
            0)
           lst))

;; 使用这个版本，our-every-oddp可以写成
(defun our-every-oddp3 (lst)
  (funcall (lrec-onlisp-book-version
            (lambda (x f)
              (and (oddp x) (funcall f))) 
            t)
           lst))

;; 使用这个版本，our-every可以写成
(defun our-every3 (fn lst)
  (funcall (lrec-onlisp-book-version
            (lambda (x f)
              (and (funcall fn x) (funcall f))) 
            t)
           lst))



;; 变形
;; 看到上面lrec-onlisp-book-version的时候，我想为什么f会以这种方式来用呢？
;; 我做了下尝试
(defun lrec-onlisp-book-version2 (rec &optional base)
  (labels ((self (lst)
             (if (null lst)
                 base
                 (funcall rec (car lst)
                          (self (cdr lst))))))
    #'self))

;; 使用这个版本，our-length可以写成
(defun our-length4 (lst)
  (funcall (lrec-onlisp-book-version2
            (lambda (x f)
              (declare (ignore x))
              (1+ (funcall f))) 
            0)
           lst))

;; 使用这个版本，our-every-oddp可以写成
(defun our-every-oddp4 (lst)
  (funcall (lrec-onlisp-book-version2
            (lambda (x f)
              (and (oddp x) f)) 
            t)
           lst))

;; 使用这个版本，our-every可以写成
(defun our-every4 (fn lst)
  (funcall (lrec-onlisp-book-version2
            (lambda (x f)
              (and (funcall fn x) f)) 
            t)
           lst))

;; 其实这里的f更像是一个值的累积。
;; 手动演算下lrec-onlisp-book-version和lrec-onlisp-book-version2 产生的函数的计算过程
;; 可以感受到两者的差别:
;; lrec-onlisp-book-version版本由于递归传入的是一个函数，实际上先计算的是列表前面的元素
;; 而lrec-onlisp-book-version2版本由于递归传入的是值，所以在展开调用的过程中，不断先求值它的参数，
;; 所以实际上先计算的是列表后面的元素
;; 而之前的lrec版本类似后面这种，先求值后面的元素
;; 总体上我觉得lrec-onlisp-book-version这样的方式更加合理些。


;; ;; 调用求值顺序打印示意
;; (defun lrec-onlisp-book-version (rec &optional base)
;;   (labels ((self (lst)
;;              (print "eval self")
;;              (if (null lst)
;;                  base
;;                  (funcall rec (car lst)
;;                           #'(lambda ()
;;                               (self (cdr lst)))))))
;;     #'self))

;; (defun our-length3 (lst)
;;   (funcall (lrec-onlisp-book-version
;;             (lambda (x f)
;;               (declare (ignore x))
;;               (print "eval rec")
;;               (1+ (funcall f))) 
;;             0)
;;            lst))

;; (defun lrec-onlisp-book-version2 (rec &optional base)
;;   (labels ((self (lst)
;;              (print "eval self")
;;              (if (null lst)
;;                  base
;;                  (funcall rec (car lst)
;;                           (self (cdr lst))))))
;;     #'self))

;; (defun our-length4 (lst)
;;   (funcall (lrec-onlisp-book-version2
;;             (lambda (x f)
;;               (declare (ignore x))
;;               (print "eval rec")
;;               (1+ f)) 
;;             0)
;;            lst))


;; CL-USER> (our-length3 '(1 2 3 4))

;; "eval self" 
;; "eval rec" 
;; "eval self" 
;; "eval rec" 
;; "eval self" 
;; "eval rec" 
;; "eval self" 
;; "eval rec" 
;; "eval self" 
;; 4


;; CL-USER> (our-length4 '(1 2 3 4))

;; "eval self" 
;; "eval self" 
;; "eval self" 
;; "eval self" 
;; "eval self" 
;; "eval rec" 
;; "eval rec" 
;; "eval rec" 
;; "eval rec" 
;; 4

;; ;; lrec版本的求值顺序
;; (defun lrec (car-fn conc-fn base)
;;   (labels ((self (lst)       ;; 这里假设产生的函数只接受一个list参数，其他处理逻辑在生成函数的时候指定
;;              (print "eval self")
;;              (if (null lst)
;;                  base ;; base case
;;                  (funcall conc-fn
;;                           (funcall car-fn (car lst))
;;                           (self (cdr lst))))))  ;; 调用连接函数，将car的结果和cdr的结果连接
;;     #'self))

;; (defun our-length2 (lst)
;;   (funcall (lrec (lambda (x) (declare (ignore x)) (print "eval car-fn") 1)
;;                  (lambda (x1 x2) (print "eval conc-fn") (+ (nil->0 x1)
;;                                                       (nil->0 x2)))
;;                  0)
;;            lst))

;; CL-USER> (our-length2 '(1 2 3 4))

;; "eval self" 
;; "eval car-fn" 
;; "eval self" 
;; "eval car-fn" 
;; "eval self" 
;; "eval car-fn" 
;; "eval self" 
;; "eval car-fn" 
;; "eval self" 
;; "eval conc-fn" 
;; "eval conc-fn" 
;; "eval conc-fn" 
;; "eval conc-fn" 
;; 4


;; 可以观察到一些异同





;; 接下来，让我们以宏的方式来构造lrec，从而简化使用，因为目前传入的都是函数，不是很直观
;; 按照onlisp chapter5.5上的lrec定义如下
(defun lrec2 (rec &optional base)
  (labels ((self (lst)
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                     base)  ;; 以防传进来的base是一个封装的function
                 (funcall rec (car lst)
                          #'(lambda ()
                              (self (cdr lst)))))))
    #'self))


(defmacro alrec (rec &optional base)
  (let ((gfn (gensym)))
    `(lrec2
      #'(lambda (it ,gfn)
          (symbol-macrolet ((rec (funcall ,gfn)))
            ,rec))
      ,base)))


;; 这里的alrec宏方便了lrec2的使用:
;; 比如说之前通过lrec2定义our-every-oddp时需要这样
(lrec2 (lambda (x f) (and (oddp x) (funcall f)))
       t)

;; 使用alrec可以简化这一流程
(alrec (and (oddp it) rec)
       t)

;; 上面的表达式展开为
(LREC2
 #'(LAMBDA (IT #:G582)
     (SYMBOL-MACROLET ((REC (FUNCALL #:G582)))
       (AND (ODDP IT) REC)))
 T)


;; 这里采用了指代宏来简化这一方法
;; it 表示car list
;; rec 表示 递归调用
;; 用宏生成外面的lambda

;; 使用这个版本，our-length可以写成
(defun our-length5 (lst)
  (funcall (alrec (1+ rec) 0)
           lst))

;; 使用这个版本，our-every-oddp可以写成
(defun our-every-oddp5 (lst)
  (funcall (alrec (and (oddp it) rec) t)
           lst))

;; 使用这个版本，our-every可以写成
(defun our-every5 (fn lst)
  (funcall (alrec (and (apply fn it) rec) t)
           lst))

;; 考虑到上面都是采用funcall 这种形式
;;(其实采用funcall这种形式是因为下面这一的定义是不行的)
;; (defun our-length-failed (lst)
;;   ((alrec (1+ rec) 0) lst))

;; 可以再用宏on-cdrs简化这一模式
(defmacro on-cdrs (rec base &rest lsts)
  `(funcall (alrec ,rec #'(lambda () ,base))  ;; 这里将base用function包裹传入，如果base是个复杂的expression的话，防止base被求值多次，包裹后只在最后一次求值
            ,@lsts))

;; 使用这个版本，our-length可以写成
(defun our-length6 (lst)
  (on-cdrs (1+ rec) 0 lst))

;; 使用这个版本，our-every-oddp可以写成
(defun our-every-oddp6 (lst)
  (on-cdrs (and (oddp it) rec) t lst))

;; 使用这个版本，our-every可以写成
(defun our-every6 (fn lst)
  (on-cdrs (and (funcall fn it) rec) t lst))

;; 使用on-cdrs定义一些实用函数
;; 该版本相比union可以接收多个lists
(defun unions (&rest sets)
  (on-cdrs (union it rec) (car sets) (cdr sets)))

(defun intersections (&rest sets)
  (unless (some #'null sets)
    (on-cdrs (intersection it rec) (car sets) (cdr sets))))

(defun differences (set &rest outs)
  (on-cdrs (set-difference rec it) set outs))

;; maxmin 返回args里最大值和最小值
;; 这边涉及到多只递归的方式有些费解啊
(defun maxmin (args)
  (when args
    (on-cdrs (multiple-value-bind (mx mn) rec ;; 这里将rec考虑成递归调用的结果,一个最大值，一个最小值
               (values (max mx it) (min mn it))) ;; 所以这里返回比较后的大的值和小的值
             (values (car args) (car args))
             (cdr args))))


