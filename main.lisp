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

;; Macro `_f`
(defmacro _f (op place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    `(let* (,@(mapcar #'list vars forms)
            (,(car var) (,op ,access ,@args)))
       ,set)))

;; Macro `conc1f` created with `_f`
;; name with `'` to avoid name conflcit
(defmacro conc1f' (lst obj)
  `(_f nconc ,lst (list ,obj)))


;; Macro `pull`
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



;; Macro `pull-if` use case
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
              (,(car var) (delete-if ,g ,access ,@args)))  ;; the different with pull is here
         ,set))))


