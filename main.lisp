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
(mvdo ((x 1 (1+ x))
       ((y z) (values 0 0) (values z x)))
    ((> x 5) (list x y z))
  (princ (list x y z)))

;; macro expand to:



;; 不同于mvdo*, mvdo需要一次性将参数初始form提取出来，采用mvpsetq初始赋值， 这样防止参数赋值前后顺序影响，
;; 另外, 由于含有多值赋值，所以需要用到上面定义的psetq的多值版本mvpsetq。
;; 同样的，原来的rebind form采用多个 setq/multiple-value-bind 前后依次更新值，现在也需要mvpsetq 来并行更新值了
;; 所以整体就没有递归调用生成了，因为不需要嵌套了

;; TODO
;; (defmacro mvdo (parm-cl test-cl &body body)
;;   (let* ((label (gensym))
;;          (temp-cl (mapcar #'(lambda (p)
;;                               (list (if (listp (first p))
;;                                         (mapcar #'(lambda (x) (list x (gensym))) (first p))
;;                                         (list (first p) (gensym)))
;;                                     (second p)))
;;                           parm-cl))
;;          (prog-cl (mapcar #'first temp-cl))
;;          (mvpsetq-cl (mapcar #'(lambda (p)     (list (if (listp (second (first p))))
;;                                                 (second p)))
;;                              temp-cl))
;;          (let-cl (flatten-1 (mapcar #'(lambda (p) (first p))
;;                                     prog-cl))))

;;     `(let (,@let-cl)
;;        ,temp-cl
;;        (mvpsetq ,mvpsetq-cl)
;;        (prog (,@prog-cl) ;; TODO init bind
;;           ,label
;;           ,@body
;;           (if ,(car test-cl)
;;               (return (progn ,@(cdr test-cl))))
;;           ;; todo ebind
;;           (go ,label)))))

;; macro expand to
;; (let (#:g1 #:g2 #:g3)
;;   (mvpsetq #:g1 1 (#:g2 #:g3) (values 0 0))
;;   (prog ((x #:g1) (y #:g2) (z #:g3))
;;      #:label
;;    ..body
;;    test

;;      (mvpsetq #:g1 (1+ x) (#:g2 #:g3) (values z x))
;;      (go #:label)
;;      ))
