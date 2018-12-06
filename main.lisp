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
           #:mvdo*))

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

;;; Chapter 11.5
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

