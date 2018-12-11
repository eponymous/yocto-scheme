; This is a init file for Mini-Scheme.

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

(define (unzip1-with-cdr . lists)
  (unzip1-with-cdr-iterative lists '() '()))

(define (unzip1-with-cdr-iterative lists cars cdrs)
  (if (null? lists)
      (cons cars cdrs)
      (let ((car1 (caar lists))
            (cdr1 (cdar lists)))
        (unzip1-with-cdr-iterative
          (cdr lists)
          (append cars (list car1))
          (append cdrs (list cdr1))))))


(define (map proc . lists)
  (if (null? lists)
      (apply proc)
      (if (null? (car lists))
        '()
        (let* ((unz (apply unzip1-with-cdr lists))
               (cars (car unz))
               (cdrs (cdr unz)))
          (cons (apply proc cars) (apply map (cons proc cdrs)))))))

(define (for-each proc . lists)
  (if (null? lists)
      (apply proc)
      (if (null? (car lists))
        #t
        (let* ((unz (apply unzip1-with-cdr lists))
               (cars (car unz))
               (cdrs (cdr unz)))
          (apply proc cars) (apply map (cons proc cdrs))))))


;; from stalin.sc
;; note: Support for multiple arguments incurs a penalty here.
(define (gcd . ns)
    (cond ((null? ns) 0)
        ((null? (cdr ns)) (abs (car ns)))
        (else
          (let loop ((n1 (abs (car ns)))
                     (n2 (abs (car (cdr ns))))
                     (ns (cdr (cdr ns)))
                     (p? (or (inexact? (car ns)) (inexact? (car (cdr ns))))))
            (if (zero? n2)
                (if (null? ns)
                    (if p? (exact->inexact n1) n1)
                    (loop n1 (abs (car ns)) (cdr ns) (or p? (inexact? (car ns)))))
                (let ((r (remainder n1 n2)))
                  (if (zero? r)
                      (if (null? ns)
                          (if p? (exact->inexact n2) n2)
                          (loop n2 (abs (car ns)) (cdr ns) (or p? (inexact? (car ns)))))
                      (loop n2 r ns p?))))))))

;; from stalin.sc
;; note: Support for multiple arguments incurs a penalty here.
(define (lcm . ns)
  (cond ((null? ns) 1)
        ((null? (cdr ns)) (abs (car ns)))
        (else
          (let loop ((n1 (abs (car ns)))
                     (n2 (abs (car (cdr ns))))
                     (ns (cdr (cdr ns)))
                     (p? (or (inexact? (car ns)) (inexact? (car (cdr ns))))))
            (let ((n (cond ((= n1 n2) n1)
                           ((zero? (remainder n1 n2)) n1)
                           ((zero? (remainder n2 n1)) n2)
                           (else (* (quotient n1 (gcd n1 n2)) n2)))))
              (if (null? ns)
                  (if p? (exact->inexact n) n)
                  (loop n (abs (car ns)) (cdr ns) (or p? (inexact? (car ns))))))))))


;; The following quasiquote macro is due to Eric S. Tiedemann.
;;   Copyright 1988 by Eric S. Tiedemann; all rights reserved.
;;
;; Subsequently modified to handle vectors: D. Souflis

(macro quasiquote
 (lambda (l)
   (define (mcons f l r)
     (if (and (pair? r)
              (eq? (car r) 'quote)
              (eq? (car (cdr r)) (cdr f))
              (pair? l)
              (eq? (car l) 'quote)
              (eq? (car (cdr l)) (car f)))
         (if (or (procedure? f) (number? f) (string? f))
               f
               (list 'quote f))
         (if (eqv? l vector)
               (apply l (eval r))
               (list 'cons l r)
               )))
   (define (mappend f l r)
     (if (or (null? (cdr f))
             (and (pair? r)
                  (eq? (car r) 'quote)
                  (eq? (car (cdr r)) '())))
         l
         (list 'append l r)))
   (define (foo level form)
     (cond ((not (pair? form))
               (if (or (procedure? form) (number? form) (string? form))
                   form
                   (list 'quote form)))
           ((eq? 'quasiquote (car form))
               (mcons form ''quasiquote (foo (+ level 1) (cdr form))))
           (else (if (zero? level)
                     (cond ((eq? (car form) 'unquote) (car (cdr form)))
                           ((eq? (car form) 'unquote-splicing)
                               (error "Unquote-splicing wasn't in a list:" form))
                           ((and (pair? (car form))
                                 (eq? (car (car form)) 'unquote-splicing))
                               (mappend form (car (cdr (car form))) (foo level (cdr form))))
                           (else (mcons form (foo level (car form)) (foo level (cdr form)))))
                     (cond ((eq? (car form) 'unquote)
                               (mcons form ''unquote (foo (- level 1) (cdr form))))
                           ((eq? (car form) 'unquote-splicing)
                               (mcons form ''unquote-splicing (foo (- level 1) (cdr form))))
                           (else (mcons form (foo level (car form)) (foo level (cdr form)))))))))
   (foo 0 (car (cdr l)))))


;; (do ((var init inc) ...) (endtest result ...) body ...)
(macro do
  (lambda (do-macro)
    (apply (lambda (do vars endtest . body)
             (let ((do-loop (gensym)))
               `(letrec ((,do-loop
                           (lambda ,(map (lambda (x)
                                           (if (pair? x) (car x) x))
                                      `,vars)
                             (if ,(car endtest)
                               (begin ,@(cdr endtest))
                               (begin
                                 ,@body
                                 (,do-loop
                                   ,@(map (lambda (x)
                                            (cond
                                              ((not (pair? x)) x)
                                              ((< (length x) 3) (car x))
                                              (else (car (cdr (cdr x))))))
                                       `,vars)))))))
                  (,do-loop
                    ,@(map (lambda (x)
                             (if (and (pair? x) (cdr x))
                               (car (cdr x))
                               nil))
                        `,vars)))))
      do-macro)))

