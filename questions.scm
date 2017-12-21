(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
    (if (null? rests) nil
        (cons (append (cons first nil) (car rests)) (cons-all first (cdr rests)))))

(define (all func values)
    (if (null? values) nil (cons (func (car values)) (all func (cdr values)))))

(define (zip pairs)
  (list (all car pairs) (all cadr pairs)))



;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  'replace-this-line
  (define first-length (length s))
  (define (help-enum s)
    (if (null? s)
        nil
        (cons (cons (- first-length (length s)) (cons (car s) nil)) (help-enum (cdr s)))))
  (help-enum s)

  )
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (cond ((null? denoms) cons nil)
      ((= total 0) cons(cons nil nil))
      ((> (car denoms) total) (list-change total (cdr denoms)))
      (else (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms)) (list-change total (cdr denoms))))
    )
  )
  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19

         expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           'replace-this-line
           (cons form (cons params (all let-to-lambda body)))
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           'replace-this-line
           (cons (cons 'lambda (cons (car (zip values)) (all let-to-lambda body))) (all let-to-lambda (cadr (zip values))))
           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
         'replace-this-line
         (cons (car expr) (all let-to-lambda (cdr expr)))
         ; END PROBLEM 19
         )))
