#lang racket

(define (make-constraint a b c op1 op2)
  (lambda (request)
    (cond ((eq? request 'I-have-a-value)
           (cond ((and (has-value? a) (has-value? b))
                  (set-value! c (op1 (get-value a) (get-value b)) adder))
                 ((and (has-value? c) (has-value? a))
                  (set-value! b (op2 (get-value c) (get-value a)) adder))
                 ((and (has-value? c) (has-value? b))
                  (set-value! a (op3 (get-value c) (get-value b)) adder)))))))

(define (adder a1 a2 sum)
  (let ((adder (make-constraint a1 a2 sum + -)))
    (connect a1 adder)
    (connect a2 adder)
    (connect sum adder)
    adder))

(define (make-connector)
  (let ((value #f)
        (informant #f)
        (constrains '()))
    (define (connector request)
      (cond ((eq? request 'connect)
             (lambda (new-constraint)
               (cond ((not (memq new-constraint constrains))
                      (set! constrains (cons new-constraint constrains))))
               (cond ((has-value? connector)
                      (inform-about-value new-constraint)))))
            ((eq? request 'set-value!)
             (lambda (new-value setter)
               (cond ((not (has-value? connector))
                      (set! value new-value)
                      (set! informant setter)
                      (for-each-exept setter inform-about-value constrains))
                     ((not (eq? value new-value))
                      (error "connector : Contradiction" (list value new-value)))
                     (else 'ignored))))
            ((eq? request 'has-value?) (if informant #t #f))
            ((eq? request 'value) value)
            (else (error "connector : Unknown request" request))))
    connector))

(define (for-each-exept exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else
            (procedure (car items))
            (loop (cdr items)))))
  (loop list))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (has-value? connector)
  (connector 'has-value?))

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (get-value connector)
  (connector 'value))

(define (constant value connector)
  (define (const request)
    (error "constant : Unknown request" request))
  (connect connector const)
  (set-value! connector value const)
  const)

(define (probe name connector)
  (define (print-probe value)
    (display (list "Probe : " name " = " value))
    (newline))
  (define (probe request)
    (cond ((eq? request 'I-have-a-value)
           ((lambda () (print-probe (get-value connector)))))
          (else "probe : Unknown request" request)))
  (connect connector probe)
  probe)

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (y (make-connector)))
    (constant 9 w)
    'ok))

(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-converter C F)

;; test
(C 'has-value?) ; called has-value?
((C 'set-value!) 9 'hoge)
(C 'value)

(define (adder-test a b)
  (let ((c (make-connector)))
    (adder a b c)
    (constant 4 c)
    'ok))
(define A (make-connector))
(define B (make-connector))
(adder-test A B)
(probe "A" A)
(probe "B" B)
(set-value! A 1 'user)
; (Probe :  A  =  1)
; (Probe :  B  =  3)

;; # English memo
;; - contradiction : 矛盾
;; - constraint : 制約
;; - prove : 証明
;; - probe : 探索する
