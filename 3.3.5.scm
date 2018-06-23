#lang racket

; SICP 3.3.5  Propagation of Constraints 
; http://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-22.html#%_sec_3.3.5

(require racket/trace)

;; constrains
(define (make-constraint l process-new-value)
  (define (constraint request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value constraint))
          ((eq? request 'I-lost-my-value)
            (begin
              (for-each (lambda (connector) (forget-value! connector constraint)) l)
              (process-new-value constraint)))
          (else (error "make-constraint : Unknown request" request))))
  (for-each (lambda (connector) (connect connector constraint)) l)
  constraint)

(define (three-arguments a b c op1 op2)
  (lambda (constraint)
    (cond ((and (has-value? a) (has-value? b))
           (set-value! c (op1 (get-value a) (get-value b)) constraint))
          ((and (has-value? c) (has-value? a))
           (set-value! b (op2 (get-value c) (get-value a)) constraint))
          ((and (has-value? c) (has-value? b))
           (set-value! a (op2 (get-value c) (get-value b)) constraint)))))

(define (adder a1 a2 sum)
  (make-constraint (list a1 a2 sum) (three-arguments a1 a2 sum + -)))

(define (multiplier m1 m2 product)
  (make-constraint (list m1 m2 product) (three-arguments m1 m2 product * /)))

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
          ((eq? request 'I-lost-my-value)
           ((lambda () (print-probe "?"))))
          (else "probe : Unknown request" request)))
  (connect connector probe)
  probe)

;; connector
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
            ((eq? request 'forget-value!)
             (lambda (retractor)
               (if (eq? retractor informant)
                   (begin (set! informant #f)
                          (for-each-exept retractor inform-about-no-value constrains))
                   'ignored)))
            ((eq? request 'has-value?) (if informant #t #f))
            ((eq? request 'value) value)
            ((eq? request 'constrains) constrains)
            (else (error "connector : Unknown request" request))))
    connector))

(define (for-each-exept exception procedure list)
  (for-each (lambda (item)
              (cond ((not (eq? item exception))
                     (procedure item))))
            list))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector informant)
  ((connector 'forget-value!) informant))

(define (has-value? connector)
  (connector 'has-value?))

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (get-value connector)
  (connector 'value))

;; test
(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-converter C F)
(probe "Celisius temp" C)
(probe "Fahrenheit temp" F)
(set-value! C 25 'user)
; (Probe :  Celisius temp  =  25)
; (Probe :  Fahrenheit temp  =  77)

(forget-value! C 'user)
; (Probe :  Celisius temp  =  ?)
; (Probe :  Fahrenheit temp  =  ?)

(set-value! F 212 'user)
; (Probe :  Fahrenheit temp  =  212)
; (Probe :  Celisius temp  =  100)

;; # English memo
;; - contradiction : 矛盾
;; - constraint : 制約
;; - prove : 証明
;; - probe : 探索する
;; - retractor : 〔手術に用いる〕開創器、開創鉤

;; ex 3.33
(define (averager a b c)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector)))
    (adder a b u)
    (multiplier w c u)
    (constant 2 w)
    'ok))

(let ((a (make-connector))
      (b (make-connector))
      (c (make-connector)))
  (averager a b c)
  (for-each (lambda (a) (probe (car a) (cdr a)))
            `((a . ,a) (b . ,b) (c . ,c)))
  (set-value! a 70 'user)
  (set-value! b 30 'user))

;; ex 3.34
(define (squere a b)
  (multiplier a a b))

(let ((a (make-connector))
      (b (make-connector)))
  (for-each (lambda (a) (probe (car a) (cdr a)))
            `((a . ,a) (b . ,b)))
  (squere a b)
  (set-value! b 3 'user))
; (Probe :  b  =  3)

;; ex 3.35
(define (squere a b)
  (make-constraint
    (list a b)
    (lambda (constraint)
      (cond ((has-value? a)
             (set-value! b (* (get-value a) (get-value a)) constraint))
            ((has-value? b)
             (set-value! a (sqrt (get-value b)) constraint))))))

; (Probe :  a  =  1.7320508075688772)
; (Probe :  b  =  3)
