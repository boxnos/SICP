(define (withdraw amount)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(define balance 100)
(withdraw 25)
(withdraw 25)

; ex 3.38 a
(define (Peter balance)
  (+ balance 10))
(define (Paul balance)
  (- balance 20))
(define (Merry balance)
  (- balance (/ balance 2)))

(require racket/list)
(for-each
  (lambda (l)
    (display (format "~a ~a\n" l
                     (fold (lambda (x acc) (eval (list x acc))) 100 l))))
  (permutations '(Peter Paul Merry)))

; (Peter Paul Merry) 45
; (Paul Peter Merry) 45
; (Peter Merry Paul) 35
; (Merry Peter Paul) 40
; (Paul Merry Peter) 50
; (Merry Paul Peter) 40
  (if (>= barance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(define balance 100)
(withdraw 25)
(withdraw 25)

; ex 3.38 a
(define (Peter balance)
  (+ balance 10))
(define (Paul balance)
  (- balance 20))
(define (Merry balance)
  (- balance (/ balance 2)))

(require racket/list)
(for-each
  (lambda (l)
    (display (format "~a ~a\n" l
                     (fold (lambda (x acc) (eval (list x acc))) 100 l))))
  (permutations '(Peter Paul Merry)))

; (Peter Paul Merry) 45
; (Paul Peter Merry) 45
; (Peter Merry Paul) 35
; (Merry Peter Paul) 40
; (Paul Merry Peter) 50
; (Merry Paul Peter) 40

; ex. 3.38 b
(define (permutations ls)
  (define (loop l a b)
    (if (null? l)
        (cons (reverse a) b)
        (fold-right
          (lambda (x y)
            (loop (remove (lambda (a) (eq? a x)) l)
                  (cons x a) y)) b l)))
  (loop ls '() '()))

(define (permutations2 ls)
  (define (loop l a b)
    (define (rm x) (remove (lambda (a) (eq? a x)) l))
    (define (rm-list x)
      (if (null? (cdr x))
          (rm x)
          (cons (cdr x) (rm x))))
    (if (null? l)
        (cons (reverse a) b)
        (fold-right
          (lambda (x y)
            (loop (rm-list x)
                  (cons (car x) a) y)) b l)))
  (loop ls '() '()))

(length (permutations2 '((a aa) (b bb) (c cc)))) ; 90

(for-each
  (lambda (l)
    (display (format "~a~%" l)))
  (permutations2 '((a aa) (b bb) (c cc))))
; (a aa b bb c cc)
; (a aa b c cc bb)
; (a aa b c bb cc)
; (a aa c cc b bb)
; ...

(define (make-person action)
  (let ((balance #f))
    (lambda (state b)
      (if (eq? state 'withdraw)
          (begin
            (set! balance (action b))
            balance)
          balance))))

(define Peter (make-person (lambda (balance) (+ balance 10))))
(define Paul (make-person (lambda (balance) (- balance 20))))
(define Merry (make-person (lambda (balance) (/ balance 2))))

(for-each
  (lambda (l)
    (display (format "~a ~a~%" l
                     (fold (lambda (x acc) (eval (append x (list acc)))) 100 l))))
  (permutations2 '(((Peter 'withdraw) (Peter 'set))
                   ((Paul 'withdraw) (Paul 'set))
                   ((Merry 'withdraw) (Merry 'set)))))
; ((Peter (quote withdraw)) (Peter (quote set)) (Paul (quote withdraw)) (Paul (quote set)) (Merry (quote withdraw)) (Merry (quote set))) 45
; ((Peter (quote withdraw)) (Peter (quote set)) (Paul (quote withdraw)) (Merry (quote withdraw)) (Merry (quote set)) (Paul (quote set))) 90
; ((Peter (quote withdraw)) (Peter (quote set)) (Paul (quote withdraw)) (Merry (quote withdraw)) (Paul (quote set)) (Merry (quote set))) 45
; ((Peter (quote withdraw)) (Peter (quote set)) (Merry (quote withdraw)) (Merry (quote set)) (Paul (quote withdraw)) (Paul (quote set))) 35
; ((Peter (quote withdraw)) (Peter (quote set)) (Merry (quote withdraw)) (Paul (quote withdraw)) (Paul (quote set)) (Merry (quote set))) 55
; ((Peter (quote withdraw)) (Peter (quote set)) (Merry (quote withdraw)) (Paul (quote withdraw)) (Merry (quote set)) (Paul (quote set))) 35
; ((Peter (quote withdraw)) (Paul (quote withdraw)) (Paul (quote set)) (Peter (quote set)) (Merry (quote withdraw)) (Merry (quote set))) 55
; ...
