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

(length (permutations2 '((a b c) (x y z)))) ; 20

; (sleep 10)
(define str "")
(thread (lambda ()
          (for ([i 10])
            (string-append str "thread 1\n"))))

(thread (lambda ()
          (for ([i 20])
            (sleep 1)
            (printf "thread 2\n"))))
(define x 10)
(thread (lambda () (set! x (* x x))))
(thread (lambda () (set! x (+ x 1))))
x

; see
; SICP 3.4.2 Mechanisms for Controlling Concurrency - プログラミング再入門 -
; http://d.hatena.ne.jp/tetsu_miyagawa/20130929/1380407280

(require racket/list)
(define (parallel-execute . procs)
  (for-each thread-wait
            (map (lambda (p)
                   (thread p))
                 (shuffle procs))))
(define (parallel-test)
  (let ((x 10))
    (parallel-execute (lambda () (set! x (* x x)))
                      (lambda () (set! x (+ x 1))))
    x))
(define (iterate-test x res)
  (if (zero? x)
      res
      (iterate-test (- x 1)
                    (let ((r (parallel-test)))
                      (if (memq r res)
                          res
                          (cons r res))))))
(iterate-test 10 '())
