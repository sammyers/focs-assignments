#lang racket

;;; Student Name: Frankly Olin [change to your name]
;;;
;;; Check one:
;;; [X] I completed this assignment without assistance or external resources.
;;; [ ] I completed this assignment with assistance from ___
;;;     and/or using these external resources: ___

;;;;;;;;;;;
;; 1. assq

;; `assq` is a function that takes a key and an association list.
;;
;; It returns the corresponding key/value pair from the list
;; (*i.e.*, the pair whose key is *eq?* to the one it is given).
;;
;; If the key is not found in the list, `assq` returns `#f`.

(define (my-filter test lst)
  (if (empty? lst)
      lst
      (if (test (first lst))
          (cons (first lst) (my-filter test (rest lst)))
          (my-filter test (rest lst))
      )
  )
)

(define operator-list
  (list (list 'ADD +)
        (list 'SUB -)
        (list 'MUL *)
        (list 'DIV /)
        (list 'GT >)
        (list 'LT <)
        (list 'GE >=)
        (list 'LE <=)
        (list 'EQ =)
        (list 'NEQ (lambda (x y) (not (= x y))))
        (list 'ANND (lambda (x y) (and x y)))
        (list 'ORR (lambda (x y) (or x y)))
        (list 'NOTT not)))

(define (has-key key)
  (lambda (lst) (eq? (first lst) key))
)

(define (assq key lst)
  (let ([match (my-filter (has-key key) lst)])
    (if (empty? match)
        #f
        (first match))
  )
)

(assq 'ADD operator-list)

;;;;;;;;;;;
;; 2. lookup-list

;; Add the ability to look up symbols to your evaluator.
;;
;; Add the `lookup-list` argument to your hw2 evaluator (or ours, from the solution set).
;; `(evaluate 'foo lookup-list)` should return whatever `'foo` is associated with in `lookup-list`.

(define (basic-calculate operator operand1 operand2)
  (cond [(eq? operator 'ADD)
         (+ operand1 operand2)]
        [(eq? operator 'SUB)
         (- operand1 operand2)]
        [(eq? operator 'MUL)
         (* operand1 operand2)]
        [(eq? operator 'DIV)
         (/ operand1 operand2)]
        [(eq? operator 'GT)
         (> operand1 operand2)]
        [(eq? operator 'LT)
         (< operand1 operand2)]
        [(eq? operator 'GE)
         (>= operand1 operand2)]
        [(eq? operator 'LE)
         (<= operand1 operand2)]
        [(eq? operator 'EQ)
         (= operand1 operand2)]
        [(eq? operator 'NEQ)
         (not (= operand1 operand2))]
        [(eq? operator 'ANND)
         (and operand1 operand2)]
        [(eq? operator 'ORR)
         (or operand1 operand2)]
  )
)

(define (evaluate x lookup-list)
  (cond [(null? x)
         x]
        [(number? x)
         x]
        [(symbol? x)
         (second (assq x lookup-list))]
        [else
         (cond [(= (length x) 3)
                (basic-calculate (first x) (evaluate (second x) lookup-list) (evaluate (third x) lookup-list))]       
               [(= (length x) 2)
                (not (evaluate (second x) lookup-list))]
               [(= (length x) 4)
                (let ([conditional (second x)]
                      [true-expr (third x)]
                      [false-expr (fourth x)])
                  (if (evaluate conditional lookup-list)
                      (evaluate true-expr lookup-list)
                      (evaluate false-expr lookup-list)
                  )
                )]
         )]
  )
)

(evaluate '(NOTT (ANND (GT (ADD w x) (MUL y z)) (LE (ADD w (MUL x y)) (SUB 0 (SUB (ADD w x) (MUL y z)))))) '((w 3) (x 4) (y 5) (z 6)))