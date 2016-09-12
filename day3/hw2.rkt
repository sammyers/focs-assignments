#lang racket

;;; Student Name: Sam Myers
;;;
;;; Check one:
;;; [X] I completed this assignment without assistance or external resources.
;;; [ ] I completed this assignment with assistance from ___
;;;     and/or using these external resources: ___

;;; 1.  Create a calculator that takes one argument: a list that represents an expression.

(define (basic-calculate x)
  (let ([operator (first x)]
        [operand1 (second x)]
        [operand2 (third x)])
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
)

(define (calculate x)
  (cond [(= (length x) 3)
         (let ([operator (first x)]
               [operand1 (second x)]
               [operand2 (third x)])
           (cond [(and (list? operand1) (not (list? operand2)))
                  (basic-calculate (list operator (calculate operand1) operand2))]
                 [(and (not (list? operand1)) (list? operand2))
                  (basic-calculate (list operator operand1 (calculate operand2)))]
                 [(and (list? operand1) (list? operand2))
                  (basic-calculate (list operator (calculate operand1) (calculate operand2)))]
                 [else
                  (basic-calculate x)]
                 )
           )]
        [(= (length x) 2)
         (not (calculate (second x)))]
        [(= (length x) 4)
         (let ([conditional (second x)]
                [true-expr (third x)]
                [false-expr (fourth x)])
            (if (calculate conditional)
                (calculate true-expr)
                (calculate false-expr)
            )
          )]
  )
)

(calculate '(ADD 3 4)) ;; --> 7

;;; 2. Expand the calculator's operation to allow for arguments that are themselves well-formed arithmetic expressions.

(calculate '(ADD 3 (MUL 4 5))) ;; --> 23   ;; what is the equivalent construction using list?
(calculate '(SUB (ADD 3 4) (MUL 5 6))) ;; --> -23

;;; 3. Add comparators returning booleans (*e.g.*, greater than, less than, …).
;; Note that each of these takes numeric arguments (or expressions that evaluate to produce numeric values),
;; but returns a boolean.  We suggest operators `GT`, `LT`, `GE`, `LE`, `EQ`, `NEQ`.

	(calculate '(GT (ADD 3 4) (MUL 5 6))) ;; --> #f
	(calculate '(LE (ADD 3 (MUL 4 5)) (SUB 0 (SUB (ADD 3 4) (MUL 5 6))))) ;; --> #t

;;; 4. Add boolean operations ANND, ORR, NOTT

(calculate '(ANND (GT (ADD 3 4) (MUL 5 6)) (LE (ADD 3 (MUL 4 5)) (SUB 0 (SUB (ADD 3 4) (MUL 5 6)))))) ;; --> #f

;;; 5. Add IPH

(calculate '(IPH (GT (ADD 3 4) 7) (ADD 1 2) (ADD 1 3))) ;; -> 4
