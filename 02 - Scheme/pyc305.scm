;; Course: NYU Programming Languages
;; Title: Scheme Programming Assignment
;; Name: Peng-Yu Chen
;; NetID: pyc305

;; Problem 01
;; (fromTo k n)
;;    returns the list of integers from k to n. The size of the problem can be
;;    seen as the number of integers between k and n, inclusive.
;; Base Case:
;;    if k > n (i.e. if the size of the problem is 0), then the result is the
;;    empty list.
;; Hypothesis:
;;    Assume (fromTo (+ k 1) n) returns the list of integers from k + 1 to n,
;;    since the size of that problem is one less than the size of the orignal
;;    problem, (fromTo k n).
;; Recursive Step:
;;    (fromTo k n) = (cons k (fromTo (+ k 1) n)
;; Example:
;;    (fromTo 3 8) -> (3 4 5 6 7 8)

(define (fromTo k n)
  (if (> k n) '()
  (cons k (fromTo (+ k 1) n))))

;; Problem 02
;; (removeMults m L)
;;    returns a list containing all the elements of L that are not multiples of m
;; Base Case:
;;    if L is empty, then the result is the empty list
;; Hypothesis:
;;    Assume (removeMults m (cdr L)) return the list of integers except the first
;;    element in the list L that are not multiples of m
;; Recursive Step:
;;    (removeMults m L)
;;      = (removeMults m (cdr L))               , if (car L) % m == 0
;;      = (cons (car L) (removeMults m (cdr L))), otherwise
;;
;; Example:
;;    (removeMults 3 '(2 3 4 5 6 7 8 9 10)) -> (2 4 5 7 8 10)

(define (removeMults m L)
  (cond ((null? L) '())
        ((= (modulo (car L) m) 0) (removeMults m (cdr L)))
        (else (cons (car L) (removeMults m (cdr L))))))

;; Problem 03
;; (removeAllMults L)
;;    given a list L containing integers in strictly increasing order, returns a
;;    list containing those elements of L that are not multiples of each other.
;; Base Case:
;;    if L == null, then the result is the empty list
;; Hypothesis:
;;    Assume (removeAllMults L') which, given a list L' containing integers in
;;    strictly increasing order, returns a list containing those elements of L'
;;    that are not multiples of each other. (L': the list that contains elements
;;    originally in L but are not a multiple of (car L))
;; Recursive Step:
;;    (removeAllMults L)
;;      = (cons (car L) (removeAllMults L'))
;;      = (cons (car L) (removeAllMults (removeMults (car L) (cdr L)))
;; Example:
;;    (removeAllMults '(3 4 6 7 8 10 12 15 20 22 23)) -> (3 4 7 10 22 23)

(define (removeAllMults L)
  (if (null? L) '()
  (cons (car L) (removeAllMults (removeMults (car L) (cdr L))))))

;; Problem 04
;; (primes L)
;;    computes the list of all primes less than or equal to n
;; Example:
;;    (primes 30) -> (2 3 5 7 11 13 17 19 23 29)

(define (primes n)
  (removeAllMults (fromTo 2 n)))

;; Problem 05
;; (maxDepth L)
;;    where L is a list, that returns the maximum nesting depth of any element
;;    within L, such that the topmost elements are at depth 0.
;; Base Case:
;;    if L is empty, then the result is the 0
;; Recursive Step:
;;    (maxDepth L)
;;      = (max (+ 1 (maxDepth (car L))) (maxDepth (cdr L))), if (car L) is a list
;;      = (maxDepth (cdr L))                               , otherwise
;; Example:
;;    (maxDepth '(1 2 3)) -> 0
;;    (maxDepth '((0 1) (2 (3 (4 5 (6 (7 8) 9) 10) 11 12) 13) (14 15))) -> 5

(define (maxDepth L)
  (cond ((null? L) 0)
        ((list? (car L))
         (max (+ 1 (maxDepth (car L)))
              (maxDepth (cdr L))))
        (else (maxDepth (cdr L)))))

;; Problem 06
;; (prefix exp)
;;    transforms an infix arithmetic expression exp into prefix notation
;; Base Case:
;;    if exp is a number, then the result is the number
;;    if exp is a symbol, then the result is the symbol
;;    if (cdr exp) is empty, then the result is (car exp)
;; Recursive Step:
;;    (prefix exp)
;;      = (list operator (prefix operand1) (prefix operand2)),
;;        where operator = (cadr exp),
;;              operand1 = (car exp),
;;              operand2 = (cddr exp)
;; Examples:
;;    (prefix '((3 + 4) * 5)) -> (* (+ 3 4) 5)
;;    (prefix '(3 + 4 * 5 - 6)) -> (+ 3 (* 4 (- 5 6)))
;;    (prefix '((3 * 4) + (5 - 6) * 7)) -> (+ (* 3 4) (* (- 5 6) 7))

(define (prefix exp)
  (cond
    ((number? exp) exp)
    ((symbol? exp) exp)
    ((null? (cdr exp)) (car exp))
    ((let ((operand1 (car exp))
           (operator (cadr exp))
           (operand2 (cddr exp)))
       (list operator
             (prefix operand1)
             (prefix operand2))))))

;; Problem 07
;; (composition fns)
;;    takes a list of functions fns and returns a function that is the composition
;;    of the functions in fns.
;; Base Case:
;;    if fns is empty, then the result is x
;; Recursive Step:
;;    define a sub-function (f fns x) in (composition fns) which takes two
;;    paramters fns (functions) and x (input value), f recursively compute
;;    f1 (f (f2..fn)). So, (composition fns) = (lambda (x) (f fns x))
;; Example:
;;    (define f (composition
;;               (list (lambda (x) (+ x 1))
;;                     (lambda (x) (* x 2))
;;                     (lambda (x) (- x 5)))))
;;    (f 8) -> 7

(define (composition fns)
  (define (f fns x)
    (if (null? fns) x
        ((car fns) (f (cdr fns) x))))
  (lambda (x) (f fns x)))

;; Problem 08
;; (bubble-to-nth L N)
;;    where L is a list of numbers and N is an integer. The result should be a
;;    list containing all the elements of L, except that the largest element
;;    among the first N elements of L is now the Nth element of the resulting
;;    list, and the elements after the Nth element are left in their original order.
;; Base Case:
;;    if N == 1, then the result is L
;; Recursive Step:
;;    (bubble-to-nth L N)
;;      = (cons (cadr L) (bubble-to-nth (cons (car L) (cddr L)) (- N 1))),
;;        if (> (car L) (cadr L))
;;      = (cons (car L) (bubble-to-nth (cdr L) (- N 1))), otherwise;;
;; Examples:
;;    (bubble-to-nth '(1 6 2 3 5 4 8 0) 3) -> (1 2 6 3 5 4 8 0)
;;    (bubble-to-nth '(1 6 2 3 5 4 8 0) 4) -> (1 2 3 6 5 4 8 0)
;;    (bubble-to-nth '(1 6 2 3 5 4 8 0) 5) -> (1 2 3 5 6 4 8 0)
;;    (bubble-to-nth '(1 6 2 3 5 4 8 0) 6) -> (1 2 3 5 4 6 8 0)
;;    (bubble-to-nth '(1 6 2 3 5 4 8 0) 7) -> (1 2 3 5 4 6 8 0)
;;    (bubble-to-nth '(1 6 2 3 5 4 8 0) 8) -> (1 2 3 5 4 6 0 8)

(define (bubble-to-nth L N)
  (cond
    ((= N 1) L)
    ((> (car L) (cadr L)) (cons (cadr L) (bubble-to-nth (cons (car L) (cddr L)) (- N 1))))
    (else (cons (car L) (bubble-to-nth (cdr L) (- N 1))))))

;; Problem 09
;; (b-s L N)
;;    where L is a list of numbers and N is an integer, that returns the a list
;;    containing the elements of L in their original order except that the first
;;    N elements are in sorted order.
;; Base Case:
;;    if N == 0, then the result is L
;; Recursive Step:
;;    (b-s L N)
;;      = (b-s (bubble-to-nt L N) (- N 1))

(define (b-s L N)
  (if (= N 0) L
      (b-s (bubble-to-nth L N) (- N 1))))

;; Problem 10
;; (bubble-sort L)
;;    calls b-s above to return a list of the elements of L in sorted order.

(define (bubble-sort L)
  (b-s L (length L)))
