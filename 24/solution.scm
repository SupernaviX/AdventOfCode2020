(import srfi-69) ; hash tables

; things that are, somehow, not in a prelude
(define (reduce fn list init)
  (if (null? list) init
    (fn (car list)
      (reduce fn (cdr list) init))))
(define (filter fn list)
  (cond
    ((null? list) '())
    ((fn (car list)) (cons (car list) (filter fn (cdr list))))
    (else (filter fn (cdr list)))))
(define (iterate fn times init)
  (if (= times 0)
    init
    (iterate fn (- times 1) (fn init))))

(define (parse-input filename)
  (define (parse-lines port)
    (let ((line (parse-line port)))
      (if (pair? line)
        (cons line (parse-lines port)))))
  (define (parse-line port)
    (define (ns? char)
      (or (char=? char #\n)
          (char=? char #\s)))
    (define (ew? char)
      (or (char=? char #\e)
          (char=? char #\w)))
    (let ((c (read-char port)))
      (cond
        ((eof-object? c) '())
        ((ns? c) (cons (string c (read-char port)) (parse-line port)))
        ((ew? c) (cons (string c) (parse-line port)))
        (else '()))))
  (call-with-input-file filename
    (lambda (port) (parse-lines port))))

(define (direction->delta direction)
  (cond
    ((string=? direction "ne") '(1 1))
    ((string=? direction "e") '(2 0))
    ((string=? direction "se") '(1 -1))
    ((string=? direction "sw") '(-1 -1))
    ((string=? direction "w") '(-2 0))
    ((string=? direction "nw") '(-1 1))))
(define deltas (map direction->delta (list "ne" "e" "se" "sw" "w" "nw")))
(define (neighbors coordinates)
  (map (lambda (delta) (map + coordinates delta)) deltas))

(define (map+ xs ys)
  (map + xs ys))

(define (directions->coordinates directions)
  (reduce map+ (map direction->delta directions) '(0 0)))

(define (resolve coordinate alist)
  (let ((existing (assoc coordinate alist)))
    (if existing
      (begin
        (set-car! (cdr existing) (+ (cadr existing) 1))
        alist)
      (cons (list coordinate 1) alist))))

(define (initial-black-tiles coordinates)
  (let* ((resolved (reduce resolve coordinates '()))
         (black-tiles (filter
           (lambda (flips) (odd? (cadr flips))) resolved)))
    (map car black-tiles)))

(define (make-hash-set values)
  (let ((johnson (make-hash-table equal? equal?-hash initial: #f)))
    (for-each (lambda (v) (hash-table-set! johnson v #t)) values)
    johnson))
(define hash-set-exists? hash-table-exists?)
(define hash-set-keys hash-table-keys)
(define (hash-set-add! table value)
  (hash-table-set! table value #t))
(define (hash-set-add-many! table values)
  (for-each (lambda (v) (hash-set-add! table v)) values))

(define (count-black-neighbors coords old-tiles)
  (let* ((all-neighbors (neighbors coords))
         (is-black? (lambda (c) (hash-set-exists? old-tiles c))))
    (length (filter is-black? all-neighbors))))
(define (will-be-black? old-tiles coords)
  (let ((count (count-black-neighbors coords old-tiles)))
    (if (hash-set-exists? old-tiles coords)
      (or (= count 1) (= count 2))
      (= count 2))))

(define (update-black-tile! coords old-tiles new-tiles)
  (if (will-be-black? old-tiles coords)
    (hash-set-add! new-tiles coords))
  (let ((all-neighbors (neighbors coords))
        (add? (lambda (c) (will-be-black? old-tiles c))))
    (hash-set-add-many! new-tiles (filter add? all-neighbors))))

(define (update old-tiles)
  (let ((new-tiles (make-hash-set '())))
    (for-each
      (lambda (coords) (update-black-tile! coords old-tiles new-tiles))
      (hash-set-keys old-tiles))
    new-tiles))

(let* ((input (parse-input "input"))
       (coordinates (map directions->coordinates input))
       (black-tiles (initial-black-tiles coordinates)))
  (display (length black-tiles))
  (newline)
  (let ((final-black-tiles (iterate update 100 (make-hash-set black-tiles))))
    (display (length (hash-set-keys final-black-tiles)))
    (newline)))
(exit)