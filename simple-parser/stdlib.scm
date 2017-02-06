(define (not x)            (if x #f #t))
(define (null? ls)         (if (eqv? ls '()) #t #f))

(define (list . objs)      objs)

(define (id obj)           obj)
(define (flip func)        (lambda (arg1 arg2) (func arg2 arg1)))
(define (curry func arg1)  (lambda (argv) (apply func (cons arg1 (list argv)))))
(define (compose f g)      (lambda (argv) (f (apply g argv))))

(define zero?              (curry = 0))
(define positive?          (curry < 0))
(define negative?          (curry > 0))
(define (odd? num)         (= (mod num 2) 1))
(define (even? num)        (= (mod num 2) 0))

(define (foldr func acc lst)
  (if (null? lst)
    acc
    (func (car lst) (foldr func acc (cdr lst)))))

(define (foldl func acc lst)
  (if (null? lst)
    acc
    (foldl func (func acc (car lst)) (cdr lst))))

(define fold foldl)
(define reduce foldr)

(define (unfold func init pred)
  if (pred init)
    (cons init '())
    (cons init (unfold func (func init) pred)))

(define (sum . lst)         (fold + 0 lst))
(define (product . lst)     (fold * 1 lst))
(define (and . lst)         (fold && #t lst))
(define (or . lst)          (fold || #f lst))

(define (max first . rest)  (fold (lambda (acc item) (if (> acc item) acc item)) first rest))
(define (min first . rest)  (fold (lambda (acc item) (if (< acc item) acc item)) first rest))

(define (length lst)        (fold (lambda (acc _) (+ 1 acc)) 0 lst))
(define (reverse lst)       (fold (lambda (acc item) (cons item acc)) '() lst))

(define (map fn lst)        (reduce (lambda (item acc) (cons (fn item) acc)) '() lst))
(define (filter pred lst)   (reduce (lambda (item acc) (if (pred item)
                                                         (cons item acc)
                                                         acc)) '() lst))

(define (mem-helper pred op)
  (lambda (acc item) (if (and (not acc) (pred (op item)))
                       item
                       acc)))

(define (memq obj lst)    (fold (mem-helper (curry eq? obj)    id)  #f lst))
(define (memv obj lst)    (fold (mem-helper (curry eqv? obj)   id)  #f lst))
(define (member obj lst)  (fold (mem-helper (curry equal? obj) id)  #f lst))
(define (assq obj lst)    (fold (mem-helper (curry eq? obj)    car) #f lst))
(define (assv obj lst)    (fold (mem-helper (curry eqv? obj)   car) #f lst))
(define (assoc obj lst)   (fold (mem-helper (curry equal? obj) car) #f lst))

