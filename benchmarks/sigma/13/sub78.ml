(define (leaf x) (cons 'leaf x))
(define (node x) (if (list? x) x ("Error 1: input is not list")))
(define (is-empty-tree? t) (eq? t `()))
(define (is-leaf? t) (and (pair? t) (eq? 'leaf (car t))))
(define (leaf-val t) (if (is-leaf? t) (cdr t) "Error 2: input is not leaf"))
(define (nth-child t n) (if (= n 0) (car t)
                          (nth-child (cdr t) (- n 1))))

(define (model x) (leaf x))
(define (make-branch d m) (node (list (leaf 'b) (leaf d) m)))
(define (make-mobil l r) (node (list (leaf 'm) l r)))

(define (branch? x) (and (pair? (car x)) (eq? (leaf-val (car x)) 'b)))
(define (mobil? x) (and (pair? (car x)) (eq? (leaf-val (car x)) 'm)))

(define (left-mobil x) (nth-child (nth-child x 1) 2))
(define (right-mobil x) (nth-child (nth-child x 2) 2))
(define (left-length x) (leaf-val (nth-child (nth-child x 1) 1)))
(define (right-length x) (leaf-val (nth-child (nth-child x 2) 1)))

(define (left-torque x) (* (weight (left-mobil x)) (left-length x)))
(define (right-torque x) (* (weight (right-mobil x)) (right-length x)))

(define (is-balanced? x) (cond ((mobil? x) (and (= (left-torque x) (right-torque x)) (is-balanced? (left-mobil x)) (is-balanced? (right-mobil x))))
                              ((is-leaf? x) #t)
                              (else "Error 3: unexpected arg")))

(define (weight x) (cond ((mobil? x) (+ (weight (left-mobil x)) (weight (right-mobil x))))
                         ((is-leaf? x) (leaf-val x))
                         (else "Error 4: unexpected arg")))