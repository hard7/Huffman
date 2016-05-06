; sicp ex 2.68
; huffman

(define (lenght s)
    (if (null? s) 0 (+ 1 (lenght (cdr s)))))

(define (append s1 s2)
    ; (if (null? s1) s2 (append (cdr s1) (cons (car s1) s2))))
    (if (null? s1) s2 (cons (car s1) (append (cdr s1) s2))))

(define (make-leaf symbol weight)
    (list 'leaf symbol weight))

(define (leaf? object)
    (and (pair? object) (eq? (car object) 'leaf)))

(define (symbol-leaf leaf) (cadr leaf))
(define (weight-leaf leaf) (caddr leaf))

(define (make-code-tree left right)
    (list left 
          right
          (append (symbols left) (symbols right))
          (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
    (if (leaf? tree) (list (symbol-leaf tree)) (caddr tree)))

(define (weight tree)
    (if (leaf? tree) (weight-leaf tree) (cadddr tree)))
    
(define (adjoin-set x set)  
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
    (if (null? pairs) 
        (list)
        (let ((pair (car pairs)))
             (adjoin-set (make-leaf (car pair) (cadr pair)) 
                         (make-leaf-set (cdr pairs))))))

(define (decode bits tree)
    (define (decode-1 bits branch)
        (if (null? bits) 
            (list)
            (let ((next-branch (choose-branch (car bits) branch)))
                 (if (leaf? next-branch) 
                     (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
                      (decode-1 (cdr bits) next-branch)))))
    (decode-1 bits tree))

(define (include? item set)
    (cond ((null? set) #f)
          ((equal? item (car set)) #t)
          (else (include? item (cdr set)))))

(define (encode-symbol symbol tree)
    (define (choose-branch-2 symbol tree-2)
        (let ((left (left-branch tree-2)) (right (right-branch tree-2)))
             (cond ((include? symbol (symbols left)) (list left 0))
                   ((include? symbol (symbols right)) (list right 1))
                   (else (error "Fail")))))
    (if (leaf? tree) 
        (list)
        (let ((pair (choose-branch-2 symbol tree)))
             (let ((next-branch (car pair)) (bit (cadr pair)))
                  (cons bit (encode-symbol symbol next-branch))))))
   ;(list 1))

(define (encode message tree)
    (if (null? message)
        (list)
        (append (encode-symbol (car message) tree) 
                (encode (cdr message) tree))))
                           
(define (choose-branch bit tree)
    (cond ((= bit 0) (left-branch tree))
          ((= bit 1) (right-branch tree))
          (else (error "bad bit " bit))))
 
 ; Ex 2.69
(define (generate-huffman-tree pairs)
    (define (successive-merge leaf-set)
        (if (< (lenght leaf-set) 2)
            leaf-set
            (successive-merge 
                (adjoin-set (make-code-tree (car leaf-set) (cadr leaf-set))
                            (cddr leaf-set)))))
    (successive-merge (make-leaf-set pairs)))

(print (generate-huffman-tree '((A 4) (C 1) (D 1) (B 2))))

; test                         
(define sample-tree
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree (make-leaf 'B 2)
                                    (make-code-tree (make-leaf 'D 1)
                                                    (make-leaf 'C 1)))))

(define some-message (list 0 1 1 0 0 1 0 1 0 1 1 1 0))
(define message (decode some-message sample-tree))













