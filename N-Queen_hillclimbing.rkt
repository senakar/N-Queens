#lang racket
(require test-engine/racket-tests)

;8-queens prob using hill climbing search
(define N 8)

(define REPEAT (* N 2)) ;;constant used to run N*2 times until a solution is (hopefully) found

;;runs the N-Queen game
(define (run)
  ;;N-Queens: -> loq or 'fail
  ;;Purpose: to find a solution to the N-queens problem using hill-climbing search, return 'fail if not found
  (define (N-Queens limit)

    (define IQUEENS (build-list N (λ (x) x)))

    ;;take in a loq, generate its children and using the costs of them, find which one is the best child(randomly if same) and return that loq
    ;;best-child: loq -> loq
    ;;Purpose: to find the next best loq after generating the children of the given one
    ;(it will do this by finding the cost of all possible moves and picking the lowest cost)
    (define (best-child loq)

      ;generate-children: loq -> list of loqs
      ;Purpose: to generate a list of all the possible children of the given list of queens
      (define (generate-children loq)
        ;generate-children-col: loq number -> list of loq
        ;Purpose: generates the children for the given column
        (define (generate-children-col loq col) ;;col goes from 0-7 stops at 8 = N
          (define (helper n) ;possibly another accum for list so far
            (if (= N (sqrt (- n col)))
                '()
                (cons
                 (append (take loq col) (list n) (drop loq (add1 col)))
                 (helper (+ N n)))))
          (helper col))
        (letrec ([helper (λ (acc)
                           (if (= acc N)
                               '()
                               (append (generate-children-col loq acc) (helper (add1 acc)))))])
   
          (helper 0)))
     
      (let* ((CHILDREN (generate-children loq))
             (CHILDRENCOSTS (map (λ (x) (find-cost x)) CHILDREN))
             (LOWESTCOST (car (sort CHILDRENCOSTS <)))
             (POSLIST (indexes-of CHILDRENCOSTS LOWESTCOST))
             (POS (list-ref POSLIST (random (length POSLIST)))))
        (list-ref CHILDREN POS)))

    ;;find-cost: loq -> num
    ;;Purpose: to find the cost of the given list of queens (calculates the cost by how many possible collisions can occur)
    (define (find-cost loq)

      ;;queen-collisions: queen -> lon
      ;;Purpose: to find the collisions of the given queen
      (define (queen-collisions queen)

        (define col (quotient queen N))
        (define row (remainder queen N))
  
        ;columns: number number -> lon
        ;;Purpose: to find the column collisions of the given number (finds the columns of the given row)
        (define (columns num)
          (define (helper num acc)
            (cond [(> acc (sub1 N)) '()]
                  [else (cons (+ (* N acc) num)
                              (helper num (add1 acc)))]))
          (helper num 0))
        ;rows: num num -> lon
        ;;Purpose: to find the row collisions of the given number (finds the rows of the given column)
        (define (rows num)
          (define (helper num acc)
            (cond [(> acc (sub1 N)) '()]
                  [else (cons (+ (* N num) acc)
                              (helper num (add1 acc)))]))
          (helper num 0))
        ;num is row, acc is col
        ;diagonals: num -> lon
        ;;Purpose: to find the diagonal collisions of the given number
        (define (diagonals r c)
          ;leftup: num num -> lon
          ;Purpose: to find all the diagonals left and up of the given board position
          (define (leftup r c)
            (cond [(or (<= r 0) (<= c 0)) '()]
                  [else (cons (- (+ r (* N c)) N 1)
                              (leftup (sub1 r) (sub1 c)))]))
          ;rightdown num num -> lon
          ;Purpose: to find all the diagonals right and down of the given board position
          (define (rightdown r c)
            (cond [(or (>= c (sub1 N)) (>= r (sub1 N))) '()]
                  [else (cons (+ (+ r (* N c)) N 1)
                              (rightdown (add1 r) (add1 c)))]))
          ;leftdown: num num -> lon
          ;Purpose: to find all the diagonals left and down of the given board position
          (define (leftdown r c)
            (cond [(or (<= r 0) (>= c (sub1 N))) '()]
                  [else (cons (+ (+ r (* N c)) (sub1 N))
                              (leftdown (sub1 r) (add1 c)))]))
          ;rightup: num num -> lon
          ;Purpose: to find all the diagonals right and up of the given board position
          (define (rightup r c)
            (cond [(or (>= r (sub1 N)) (<= c 0)) '()]
                  [else (cons (- (+ r (* N c)) (sub1 N))
                              (rightup (add1 r) (sub1 c)))]))
  
          (append (reverse (leftup r c))
                  (reverse (rightup r c))
                  (leftdown r c)
                  (rightdown r c)))
  
        (sort
         (remove-duplicates
          (append (columns row)
                  (rows col)
                  (diagonals row col))) <))

      ;;count-collisions: loq loc(listof collisions) -> number
      ;;Purpose: to find the cost of the given queen starting with the given cost
      (define (count-collisions queens loc) ;list of collisions
        (cond [(empty? queens) -1] ;we subtract one from the end count result since it will return a count for itself and we do not want that
              [else
               (+ (count (λ (x) (= x (first queens))) loc)
                  (count-collisions (cdr queens) loc))]))
      (apply + (map (λ (x) (count-collisions loq (queen-collisions x))) loq)))


    ;;find-solution: loq number -> loq or 'fail
    ;;Purpose: to find the solution using hill-climbing search (by always choosing the lowest cost child)
    ;;if there is no lower cost child returns 'fail as the search has failed to find a solution
    (define (find-solution loq cost)
      (let* ((BEST (best-child loq))
             (BESTCOST (find-cost BEST)))
        (cond [(= 0 BESTCOST) BEST] ;;BEST would be the solution if it is 0
              [(<= cost BESTCOST) (begin
                                    (display limit)
                                    (display " ")
                                    (stop-when limit))] 
              [else (find-solution BEST BESTCOST)])))

 
   
    ; (begin
    ;  (define REPEAT 2) ;;constant used to run N*2 times until a solution is (hopefully) found
    ; (display REPEAT)
    (find-solution IQUEENS (find-cost IQUEENS)))


  ;stop-when: number -> 'fail or procedure (N-Queens)
  ;Purpose: runs the N-Queens program until the given number reaches 0
  ; this is so that the possibility of finding a solution with each run increases as it will run the searching algorithm multiple times
  (define (stop-when num)
    (cond [(= num 0) 'fail]
          [else (N-Queens (sub1 num))]))

  (N-Queens REPEAT))

;used for testing
;world is a list of queens (natnum)
;list of queens is always of length 8 (there are always 8 queens)
(define INIT-WORLD '(0 1 2 3 4 5 6 7)) ; N = 8
(define INIT-W '(0 1 2 3)) ;for when N = 4
;;tests

;find-pos
;(check-expect (find-pos 0 '(1 0 1 2 3 1 0 1)) '(1 6))

;;find-cost & count-collisions
;(check-expect (find-cost INIT-W) 12)
;(check-expect (find-cost '(0 9 6 3) 8)
;(check-expect (count-collisions '(0 9 6 3) '(0 2 3 4 5 6 9 11 13)) 3)
;(check-expect (count-collisions '(0 9 6 3) '(1 2 3 4 5 8 10 12 15)) 0)


;;queen-collisions function tests
;(check-expect (queen-collisions 0) '(0 1 2 3 4 5 8 10 12 15))
;(check-expect (queen-collisions 3) '(0 1 2 3 6 7 9 11 12 15))

;(check-expect (columns 0) '(0 4 8 12))
;(check-expect (rows 0) '(0 1 2 3))
;(check-expect (reverse (leftup 2 3)) '(4 9))
;(check-expect (reverse (rightup 2 3)) '(11))
;(check-expect (rightdown 0 0) '(5 10 15))
;(check-expect (rightdown 2 1) '(11))
;(check-expect (leftdown 0 1) '())
;(check-expect (leftdown 1 2) '(12))
;(check-expect (leftdown 2 0) '(5 8))

;(check-expect (diagonals 0 0) '(5 10 15))
;(check-expect (diagonals 1 2) '(4 3 6 12 14))
;(check-expect (diagonals 3 2) '(1 6 14))

#;(check-expect (generate-children-col INIT-WORLD 0) ;when N = 8
                '((0 1 2 3 4 5 6 7)
                  (8 1 2 3 4 5 6 7)
                  (16 1 2 3 4 5 6 7)
                  (24 1 2 3 4 5 6 7)
                  (32 1 2 3 4 5 6 7)
                  (40 1 2 3 4 5 6 7)
                  (48 1 2 3 4 5 6 7)
                  (56 1 2 3 4 5 6 7)))


#;(check-expect (generate-children INIT-W)  ;;if the board was n = 4 instead of 8
                (list
                 '(4 1 2 3)
                 '(8 1 2 3)
                 '(12 1 2 3)
                 '(0 5 2 3)
                 '(0 9 2 3)
                 '(0 13 2 3)
                 '(0 1 6 3)
                 '(0 1 10 3)
                 '(0 1 14 3)
                 '(0 1 2 7)
                 '(0 1 2 11)
                 '(0 1 2 15)))

