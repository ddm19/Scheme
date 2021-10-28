#lang racket
(require rackunit)

;;
;; Ejercicio 1
;;

;; a

(define lista-a '((a b) d (c (e) (f g) h) ) )

(check-equal? (cadddr (caddr lista-a)) 'h)

(define lista-b1 '( (2 (3) ) (4 2) ( (2) 3 ) ) )

(define lista-b2 '( (b) ( c (a) ) d (a) ) )

;;b

;;-----------------------------------------------------------------
(define (hoja? elem)                                               ;
   (not (list? elem)))                                             ;
                                                                   ;
(define (plana? lista)                                             ; Auxiliares Listas
   (or (null? lista)                                               ;
       (and (hoja? (car lista))                                    ;
            (plana? (cdr lista)))))                                ;
(define (suma-1-si-mayor-igual-que-0 x)                            ;
  (if (>= x 0)                                                     ;
      (+ x 1)                                                      ;
      x))                                                          ;
(define (nivel-hoja-fos dato lista)                                ;
  (if (hoja? lista)                                                ;
      (if (equal? lista dato) 0 -1)                                ;
      (suma-1-si-mayor-igual-que-0                                 ;
       (foldr max -1 (map (lambda (elem)                           ;
                           (nivel-hoja-fos dato elem)) lista)))))  ;
                                                                   ;
                                                                   ;
(define (aplana lista)                                             ;
  (cond                                                            ;
    ((null? lista) '())                                            ;
    ((hoja? lista) (list lista))                                   ;
    (else                                                          ;
     (append (aplana (car lista))                                  ;
             (aplana (cdr lista))))))                              ;
                                                                   ;
                                                                   ;
(define (for-all? predicado lista)                                 ;
  (or (null? lista)                                                ;
      (and (predicado (car lista))                                 ;
           (for-all? predicado (cdr lista)))))                     ;                                                                  ;
                                                                   ;
                                                                   ;
                                                                   ;
                                                                   ;
                                                                   ;
;;-----------------------------------------------------------------


  (define (cuadrado-estruct elem)      
  (cond ((null? elem) '())
        ((hoja? elem) (* elem elem ))
        (else (cons (cuadrado-estruct (car elem))
                    (cuadrado-estruct (cdr elem))))))

(check-equal? (cuadrado-estruct lista-b1) '( (4 (9) ) (16 4) ( (4) 9 ) ) )


;;
;; Ejercicio2
;;

;;a

(define (cuenta-pares lista)
  (cond ((null? lista) 0)
        ((hoja? lista) (if (even? lista) 1 0))
        (else (+ (cuenta-pares (car lista)) (cuenta-pares (cdr lista))))
        )
  )
  
  
(check-equal? (cuenta-pares '(1 (2 3) 4 (5 6))) 3)
(check-equal? (cuenta-pares '(((1 2) 3 (4) 5) ((((6)))))) 3)


(define (cuenta-pares-fos lista)
  (cond ((null? lista) 0)
        ((hoja? lista) (if (even? lista) 1 0))
        (else (apply + (map cuenta-pares-fos (aplana lista)))
           )
    )
  )

(check-equal? (cuenta-pares-fos '(1 (2 3) 4 (5 6))) 3)
(check-equal? (cuenta-pares-fos '(((1 2) 3 (4) 5) ((((6)))))) 3)

;;b

(define (todos-positivos lista)
  (cond ((null? lista) #t)
        ((hoja? lista) (> lista 0))
        (else (and (todos-positivos (car lista)) (todos-positivos (cdr lista))))
        )
  )

(check-equal? (todos-positivos '(1 (2 (3 (-3))) 4)) #f)
(check-equal? (todos-positivos '(1 (2 (3 (3))) 4))  #t)

(define (todos-positivos-fos lista)
   (cond ((null? lista) #t)
        ((hoja? lista) (> lista 0))
        (else (for-all? (lambda (elem) (> elem 0)) (aplana lista) ))
        )
  )

(check-equal? (todos-positivos-fos '(1 (2 (3 (-3))) 4)) #f)
(check-equal? (todos-positivos-fos '(1 (2 (3 (3))) 4))  #t)

;;
;; Ejercicio 3
;;

(define (cumplen-predicado pred lista)
  (cond ((null? lista) '())
        ((hoja? lista) (if (pred lista) (list lista) '() ))
        (else (append (cumplen-predicado pred (car lista)) (cumplen-predicado pred (cdr lista))))
        )
  )
(check-equal? (cumplen-predicado even? '(1 (2 (3 (4))) (5 6))) '(2 4 6))
(check-equal? (cumplen-predicado pair? '(((1 . 2) 3 (4 . 3) 5) 6)) '((1 . 2) (4 . 3)))
        
 (define (cumplen-predicado-fos pred lista)
   (filter pred (aplana lista))
   )
(check-equal? (cumplen-predicado-fos even? '(1 (2 (3 (4))) (5 6))) '(2 4 6))
(check-equal? (cumplen-predicado-fos pair? '(((1 . 2) 3 (4 . 3) 5) 6)) '((1 . 2) (4 . 3)))

;;
;; Ejercicio 4
;;

;;a
(define (sustituye-elem elem-old elem-new lista)
  (cond ((null? lista) '())
        ((hoja? lista) (if (equal? lista elem-old) elem-new lista))
        (else (cons (sustituye-elem elem-old elem-new (car lista) ) (sustituye-elem elem-old elem-new (cdr lista))))
        )
  )

(check-equal? (sustituye-elem 'c 'h '(a b (c d (e c)) c (f (c) g))) '(a b (h d (e h)) h (f (h) g)))

;;b

(define (diff-listas l1 l2)
  (cond ((null? l1) '() )
        ((and (hoja? l1) (hoja? l2)) (if (equal? l1 l2) '() (list (cons l1 l2))))
        (else (append (diff-listas (car l1) (car l2)) (diff-listas (cdr l1) (cdr l2))))
        )
  )

(check-equal? (diff-listas '(a (b ((c)) d e) f) '(1 (b ((2)) 3 4) f)) '((a . 1) (c . 2) (d . 3) (e . 4)))
(check-equal? (diff-listas '((a b) c) '((a b) c)) '())

;;
;; Ejercicio 5
;;

;;a

(define (mezclar lista1 lista2 n)
  (cond ((null? lista1) '())
        ((hoja? lista1) (if (>= n 0) lista1 lista2))
        (else (cons (mezclar (car lista1) (car lista2) (- n 1)) (mezclar (cdr lista1) (cdr lista2) n)))
        )
  )

(define lista1 '(((a b) ((c))) (d) e))
(define lista2 '(((1 2) ((3))) (4) 5))
(check-equal? (mezclar lista1 lista2 2) '(((1 2) ((3))) (d) e))

;;b

(define (nivel-elemento lista)
  (nivel-elemento2 lista 0))

(define (nivel-elemento2 lista n)
  (cond ((null? lista) (cons null 0))
        ((hoja? lista) (cons lista n))
  (else (if (> (cdr (nivel-elemento2 (car lista) (+ n 1) ) ) (cdr (nivel-elemento2 (cdr lista) (+ n 1) ) ) )
            (nivel-elemento2 (car lista) (+ n 1))
            (nivel-elemento2 (cdr lista) n)
            )
        )
  )
  )
  



(check-equal? (nivel-elemento '(2 (3))) '(3 . 2))
(check-equal? (nivel-elemento '((2) (3 (4)((((((5))) 6)) 7)) 8)) '(5 . 8))
