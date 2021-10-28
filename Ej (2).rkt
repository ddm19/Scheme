#lang racket
(require rackunit)

;;
;;Ejercicio 1 
;;a
(define (minimo l1)
  (if(> (length l1) 1)
    (min (car l1) (minimo (cdr l1) )) ;; Recursivo
    (car l1)                          ;; Base
  )
  )

(check-equal? (minimo '(1 8 6 4 3 )) 1)
(check-equal? (minimo '(1 -1 3 -6 4 )) -6)
(check-equal? (minimo '(-12 -50 3 43 -300 )) -300)

;;b
(define (concatena lista-chars )

  (if(null? lista-chars)
     "" ;; Caso Base
     (string-append (string (car lista-chars)) (concatena (cdr lista-chars))) ;; Caso Recursivo
  )
 )
(check-equal? (concatena '(#\H #\o #\l #\a) ) "Hola" )
(check-equal? (concatena'(#\S #\c #\h #\e #\m #\e #\space #\m #\o #\l #\a)) "Scheme mola" )
(check-equal? (concatena'(#\L #\a #\space #\v #\i #\d #\a #\space #\e #\s #\space #\u #\n #\a #\space #\T #\ó #\m #\b #\o #\l #\a )) "La vida es una Tómbola" )

;;c
(define (contiene-lista? lista dato)
  (if (null? lista) 
    #f
  (or (equal? (car lista) dato) (contiene-lista? (cdr lista) dato) )
  )
 )
(define (contiene? cadena char)
  (contiene-lista? (string->list cadena) char)
  )

  
(check-equal? (contiene? "Hola" #\o) #t)
(check-equal? (contiene? "Esto es una frase" #\space) #t)
(check-equal? (contiene? "Si la Vida te da limones" #\x) #f)

;;
;; Ejercicio 2
;;

;;a

(define (binario-a-decimal lista-bits)
(if (null? lista-bits)
    0
  (if (= (car lista-bits) 1)
     (+ (expt 2 (- (length lista-bits) 1) ) (binario-a-decimal (cdr lista-bits) ) )
      (binario-a-decimal (cdr lista-bits) )
      )
  )
)
(check-equal? (binario-a-decimal '(1 1 1 1)) 15)
(check-equal? (binario-a-decimal '(1 1 0) ) 6)
(check-equal? (binario-a-decimal '(1 0) ) 2)
(check-equal? (binario-a-decimal '(1 1 1 1 1 1 1 1 1 1 1 1 0)) 8190)

;;b
(define (ordenada-creciente? lista-nums)
   (or (null? (cdr lista-nums) )   
    ( and (< (car lista-nums) (car (cdr lista-nums) ) ) (ordenada-creciente? (cdr lista-nums) )
     )
    )
      
 )

(check-true (ordenada-creciente? '(-1 23 45 59 99)))
(check-false (ordenada-creciente? '(12 50 -1 293 1000)))
(check-true (ordenada-creciente? '(3 5 5.43 70000 70000.1)))

;;
;; Ejercicio 3
;;

;; a.1
(define p1(cons (cons 1 2) (list (cons 3 (list 4) ) ) ))


;; a.2
; (cdr (car p1) ) -> 1 

;; b.1
(define p2 (cons (cons (cons 7 (cons 8 9) ) (cons (cons 1 (list 2) ) (list 3) ) ) (cons 10 11) ) )

(check-equal? p2 '(((7 . (8 . 9)) . ((1 . (2 . ())) . (3 . ()))) . (10 . 11)))

;;
;;Ejercicio 4
;;

(define cartas1 '(3O 5E AC 2B 5O 5C 4B))
(define cartas2 '(CE AO 3B AC 2E SC 4C))

(define (obten-valor char)
      >(cond
     ((eq? char #\A) 1)
     ((eq? char #\S) 10)        ;; Obtener Valor Auxiliar
     ((eq? char #\C) 11)
     ((eq? char #\R) 12)
     (else (- (char->integer char) (char->integer #\0) ) )
     )
)
(define (traduce carta)
      (obten-valor (string-ref (symbol->string carta ) 0)  )  ;; Traducir a numero auxiliar
   )

(check-equal? (traduce '3O) 3)
(check-equal? (traduce 'AE) 1)

(define (sumahasta listacartas x) 
  (if (or (= x 0 ) (null? listacartas) )
          0
   (+ ( traduce (car listacartas) ) (sumahasta (cdr listacartas) (- x 1) ) ) ;; Sumar Cartas Auxiliar
   )
)
(check-equal? (sumahasta cartas1 4) 11)
(check-equal? (sumahasta cartas2 1) 11)
(check-equal? (sumahasta cartas2 7) 32)

(define (blackjack cartasx numx cartasy numy)
  (if (or (= (sumahasta cartasx numx) (sumahasta cartasy numy) ) (and (> (sumahasta cartasx numx) 21) (> (sumahasta cartasy numy) 21)  ) )  
       (if (> (sumahasta cartasx numx) 21)  ;;Ambos se pasan
           -1
            0                                ;; Empate < 21
            )
  
  (if (> (sumahasta cartasx numx) (sumahasta cartasy numy ) ) ;; 1º > 2º
         1                                    
         2                                  ;; 1º > 21
   )
     
     )
   )

(check-equal? (blackjack cartas1 5 cartas2 4) 0)
(check-equal? (blackjack cartas1 5 cartas2 3) 1)
(check-equal? (blackjack cartas1 3 cartas2 4) 2)
(check-equal? (blackjack cartas1 7 cartas2 6) -1)

;;
;; Ejercicio 5
;;

;;a

(define (suma-izq pareja n)
 (cons (+ (car pareja) n) (cdr pareja) )
  )
(define (suma-der pareja n)
 (cons (car pareja) (+ (cdr pareja) n)  )
  )

(check-equal? (suma-izq (cons 10 20) 3 ) (cons 13 20) )
(check-equal? (suma-der (cons 10 20) 5 ) (cons 10 25) )

;;b

(define (suma-impares-pares lista-num)
  (if (null? lista-num)
      (cons 0 0)
  (if ( even? (car lista-num) ) 
       (suma-der (suma-impares-pares (cdr lista-num) )  (car lista-num) )
       (suma-izq (suma-impares-pares (cdr lista-num) )  (car lista-num) )
   )
  )
)
(check-equal? (suma-impares-pares '(3 2 1 4 8 7 6 5) ) (cons 16 20) )
(check-equal? (suma-impares-pares '(3 1 5) ) (cons 9 0) )
(check-equal? (suma-impares-pares '(1 2 1 1 1 2 1 2 1 2 1) ) (cons 7 8) )

;;
;; Ejercicio 6
;;

(define (cadenagrande lista)
  (if (null? lista)
      ""
      (if (> (string-length (car lista) ) (string-length (cadenagrande (cdr lista) ) ) ) ;; Auxiliar String mayor
          (car lista)
          (cadenagrande (cdr lista) )
      )
   )
)

(check-equal? (cadenagrande '("al" "corro" "de" "la" "patata" ) ) "patata"  )

(define (cadena-mayor lista)
  (cons (cadenagrande lista) (string-length (cadenagrande lista) ) )
  )

(check-equal? (cadena-mayor '("vamos" "a" "obtener" "la" "cadena" "mayor") ) (cons "obtener" 7) )
(check-equal? (cadena-mayor '("prueba" "con" "maximo" "igual" ) ) (cons "maximo" 6) )
(check-equal? (cadena-mayor '() ) (cons "" 0) )
