#lang racket
(require rackunit)

;;
;; Ejercicio 1
;;

;;a

(define (es-prefijo? pal1 pal2)
  
  (equal? pal1 (substring pal2 0 (string-length pal1)) ) ;; Auxiliar prefijo
)

(define (contiene-prefijo prefijo lista-pal) 
  (if (= (length lista-pal) 1 )  
      (list (es-prefijo? prefijo (car lista-pal) ) )
      (cons (es-prefijo? prefijo (car lista-pal) ) (contiene-prefijo prefijo (cdr lista-pal) )  )
  )
)

(check-equal? (contiene-prefijo "ante" '("anterior" "antígona" "antena" "anatema")) '(#t #f #t #f) ) 
(check-equal? (contiene-prefijo "pan" '("panadero" "pastel" "panadería" "pen")) '(#t #f #t #f) ) 
(check-equal? (contiene-prefijo "auto" '("autismo" "aullar" "antena" "automóvil")) '(#f #f #f #t) ) 

;; b

(define (inserta-pos dato pos lista)
  (if (= pos 0)
      (cons dato lista)
  (cons (car lista) (inserta-pos dato (- pos 1) (cdr lista) ) )
   )
)

(check-equal? (inserta-pos 'b 2 '(a a a a) ) '(a a b a a) )
(check-equal? (inserta-pos 'b 0 '(a a a a) ) '(b a a a a) )
(check-equal? (inserta-pos 'b 4 '(a a a a) ) '(a a a a b) )

;; c
(define (inserta-ordenada n lista-ordenada)
  (if (null? lista-ordenada)
      (list n)
      (if (< n (car lista-ordenada) ) 
          (cons n lista-ordenada)
          (cons (car lista-ordenada) (inserta-ordenada n (cdr lista-ordenada) ) )
      )
  )
)

(check-equal? (inserta-ordenada 3 '(-400 -56 2 70) ) '(-400 -56 2 3 70) )
(check-equal? (inserta-ordenada 10 '(-8 2 3 11 20) ) '(-8 2 3 10 11 20) )

;; ordena

(define (ordena lista)
  (if (null? lista) 
  '()
  (inserta-ordenada (car lista) (ordena (cdr lista) ) ) 
  )
)

(check-equal? (ordena '(4 3 1 5) ) '(1 3 4 5) )
(check-equal? (ordena '(4 3 1 -54 -312 5 2) ) '(-312 -54 1 2 3 4 5) )

;;
;; Ejercicio 2
;;

(define (expande1 pareja)
  (if (= 1 (cdr pareja) )
  (car pareja)
  (cons (car pareja) (expande1 (cons (car pareja) (- pareja 1)) ) )
  )
)

(define (unelistas lista1 lista2)
  (if (= (length lista1) 1)
      (cons (car lista1) lista2)                             
      (cons (car lista1) (unelistas (cdr lista1) lista2) )
      )
  )
(define (expande-parejas pareja1 pareja2 . pareja_n)
  (if (= (length pareja_n) 0)
      (unelistas (expande1 pareja1) (expande1 pareja2) )
      (unelistas (expande1 pareja1) (expande-parejas pareja2 (cdr pareja_n) ) )
      )
  )
 
(check-equal? (expande-parejas '(#t . 3) '("LPP" . 2) '(b . 4)) '(#t #t #t "LPP" "LPP" b b b b)) 
