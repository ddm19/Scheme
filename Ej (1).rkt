;; Daniel Domenech Moreno

#lang racket
(require rackunit)

;;
;; Ejercicio 1 Binariotron3000
;;

;;a
(define (binario-a-decimal b3 b2 b1 b0)
  (+ (+ (* b3 (expt 2 3) ) (* b2 (expt 2 2) )) (+ (* b1 (expt 2 1) ) (* b0 (expt 2 0) )))
)
  
  (check-equal? (binario-a-decimal 1 1 1 1)  15 )
  (check-equal? (binario-a-decimal 0 1 1 0)  6 )
  (check-equal? (binario-a-decimal 0 0 1 0)  2 )          ;; Casos Prueba Binario-Decimal
  (check-equal? (binario-a-decimal 0 1 0 1)  5 )
;;b
(define (binario-a-hexadecimal b3 b2 b1 b0)
  (if ( > (binario-a-decimal b3 b2 b1 b0) 9 )
      (integer->char (+ (char->integer #\A) (- (binario-a-decimal b3 b2 b1 b0) 10)))
      (integer->char (+ (char->integer #\0) (binario-a-decimal b3 b2 b1 b0) ))
      )
)

  (check-equal? (binario-a-hexadecimal 1 1 1 1)  #\F )
  (check-equal? (binario-a-hexadecimal 0 1 1 0)  #\6 )   ;; Casos Prueba Binario-Hexadecimal
  (check-equal? (binario-a-hexadecimal 1 0 1 0)  #\A )
  (check-equal? (binario-a-hexadecimal 1 0 1 1)  #\B )

;;
;; Ejercicio 1 MenorTron2000
;;

;;Versión 1
(define (menor-de-tres n1 n2 n3)
  (if (< n1 n2) 
     (if (< n1 n3)
         n1
         n3
      )
  (if (< n2 n3)
         n2
         n3
      )
   )
 )

(check-equal? (menor-de-tres 1 4 6) 1)
(check-equal? (menor-de-tres 10 4 6) 4)          ;; Casos Prueba Menor de Tres
(check-equal? (menor-de-tres 11 42 6) 6)

;;Versión 2
(define (menor x y)
   (if (< x y)            ;; Función auxiliar Menor
       x
       y
       )
)

(define (menor-de-tres-v2 n1 n2 n3)
  (menor (menor n1 n2) (menor n2 n3) )
  )
(check-equal? (menor-de-tres-v2 1 4 6) 1)
(check-equal? (menor-de-tres-v2 10 4 6) 4)          ;; Mismos Casos Prueba ( Funciones equivalentes )
(check-equal? (menor-de-tres-v2 11 42 6) 6)

;;
;; Ejercicio 3
;;
(define (f x y) 
    (cons x y))
                   ;; Definiciones Enunciado
(define (g x)
    (cons 2 x))
;; (f (g (+ 2 1)) (+ 1 1))  <-- EVALUACIÓN PASO A PASO

;;  Orden Aplicativo
;; (f (g (+ 2 1)) (+ 1 1)) -> Sustituimos g por su cuerpo
;; (f (  (cons 2 (+ 2 1) ) (+ 1 1) ) -> Evaluamos (+ 2 1)
;; (f (  (cons 2 3) ) (+ 1 1) ) -> Evaluamos (cons 2 3)
;; (f (  (2 3) ) (+ 1 1) ) -> Evaluamos (+ 1 1)
;; (f (  (2 3) ) 2 ) -> Sustituimos f por su cuerpo
;; (cons (2 3)  2 ) -> Evaluamos cons
;; 2 3 2 

;;  Orden Normal

;; (f (g (+ 2 1)) (+ 1 1)) -> Sustituimos 'f' por su cuerpo
;; (cons (g (+2 1) ) (+ 1 1) ) -> Sustituimos 'g' por su cuerpo
;; (cons (cons 2 (+2 1) ) (+ 1 1) ) -> Evaluamos cons
;; ( ( 2 (+2 1) ) (+ 1 1)  ) -> Evaluamos +
;;  2 3 2


;;
;; Ejercicio 4
;;

(define (sumap t1)
  (+ (car t1) (cdr t1) )
  )                                    ;; FUNCIONES AUXILIARES
(define (diferencia t1)
  (if (> (sumap t1) 7)
       (- (sumap t1) 7) 
       (- 7 (sumap t1) )
       )
  )
  
          

(define (tirada-ganadora t1 t2)
  (if(= (diferencia t1) (diferencia t2) )
   0
   (if (< (diferencia t1) (diferencia t2) )  ;; TIRADA GANADORA
       1
       2
       )
   )
  )
(check-equal? (tirada-ganadora (cons 1 3) (cons 1 6)) 2)
(check-equal? (tirada-ganadora (cons 1 5) (cons 2 2)) 1)        ;; Casos de Prueba
(check-equal? (tirada-ganadora (cons 6 2) (cons 3 3)) 0)

;;
;; Ejercicio 5
;;

(define (obten-palo char)
  >(cond
     ((eq? char #\O) 'Oros)
     ((eq? char #\C) 'Copas)        ;; Obtener Palo
     ((eq? char #\B) 'Bastos)
     ((eq? char #\E) 'Espadas)
     )
  )
(define (obten-valor char)
      >(cond
     ((eq? char #\A) 1)
     ((eq? char #\S) 10)        ;; Obtener Valor
     ((eq? char #\C) 11)
     ((eq? char #\R) 12)
     (else (- (char->integer char) (char->integer #\0) ) )
     )
)

(define (carta t1)
  (cons (obten-valor (string-ref (symbol->string t1) 0)) (obten-palo (string-ref (symbol->string t1) 1)) ) ;; Función Carta
)

(define tres-de-oros '3O)
(define as-de-copas 'AC)
(define caballo-de-espadas 'CE)

(check-equal? (carta tres-de-oros) (cons 3 'Oros) )
(check-equal? (carta as-de-copas) (cons 1 'Copas) )      ;; Casos Prueba
(check-equal? (carta caballo-de-espadas) (cons 11 'Espadas) )
(check-equal? (carta 'RB) (cons 12  'Bastos) )

;;
;; Ejercicio 6
;;
(define epsilon 0.0001)

(define (iguales-reales? x y)
    (< (abs (- x y)) epsilon))  ;; Funciones auxiliares
(define (resta-cuadrada x y)
  (expt (- (max x y) (min x y) )  2)
  )
(define (longitud-lado t1 t2)
  (sqrt (+ (resta-cuadrada (car t1) (car t2) ) (resta-cuadrada (cdr t1) (cdr t2)  ) ) )
  )

(define (Equilatero t1 t2 t3)
  (if (and
       ( and
             (iguales-reales? (longitud-lado t1 t2) (longitud-lado t1 t3) )
             (iguales-reales? (longitud-lado t1 t2) (longitud-lado t2 t3)) )    
        (iguales-reales? (longitud-lado t2 t3) (longitud-lado t1 t3) )
        )
      #t
      #f
      )
  )
(define (Escaleno t1 t2 t3)
  (if (and
       ( and
             (not (iguales-reales? (longitud-lado t1 t2) (longitud-lado t1 t3) ) )
             (not (iguales-reales? (longitud-lado t1 t2) (longitud-lado t2 t3)) ) )    
        (not (iguales-reales? (longitud-lado t2 t3) (longitud-lado t1 t3) ) )
        )
      #t
      #f
      )
  )
;; TIPO TRIANGULO
(define (tipo-triangulo t1 t2 t3)
  (if (Equilatero t1 t2 t3)
      'Equilátero
      (if (Escaleno t1 t2 t3) 
        'Escaleno
        'Isósceles
        )
      )
  )

(check-equal? (tipo-triangulo (cons 1 1) (cons  1 6) (cons 6 1) ) 'Isósceles )
(check-equal? (tipo-triangulo (cons -2 3) (cons  2 6) (cons 5 3) ) 'Escaleno )
(check-equal? (tipo-triangulo (cons -3 0) (cons  3 0) (cons 0 5.1961) ) 'Equilátero )

