#lang racket
(require rackunit)

;1
(map (lambda (x)
         (cond 
            ((symbol? x) (symbol->string x))
            ((number? x) (number->string x))
            ((boolean? x) (if x "#t" "#f"))
            (else "desconocido"))) '(1 #t hola #f (1 . 2))) ; ⇒ ("1" "#t" "hola" "#f" "desconocido") :ok:
;2

(filter (lambda (x) 
            (equal? (string-ref (symbol->string x) 1) #\a)) '(alicante barcelona madrid almería)) ; ⇒ (alicante almería) :not ok: (barcelona madrid) [1 = pos2, 0 = pos1]
;3

(foldr append '() '( (1 2) (3 4 5) (6 7) (8) ) ) ; ⇒ (1 2 3 4 5 6 7 8) :ok:

;4

(foldl (lambda (dato resultado)
         (string-append
          (symbol->string (car dato))
          (symbol->string (cdr dato))
          resultado)) "" '((a . b) (hola . adios) (una . pareja))) ; ⇒ unaparejaholaadiosab :ok:

;5

(foldr (lambda (dato resultado)
           (cons (+ (car resultado) dato)
                 (+ (cdr resultado) 1))) '(0 . 0) '(1 1 2 2 3 3)) ; ⇒ (12 . 6) :ok:


;6

(apply + (map cdr '((1 . 3) (2 . 8) (2 . 4)))) ; ⇒ 15 :ok:

;7

(apply min (map car (filter (lambda (p)
                                  (> (car p) (cdr p))) 
                                  '((3 . 1) (1 . 20) (5 . 2))))) ; ⇒ 3 :ok:

;;
;;b
;;
; Los siguientes ejercicios utilizan esta definición de lista

(define lista '((2 . 7) (3 . 5) (10 . 4) (5 . 5)))


; Queremos obtener una lista donde cada número es la suma de las
; parejas que son pares

(filter even?
        (map (lambda (x) (+ (car x)
                                 (cdr x))) ; OK
               lista))
; ⇒ (8 14 10)

; Queremos obtener una lista de parejas invertidas donde la "nueva"
; parte izquierda es mayor que la derecha.

(filter (lambda (pareja) (> (car pareja) (cdr pareja))  )
        (map (lambda (pareja) (cons (cdr pareja) (car pareja) ) ) lista)) ;:OK:
; ⇒ ((7 . 2) (5 . 3))

; Queremos obtener una lista cuyos elementos son las partes izquierda
; de aquellas parejas cuya suma sea par.

(foldr (lambda (x y) map (cons (car x) y ) ) '()
        (filter (lambda (x) (even? (+ (car x) (cdr x)))) lista) ) ; ⇒ (3 10 5)

;;
;; c
;;
(define (f1 x) (lambda (y z) (string-append y z x)))
(define g1 (f1 "a"))
(check-equal? (g1 "clase" "lpp") "claselppa")



(define (f2 x) (lambda (y z) (list y x z)))
(define g2 (f2 "lpp") )
(check-equal? (g2 "hola" "clase") (list "hola" "lpp" "clase"))


(define (f g) (lambda(z x) (g z x)))
(check-equal? ( (f cons) 3 4 ) '(3 . 4))

;;
;; Ejercicio 3
;;

;;a
(define (suma-n-izq n lista-parejas)
   (map (lambda (x) (cons (+ (car x) n) (cdr x) ) ) lista-parejas )
 )



;;b

(define (aplica-2 func lista-parejas)
  (map (lambda (x) (func (car x) (cdr x) ) )  lista-parejas)
)

(check-equal? (aplica-2 + '((2 . 3) (1 . -1) (5 . 4))) '(5 0 9) )
(check-equal? (aplica-2 (lambda (x y) (if (even? x) y (* y -1))) '((2 . 3) (1 . 3) (5 . 4) (8 . 10))) '(3 -3 -4 10) )


;;
;; Ejercicio 4
;;

; Función auxiliar que devuelve la parte derecha
; de una pareja si la parte izquierda es #t. Sino
; devuelve #f

(define (devuelve-si-existe pareja)
   (if (car pareja)
       (cdr pareja)
       #f
   )
)
(define (mi-index-of lista dato)
  (devuelve-si-existe 
   (foldl (lambda (elemento resultado)
            (cond
              ((car resultado) resultado) ; el car es #t: hemos encontrado el dato
                                          ; y no modificamos el resultado
              ((equal? dato elemento) (cons #t (cdr resultado))) ; encontramos el dato: construimos
                                                                    ; la pareja con #t y la posición actual
              (else (cons #f (+ (cdr resultado) 1))))) ; no es el dato: construimos la pareja con
                                                       ; #f e incrementamos el resultado
          (cons #f 0)  ; resultado inicial: pareja con #f (no encontrado) y 0 (posición inicial)
          lista)))
(check-equal? (mi-index-of '(a b c d c) 'c) 2)
(check-equal? (mi-index-of '(1 2 3 4 5) 10) #f)

;;
;; Ejercicio 5
;;

(define orden '(#\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0 #\J #\Q #\K #\A))

(define (valor-carta carta orden)
  (+ 1 (index-of orden (string-ref (symbol->string carta) 0))))

(define (carta-alta cartas)
  (apply max (apply valor-carta cartas) ) ;; Auxiliar devuelve la carta más alta de 1 mano

(define (ganadora lista-manos resultado)

  (if (null? lista-manos) 
      0
  (if (> (carta-alta (car lista-manos) ) (mano-ganadora (cdr lista-manos) ) )
      resultado
      (mano-ganadora (cdr lista-manos) (+ 1 resultado) )
      )
  )
  )