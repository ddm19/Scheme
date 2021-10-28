#lang racket
(require rackunit)

;;-------------------------------------------- BARRERA DE ABSTRACCIÓN Árboles --------------------------------------------

(define (dato-arbol arbol) 
    (car arbol))

(define (hijos-arbol arbol) 
    (cdr arbol))

(define (hoja-arbol? arbol) 
   (null? (hijos-arbol arbol)))
;; -- ↑↑ Selectores ↑↑ --

(define (construye-arbol dato lista-arboles) ;; Constructor
   (cons dato lista-arboles))

;;-------------------------------------------- BARRERA DE ABSTRACCIÓN Árboles Binarios --------------------------------------------

(define (hijo-izq-arbolb arbol)
   (cadr arbol))

(define (hijo-der-arbolb arbol)
   (caddr arbol))

(define arbolb-vacio '())

(define (vacio-arbolb? arbol)
   (equal? arbol arbolb-vacio))

(define (hoja-arbolb? arbol)
   (and (vacio-arbolb? (hijo-izq-arbolb arbol))
        (vacio-arbolb? (hijo-der-arbolb arbol))))
;; -- ↑↑ Selectores ↑↑ --

(define (construye-arbolb dato hijo-izq hijo-der) ;; Constructor
    (list dato hijo-izq hijo-der))


;;-------------------------------------------- BARRERA DE ABSTRACCIÓN Árboles Binarios --------------------------------------------

(define arbolej1 (construye-arbol 15 (list (construye-arbol 4 (list
                                                      (construye-arbol 2 '())
                                                      (construye-arbol 3 '())))
                                     (construye-arbol 8 (list
                                                      (construye-arbol 6 '())))
                                     (construye-arbol 12 (list
                                                      (construye-arbol 9 '())
                                                      (construye-arbol 10 '())
                                                      (construye-arbol 11 '())))
                                     )))
(check-equal?  (dato-arbol (cadr (hijos-arbol (caddr (hijos-arbol arbolej1 ))))) 10)

;;------- AUX EJ 1
(define (suma-datos-arbol arbol)
    (+ (dato-arbol arbol)
       (suma-datos-bosque (hijos-arbol arbol))))

(define (suma-datos-bosque bosque)
    (if (null? bosque)
        0
        (+ (suma-datos-arbol (car bosque)) 
           (suma-datos-bosque (cdr bosque)))))

(define (suma-datos-arbol-fos arbol)
   (foldr + (dato-arbol arbol) 
       (map suma-datos-arbol-fos (hijos-arbol arbol))))
;;-------- AUX EJ 1

(check-equal? (suma-datos-bosque (hijos-arbol arbolej1)) 65)
;;a.2-1 -> 9
;;a.2-2 -> 56
(check-equal? (suma-datos-arbol arbolej1) 80)

;; a.3
 ;; 1 -> '(9 14 42)
 ;; 2 -> 15+42, 57+14,71+9

;; b.1

(define arbolbej1 (construye-arbolb 40
                                    (construye-arbolb 23
                                                      (construye-arbolb 5 '() '())
                                                      (construye-arbolb 32
                                                                        (construye-arbolb 29 '() '())
                                                                        '())
                                                      )
                                    (construye-arbolb 45
                                                      '()
                                                      (construye-arbolb 56 '() '()))
                                    ))
(check-equal? (dato-arbol(hijo-izq-arbolb(hijo-der-arbolb(hijo-izq-arbolb arbolbej1)))) 29)
  
;;
;; Ejercicio 2
;;

;;a

(define (to-string-arbol arbol)
  (string-append (symbol->string (dato-arbol arbol))
          (to-string-bosque (hijos-arbol arbol)))
      )

 (define (to-string-bosque bosque)
   (if (null? bosque)
       ""
       (string-append (to-string-arbol (car bosque)) (to-string-bosque (cdr bosque)))
       )
   )

       
(define arbol2 '(a (b (c (d)) (e)) (f)))

(check-equal? (to-string-arbol arbol2) "abcdef")

(define (to-string-arbol-fos arbol)
  (apply string-append (symbol->string (dato-arbol arbol))
         (map to-string-arbol-fos (hijos-arbol arbol) )
         )
  )

(check-equal? (to-string-arbol-fos arbol2) "abcdef")

;;b

(define (veces-arbol dato arbol)
  (if (equal? dato (dato-arbol arbol))
      (+ 1 (veces-bosque dato (hijos-arbol arbol)))
      (veces-bosque dato (hijos-arbol arbol))
      )
  )

(define (veces-bosque dato bosque)
  (if (null? bosque)
      0
      (+ (veces-arbol dato (car bosque)) (veces-bosque dato (cdr bosque)))
      )
  )

(check-equal? (veces-arbol 'b '(b (b (c) (d)) (b (b) (f)))) 4)
(check-equal? (veces-arbol 'g '(a (b (c) (d)) (b (b) (f)))) 0)

;;
;; Ejercicio 3
;;

(define (hojas-cumplen pred arbol)
  (if (and (hoja-arbol? arbol) (pred (dato-arbol arbol)))
      (cons (dato-arbol arbol) (hojas-cumplen-bosque pred (hijos-arbol arbol)))
      (hojas-cumplen-bosque pred (hijos-arbol arbol))
      )
  )
(define (hojas-cumplen-bosque pred bosque)
  (if (null? bosque)
      '()
      (append (hojas-cumplen pred (car bosque)) (hojas-cumplen-bosque pred (cdr bosque)))
      )
  )
(define arbolej31 '(10 (2) (12 (4) (2)) (10 (5))))
(define arbolej32 '(10 (2) (12 (4) (2)) (10 (6))))

(check-equal? (hojas-cumplen even? arbolej31) '(2 4 2))
(check-equal? (hojas-cumplen even? arbolej32) '(2 4 2 6))

(define (hojas-cumplen-fos pred arbol)
  (if (and (hoja-arbol? arbol) (pred (dato-arbol arbol ) ))
     (list (dato-arbol arbol))
     (apply append (map (lambda (subarbol) (hojas-cumplen-fos pred subarbol))
                        (hijos-arbol arbol)))
     )
  )

(check-equal? (hojas-cumplen-fos even? arbolej31) '(2 4 2))
(check-equal? (hojas-cumplen-fos even? arbolej32) '(2 4 2 6))

;;b

(define (todas-hojas-cumplen? pred arbol)
  (if (hoja-arbol? arbol)
      (pred (dato-arbol arbol))
      (todas-hojas-cumplen?-bosque pred (hijos-arbol arbol) )
      )
  )


(define (todas-hojas-cumplen?-bosque pred bosque)
  (or (null? bosque)
      (and (todas-hojas-cumplen? pred (car bosque))
           (todas-hojas-cumplen?-bosque pred (cdr bosque))
           )
      )
  )

(check-equal? (todas-hojas-cumplen? even? arbolej31) #f)
(check-equal? (todas-hojas-cumplen? even? arbolej32) #t)

;;
;; Ejercicio 4
;;
;;a

(define (suma-raices-hijos arbol)
  (apply + (map (lambda (subarbol)
                  (dato-arbol subarbol)) (hijos-arbol arbol))
         )
  )

(define arbolej4 '(20 (2) (8 (4) (2)) (9 (5))))
(check-equal? (suma-raices-hijos arbolej4) 19)
(check-equal? (suma-raices-hijos (cadr (hijos-arbol arbolej4))) 6)

;;b

(define (raices-mayores-arbol? arbol)
  (and (> (dato-arbol arbol) (suma-raices-hijos arbol))
       (raices-mayores-bosque? (hijos-arbol arbol))
       )
  )
(define (raices-mayores-bosque? bosque)
  (or (null? bosque)
      (and (raices-mayores-arbol? (car bosque))
           (raices-mayores-bosque? (cdr bosque))
           )
      )
  )

(check-equal? (raices-mayores-arbol? arbolej4) #t)
(check-equal? (raices-mayores-arbol? '(20 (2) (8 (4) (5)) (9 (5)))) #f)

