#lang racket
(require rackunit)
(require graphics/turtles)

;;
;; Ejercicio1
;;

;;a
(define (concat lista)
  (concat-iter lista "")
  )
(define (concat-iter lista resultado)
  (if (null? lista)
      resultado
      (concat-iter (cdr lista) (string-append resultado (car lista) ) )
      )
  )
(check-equal? (concat '("El" " Jorobado" " de" " Notredam")) "El Jorobado de Notredam")

;;b

(define (min-max-iter lista resultado)
  (if (null? lista)
      resultado
  (if (< (car lista) (car resultado)) 
      (min-max-iter (cdr lista) (cons (car lista) (cdr resultado)) )
      (if (> (car lista) (cdr resultado) ) 
          (min-max-iter (cdr lista) (cons (car resultado) (car lista)) ) 
          (min-max-iter (cdr lista) resultado)
          )
      )
  )
  )

      
(check-equal? (min-max-iter '(5 9 12 5 0 4) (cons 2 2)) '(0 . 12))

(define (min-max lista)
  (min-max-iter lista (cons (car lista) (car lista)) )
  )

(check-equal? (min-max '(2 3 5 483 1) ) '(1 . 483))

;;
;; Ejercicio 2
;;

;;a

(define (expande-pareja-iter pareja resultado)
  (if (= (cdr pareja) 0)
      resultado
      (cons (car pareja) (expande-pareja-iter (cons (car pareja) (- (cdr pareja) 1) ) resultado))
  )
)
(check-equal? (expande-pareja-iter (cons 'a 4) '()) '(a a a a) )
              
(define (expande-pareja pareja)
  (expande-pareja pareja '())
 )

;;b

(define (rotar k lista)
  (if (= k 0) 
      lista
      (rotar (- k 1) (append (cdr lista) (list (car lista) ) ) )
      )
)
(check-equal? (rotar 4 '(a b c d e f g)) '(e f g a b c d))

;;
;; Ejercicio 3
;;

;;a

(define (mi-foldl procedimiento base lista)
  (if (null? lista)
      base
      (mi-foldl procedimiento (procedimiento (car lista) base) (cdr lista) )
  )
 )

(check-equal? (mi-foldl string-append "****" '("hola" "que" "tal")) "talquehola****")
(check-equal? (mi-foldl cons '() '(1 2 3 4)) '(4 3 2 1))

;;b
(define (prefijo-lista? lista1 lista2)
  (prefijo-lista-iter lista1 lista2 #t)
  )
(define (prefijo-lista-iter lista1 lista2 resultado)
  (if (null? lista1)
      resultado
      (prefijo-lista-iter (cdr lista1) (cdr lista2) (equal? (car lista1) (car lista2)))
      )
  )

(check-equal? (prefijo-lista? '(a b c) '(a b c d e)) #t)
(check-equal? (prefijo-lista? '(b c) '(a b c d e)) #f)

;;
;; Ejercicio 4
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (crea-diccionario)
  (mcons '*diccionario* '()))             
                                             ;
(define (busca key dic)                      ;
  (cond
    ((null? dic) #f)
    ((equal? key (mcar (mcar dic)))
     (mcar dic))
    (else (busca key (mcdr dic)))))
                                             ;
(define (get key dic)                        ;
  (define record (busca key (mcdr dic)))     ;
  (if (not record)
      #f
      (mcdr record)))

(define (put key value dic)
  (define record (busca key (mcdr dic)))
  (if (not record)
      (set-mcdr! dic                          ;
                (mcons (mcons key value)      ;
                      (mcdr dic)))
      (set-mcdr! record value))
  value)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define diccionario (crea-diccionario))

(define (pascal-memo fila col dic)
   (cond ((= col 0) 1)
         ((= col fila) 1)
         ((not (equal? (get (cons fila col) dic) #f)) (get (cons fila col) dic))
         (else (put (cons fila col) (+ (pascal-memo (- fila 1) (- col 1) dic)
                  (pascal-memo (- fila 1) col dic) ) dic)
               )
         )
  )

(check-equal? (pascal-memo 8 4 diccionario) 70 )
(check-equal? (pascal-memo 40 20 diccionario) 137846528820)

;;
;; Ejercicio 5
;;

;;a koch
(define (koch nivel trazo)
  (if (= nivel 0 )
      (draw trazo)
      (begin
        (koch (- nivel 1) (/ trazo 3) )
        (turn 60)
        (koch (- nivel 1) (/ trazo 3) )
        (turn -120)
        (koch (- nivel 1) (/ trazo 3) )
        (turn 60)
        (koch (- nivel 1) (/ trazo 3) )

        )

  )
 )


(turtles #t)
;(koch 1 600)


;;b copo

(define (copo-nieve nivel trazo)
      (begin
         (koch nivel trazo)
         (turn -120)
         (koch nivel trazo)
         (turn -120)
         (koch nivel trazo)
         )
  )

;;Alfombra

(define (cuadrado w)
   (begin
      (draw w)
      (turn 90)
      (draw w)  ;;Aux triangulo
      (turn 90)
      (draw w)
      (turn 90)
      (draw w)
      (turn 90)
      )
  )

(define (alfombra1 w)
  (begin
    (alfombra-sierpinski (/ w 3))
    (move (/ w 3))
    (alfombra-sierpinski (/ w 3))
    (move (/ w 3))
    (alfombra-sierpinski (/ w 3))
    (move (/ w 3))

    
    )
  )

(define (alfombra-sierpinski w)
   (if (> w 20)
      (begin
         (alfombra1 w)
         (turn 90) 
         ;;abaj
         (alfombra1 w)
         ;;der
         (turn 90) 
         (alfombra1 w)
         ;;arr
         (turn 90) 
         (alfombra1 w)
         ;;izq
         (turn 90) 
         )
              
      (cuadrado w)))

(define (alfombra-sierpinski-ventana w)
  (turtles #t)
  (clear)
  (turn -90)
  (move (/ w 2))
  (turn -90)
  (move (/ w 2))
  (turn 180)
  (alfombra-sierpinski w))

(alfombra-sierpinski-ventana 80)






