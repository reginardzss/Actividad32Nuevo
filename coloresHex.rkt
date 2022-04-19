#lang racket
(define (imprime lista p2)
  (define (despliega lista p2)
    (display (first lista) p2)
    (display " " p2)
    (display (second lista) p2)
    (newline p2)
      1)
  
   (if (empty? lista)
      0
      
      (+ (despliega (car lista) p2) (imprime (cdr lista) p2))))

(define (get-atomo atomo)
  (cond
    [(integer? atomo)(list "Naranja " atomo) ]
    [(real? atomo) (list "Naranja " atomo) ]
    [(equal? 'if atomo) (list "Magenta " atomo) ]
    [(equal? 'for atomo) (list "Magenta " atomo) ]
    [(equal? 'while atomo) (list "Magenta " atomo) ]
    [(equal? 'else atomo) (list "Magenta " atomo) ]
    [(equal? 'elif atomo) (list "Magenta " atomo) ]
    [(equal? 'in atomo) (list "Magenta " atomo) ]
    [(equal? 'with atomo) (list "Magenta " atomo) ]
    [(equal? 'as atomo) (list "Magenta " atomo) ]
    [(equal? 'def atomo) (list "Cyan " atomo) ]
    [(equal? '= atomo) (list "Blanco " atomo) ]
    [(equal? ': atomo) (list "Blanco " atomo) ]
    [(equal? 'print atomo) (list "Morado " atomo) ]
    [(equal? 'input atomo) (list "Morado " atomo) ]
    [(equal? 'import atomo) (list "Rojo " atomo) ]
    [(equal? 'true atomo) (list "Amarillo " atomo) ]
    [(equal? 'false atomo) (list "Amarillo " atomo) ]
    [(equal? "#" atomo) (list "Beige " atomo) ]
    [(string? atomo) (list "Azul " atomo) ]
    
    
    
    [else (list "Blanco " atomo)]

    #|
    Falta parentesis y comenytarios
    |#
      ))



(define (recorre-2 p1)
  (if (eof-object? (peek-char p1))
     '()
     (append (list (get-atomo (read p1))) (recorre-2 p1)))
  )

(define (recorre file1 file2)
  (cond [(file-exists? file2 ) (delete-file file2) ])
  
  (define p1(open-input-file file1))
  (define p2(open-output-file file2))
  (define lista (recorre-2 p1))
  (display lista)
  (define cantidad (imprime lista p2))
  (display "Cantidad de Tokens: " p2 )
  (display cantidad p2)
  (close-output-port p2)
  (close-input-port p1)
  )

;(recorre "ejemplo3_4.txt" "salida3_4.txt")