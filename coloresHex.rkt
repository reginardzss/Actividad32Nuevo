#lang racket

(define archivo (open-input-file "ejemplo3_4.txt"))

;Imprimir listas
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
  
;Comentarios
(define (comentarios p1 atomo)
  (list "Beige" (list atomo (read-line p1))) ;lee toda la linea
  )

;operadores
(define (operadores atomo)
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
    [(string? atomo) (list "Azul " atomo) ]
    [else (list "Blanco " atomo)]
  )
)

;listas
(define (listas lst)
  (if (empty? lst)
      (list)
      (append (list(operadores (car lst)) (listas (cdr lst))))) ;lee cada elemento de la lista y lo clasifica
  )

;Dividir cada caso
(define (convierte p1 atomo)
  (cond
    [(integer? atomo) (real? atomo) (equal? 'if atomo) (equal? 'for atomo) (equal? 'while atomo) (equal? 'else atomo) (equal? 'elif atomo) (equal? 'in atomo) (equal? 'with atomo) (equal? 'as atomo) (equal? 'def atomo) (equal? '= atomo) (equal? ': atomo) (equal? 'print atomo) (equal? 'input atomo) (equal? 'import atomo) (equal? 'true atomo) (equal? 'false atomo)]
    [(equal? '#' atomo) (comentarios p1 atomo)]
    ;[(equal ' atomo) (comentarios p1 atomo)]
    [(list? atomo) (listas atomo)]
    ;[(list? atomo) (list "LISTA:       " atomo)]
    [else (list "VARIABLE:       " atomo)])
  )

;Recorrer el archivo
(define (recorre-2 p1)
  (if (eof-object? (peek-char p1))
      '()
      (append (list(convierte p1 (read p1))) (recorre-2 p1)))
  )

;Correr las funciones
(define (recorre file1 file2)
  (cond [(file-exists? file2 ) (delete-file file2) ])
  
  (define p1 (open-input-file file1))
  (define p2 (open-output-file file2))
  (define lista (recorre-2 p1))
  (display lista)
  (define cantidad (imprime lista p2))
  (display "Cantidad de Tokens: " p2)
  (close-output-port p2)
  (close-input-port p1)
  )

 ;(recorre "ejemplo3_4.txt" "salida3_4.txt")