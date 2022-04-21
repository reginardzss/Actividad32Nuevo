#lang racket

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
  (display atomo)
  (cond
    [(regexp-match #rx"^if$|^for$|^while$|^else$|^elif$|^in$|^with$|^as$" atomo)  (append (list "<span style='color: #ffe4c4'>") (list atomo) ("</span>")) ] 
    [(regexp-match #rx"1|2|3|4|5|6|7|8|9|0" atomo) (list "<span style='color: #ffa500'>" atomo "</span>" ) ]
    [(regexp-match #rx"^def$" atomo)  (append (list "<span style='color: #00FFFF'>") (list atomo) ("</span>")) ]
    [(regexp-match #rx"=|:" atomo)  (append (list "<span style='color: #000000'>") (list atomo) ("</span>")) ]
    [(regexp-match #rx"^print$|^input$" atomo)  (append (list "<span style='color: #9932CC'>") (list atomo) ("</span>")) ]
    [(regexp-match #rx"^import$" atomo)  (append (list "<span style='color: #FF0000'>") (list atomo) ("</span>")) ]
    [(regexp-match #rx"import" atomo)  (append (list "<span style='color: #ffe4c4'>") (list atomo) '("</span>")) ]
    [(regexp-match #rx"^true$|^false$" atomo)  (append (list "<span style='color: #ffe4c4'>") (list atomo) '("</span>")) ]



    [(real? atomo) (list "Naranja " atomo) ]
    [(equal? 'def atomo) (list "Cyan " atomo) ]
    [(equal? "=" atomo) (list "Blanco " atomo) ]
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
    [(equal? "#" atomo) (comentarios p1 atomo)]
    ;[(equal? ' atomo) (comentarios p1 atomo)]
    [(list? atomo) (listas atomo)]
    [(list? atomo) (list "LISTA:       " atomo)]
    [else (list "VARIABLE:       " atomo)])
  )

;Recorrer el archivo
(define (recorre-2 p1)
  (if (eof-object? (peek-char p1))
      '()
      ;aqui se agrega el div de html
      (append (list(operadores (read-line p1))) (recorre-2 p1)))
  )

;Correr las funciones
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