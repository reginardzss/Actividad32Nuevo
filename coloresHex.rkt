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
  

;Segundo filtro
(define (operadores atomo)
 
  (cond
    [(regexp-match #rx"^if$|^for$|^while$|^else$|^elif$|^in$|^with$|^as$" atomo) (cons(cons  (list "<span style='color: #ffe4c4'>") ( list atomo)) '( "</span>" ))]
    ;[(regexp-match #rx"1|2|3|4|5|6|7|8|9|0" atomo) (cons (cons (list "<span style='color: #ffa500'>" (list atomo) )) '("</span>") ) ]
    [(regexp-match #rx"^def$" atomo)  (cons (cons (list "<span style='color: #00FFFF'>") (list atomo)) (list "</span>")) ]
    [(regexp-match #rx"=|:|>|<|" atomo)  (cons(cons (list "<span style='color: #FFFFFF'>") (list atomo)) '("</span>")) ]
    [(regexp-match #rx"^print$|^input$" atomo)  (cons(cons (list "<span style='color: #9932CC'>") (list atomo)) '("</span>")) ]
    [(regexp-match #rx"^import$" atomo)  (cons(cons (list "<span style='color: #FF0000'>") (list atomo)) '("</span>")) ]
    ;[(regexp-match #rx" "" " atomo)  (append (list "<span style='color: #ffe4c4'>") (list atomo) ("</span>")) ]
    [(regexp-match #rx"^true$|^false$" atomo)  (cons(cons (list "<span style='color: #ffe4c4'>") (list atomo)) '("</span>")) ]
    [else (cons(cons (list "<span style='color: #FFFFFF'>") (list atomo)) '("</span>"))]
  )
)



;Recorrer el archivo
(define (recorre-2 p1)
  ;(define p2(current-output-port file2))
  (display "<div></div>" (current-output-port))
  (if (eof-object? (peek-char p1))
      '()
      ;aqui se agrega el div de html
      (append (list(operadores (read-line p1)))  (recorre-2 p1)) )
  
  )

;Correr las funciones
(define (recorre file1 file2)
  (cond [(file-exists? file2 ) (delete-file file2) ])

  (define p1(open-input-file file1))
  (define p2(open-output-file file2))
  ;header html
  (display "<!DOCTYPE html>" p2)
  (newline p2)
  (display "<html>" p2)
  (newline p2)
  (display "<body style='background-color:black;'>" p2)
  (newline p2)

     
  ;recorrer archivo de entrada
  (define lista (recorre-2 p1))
  (display lista)
  (define cantidad (imprime lista p2))

  ;footer html
  (newline p2)
  (display "<div></div>" p2)
  (newline p2)
  (display "</body>" p2)
  (newline p2)
  (display "</html>" p2)
  ;cierre de archivos 
  (close-output-port p2)
  (close-input-port p1)
  )

 ;(recorre "ejemplo3_4.txt" "salida3_4.txt")
;(recorre "ejemplo3_4.txt" "salida3_4.html")