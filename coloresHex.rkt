#lang racket

;Imprimir listas
(define (imprime lista p2)

  (define (despliega lista p2)
    (display (first lista) p2)
    (display " " p2)
    ;(display (second lista) p2)
    (newline p2)
    (newline p2)
      1)
  
   (if (empty? lista)
      0
      
      (+ (despliega (car lista) p2) (imprime (cdr lista) p2))))

;funcion auxiliar para convertir lista a string
(define (slist->string slst)

  ;(display (string-join (map symbol->string slst) " "))
  (string-join slst " ")
  )

;primer filtro
(define (filtro1 linea)

  (define lista_linea (string-split linea))

  (if (empty? lista_linea)
      '("")
      
  (cond
    [(regexp-match #rx"#" linea)  (list(string-append(string-append(string-append  "<span style='color: #E1C699'>" linea) "</span>")"<div></div>"))]
    
    [else

     (if (empty? (cdr lista_linea))
         ;'("")
         (list (string-append (string-append (filtro-2 (car lista_linea))  )  "<div></div>"))

         (list (string-append (string-append (filtro-2 (car lista_linea))  (slist->string(filtro1 (slist->string  (cdr lista_linea))) )))  "<div></div>")
         )
     ]
    ;[else (list (string-append (string-append (operadores (car lista_linea))  (slist->string(filtro1 (slist->string (cdr lista_linea)))) ) "<br>")) ]
    )

  )

  )


;Segundo filtro
(define (filtro-2 atomo)
 
    (cond
    [(regexp-match #rx"^if$|^for$|^while$|^else$|^elif$|^in$|^with$|^as$" atomo)  (string-append(string-append   " <span style='color: #FF00FF'>"   atomo)  "</span>" )]
    [(regexp-match #rx"^([1-9]|[1-9][0-9]|[1-9][0-9][0-9]|[1-9][0-9][0-9][0-9])$" atomo) (string-append(string-append " <span style='color: #FFA500'>"  atomo) "</span>")  ]
    [(regexp-match #rx"^def$" atomo)  (string-append(string-append " <span style='color: #00FFFF'>"  atomo) "</span>") ]
    [(regexp-match #rx"=|:|>|<" atomo)  (string-append(string-append " <span style='color: #FFFFFF'>"  atomo) "</span>") ]
    [(regexp-match #rx"^print$|^input$" atomo)  (string-append(string-append " <span style='color: #9932CC'>" atomo) "</span>") ]
    [(regexp-match #rx"^import$" atomo)  (string-append(string-append " <span style='color: #FF0000'>"  atomo) "</span>") ]
    [(regexp-match #rx"^int$|^str$" atomo)  (string-append(string-append " <span style='color: #3CB371'>" atomo) "</span>") ]
    [(regexp-match #rx"^True$|^False$" atomo)  (string-append(string-append " <span style='color: #FFFF00'>"  atomo) "</span>") ]
    [else (string-append(string-append " <span style='color: #FFFFFF'>" atomo) "</span>")]
  )
)



;Recorrer el archivo
(define (recorre-2 p1)
  (if (eof-object? (peek-char p1))
      '()

      (append (list(filtro1 (read-line p1)))  (recorre-2 p1)) )
  
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
(recorre "archivoEntrada.txt" "salida3_4.html")
;(recorre "archivoEntrada.txt" "salida3_4.txt")