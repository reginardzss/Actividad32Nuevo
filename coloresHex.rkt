;Actividad Integradora 3.4 Resaltador de sintaxis (Evidencia de Competencia)
; Paulina Lopez Holguin A01284186
; Pablo Navarro Zepeda A01284116
; Regina Rodriguez Sanchez A01284329

;Imprimir listas
(define (imprime lista p2)

  (define (despliega lista p2)
    (display (car lista) p2)
    (display " " p2)
    (newline p2)
      1)
   (if (empty? lista)
      0  
   (+ (despliega (car lista) p2) (imprime (cdr lista) p2))))

;Funcion auxiliar para convertir lista a string
(define (slist->string slst)
  (string-join slst " ")
  )

;Primer filtro
(define (filtro1 linea) 
  (define lista_linea (string-split linea)) ; Agarra la linea y la hace una lista de stings (palabra por palabra)

  (if (empty? lista_linea)
      '("")   
      (cond
        [(regexp-match #rx"#" linea)  (list(string-append(string-append(string-append  "<span style='color: #E1C699'>" linea) "</span>")"<div> </div>"))] ;Si la linea es un comentario
        [else
         (if (empty? (cdr lista_linea))
         (list (string-append (string-append (filtro2 (car lista_linea))  )  "<div></div>"))
         (list (string-append (string-append (filtro2 (car lista_linea))  (slist->string(filtro1 (slist->string  (cdr lista_linea))) )))  "<div> </div>")) ; HACE ERROR
     ]
    )
  )
  )


;Segundo filtro 
(define (filtro2 atomo)
    (cond ;Todas las condiciones buscan palabras y si se encuentran cambian le agregan la configuracion html
      
    [(regexp-match #rx"^if$|^for$|^while$|^else$|^elif$|^in$|^with$|^as$" atomo)  (string-append(string-append   " <span style='color: #FF00FF'>"   atomo)  " </span>" )]
    [(regexp-match #rx"^int$|^str$" atomo)  (string-append(string-append " <span style='color: #3CB371'>" atomo) "</span>") ]
    [(regexp-match #rx"^([1-9]|[1-9][0-9]|[1-9][0-9][0-9]|[1-9][0-9][0-9][0-9])$" atomo) (string-append(string-append " <span style='color: #FFA500'>"  atomo) "</span>")  ]
    [(regexp-match #rx"^def$|^return$" atomo)  (string-append(string-append " <span style='color: #00FFFF'>"  atomo) " </span>") ]
    [(regexp-match #rx"=|:|>|<" atomo)  (string-append(string-append " <span style='color: #FFFFFF'>"  atomo) " </span>") ]
    [(regexp-match #rx"^print$|^input$" atomo)  (string-append(string-append " <span style='color: #9932CC'>" atomo) " </span>") ]
    [(regexp-match #rx"import" atomo)  (string-append(string-append " <span style='color: #FF0000'>"  atomo) "</span>") ]
    [(regexp-match #rx"^True$|^False$" atomo)  (string-append(string-append " <span style='color: #FFFF00'>"  atomo) " </span>") ]
    [else (string-append(string-append " <span style='color: #FFFFFF'>" atomo) " </span>")]
  )
)



;Recorrer el archivo
(define (recorre-2 p1)
  (if (eof-object? (peek-char p1)) ;Checa si es el final del archivo
      '() ;Caso Base: Regresa vacio si es el final del archivo
      (append (list(filtro1 (read-line p1))) (recorre-2 p1)) ) ;Agarra la siguiente linea del archivo (read-line) y entra a la función "filtro1" y vuelve a llamar a la funcion recorre para armar la lista de lineas ya cambiadas
  )

;Correr las funciones
(define (recorre file1 file2)
  (cond [(file-exists? file2 ) (delete-file file2) ]) ;Elimina el archivo de salida si existe

  (define p1(open-input-file file1)) ;Abre el archivo de entrada
  (define p2(open-output-file file2)) ;Abre el archivo de salida

  ;Imprime los headers de html
  (display "<!DOCTYPE html>" p2) 
  (newline p2)
  (display "<html>" p2)
  (newline p2)
  (display "<body style='background-color:black;'>" p2)
  (newline p2)

  ;Recorre archivo de entrada
  (define lista (recorre-2 p1)) ;Crea una lista llamada "lista" usando la función recorre-2 con el archivo de entrada "p1"
  (display lista) ;Imprime "lista"
  (define cantidad (imprime lista p2)) ;Crea variable "cantidad" con resultado de la función imprime usando la variable "lista" y el archivo de salida "p2"

  ;Footer html
  (newline p2)
  (display "<div></div>" p2)
  (newline p2)
  (display "</body>" p2)
  (newline p2)
  (display "</html>" p2)
  
  ;Cierre de archivos 
  (close-output-port p2)
  (close-input-port p1)
  
  )

(recorre "archivoEntrada.txt" "salida3_4.html")