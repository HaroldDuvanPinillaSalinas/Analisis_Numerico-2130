n <- 7                #Dato
E <- 10^-8            #Error permitido
x <- abs(3)           #valor inicial
y <- (x + n/x) / 2    #respuesta calculada con error E

cant_decimales <- function(x, k) trimws(format(round(x, k), nsmall=k))

valorOriginal = sqrt(n)
valorRedondeado = cant_decimales(sqrt(n), 8)
cat("El valor de la raiz obtenido mediante la funcion sqrt es de: ", valorRedondeado, "\n")

contador <- 0
while(abs(x-y) > E) {
  x <- y
  y <- (x + n/x) / 2 
  
  contador <- contador + 1
  cat("Iteracion ", contador, "\n")
  cat("El valor calculado es: ", cant_decimales(y, 8), "\n")
  cat("El error absoluto es: ", abs(valorOriginal - y), "\n")
  cat("El error relativo es: ", abs(valorOriginal - y)/valorOriginal, "\n\n")
}

cat("El resultado con un error absoluto de ", E, " es: ", cant_decimales(y, 8), "\n")
cat("El error absoluto al final es de: ", abs(valorOriginal - y), "\n")