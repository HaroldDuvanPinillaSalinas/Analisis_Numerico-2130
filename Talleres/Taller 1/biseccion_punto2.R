# BISECCION - PUNTO 1
f <- function(x) { exp(x)-x-1 }                                        

cant_decimales <- function(x, k) trimws(format(round(x, k), nsmall=k))

raices <- list()

biseccion <- function(a,b) {
  tol <- 10^-8
  it <- 0
  num_raiz <- 1
  
  s <- seq(a, b, 0.01)
  plot(s, f(s), type = "l", col = "blue")
  abline(h = 0, col = "red")
  
  if(f(a) != 0 && f(b) != 0) {
    x_0 <- a
    x_1 <- b
    x_2 <- (x_0+x_1)/2
    error <- (x_1-x_0)/2
    
    if(f(x_0)*f(x_1) < 0) { #Tienen signos contrarios
      
      while(abs(f(x_2)) > tol) {
        
        if(f(x_0)*f(x_2) < 0) { #Tienen signos contrarios
          x_0 <- x_0
          x_1 <- x_2
          x_2 <- (x_0+x_1)/2
          
          if(abs(f(x_2)) <= tol) {
            raiz <- cant_decimales(x_2, 16)
            raices[[num_raiz]] <- raiz
            num_raiz <- num_raiz+1
          }
        }
        else {
          x_0 <- x_2  
          x_1 <- x_1     
          x_2 <- (x_0+x_1)/2 
          
          if(abs(f(x_2)) <= tol) {
            raiz <- cant_decimales(x_2, 16)
            raices[[num_raiz]] <- raiz
            num_raiz <- num_raiz+1
          }
        }
        
        error <- (x_1-x_0)/2
        cat("\nIt: ", it+1, "\tX: ", cant_decimales(x_2, 8), "\tError: ", cant_decimales(error, 8))
        
        it <- it+1
      }
      
    }
    else{ 
      cat("\nCon el método de bisección no fue posible hallar raices en la función dada en el intervalo (", a, " , ", b, ")")
    }
  }
  else if (f(a) == 0) {
    raices[[num_raiz]] <- a
  }
  else {
    raices[[num_raiz]] <- b
  }
  
  if(length(raices) > 0) {
    cat("\n\nLas raices hayadas mediante el método de bisección fueron: \n")
    print(raices)
  }
  
  for (i in raices) {
    points(i,0)
  }
  
}

biseccion(-2,2)