# FALSA POSICION - PUNTO 5
f <- function(t) { 3*sin(t)^3-1 }                                     
g <- function(t) { 4*sin(t)*cos(t) }

cant_decimales <- function(x, k) trimws(format(round(x, k), nsmall=k))
raices <- list()

falsaPosicion <- function(a,b) {
  tol <- 10^-8
  it <- 0
  num_raiz <- 1
  
  #GRAFICAS PUNTO 5
  x=seq(0,2*pi,length=100)
  plot(x,f(x),type="l",col="blue",lwd=3,main="f(t) y g(t)",xlab="",ylab="",las=1,col.axis="red")
  lines(x,g(x),col="green",lwd=3)
  legend("bottomleft",col=c("blue","green"),legend =c("f(t) = 3sin^3(t)-1","g(t) = 4sin(t)cos(t)"), lwd=3, bty = "n")
  abline(h = 0, col = "black")
  
  if(f(a) != g(a) && f(b) != g(b)) {
    x_0 <- a
    x_1 <- b
    x_2 <- x_1 - ( (f(x_1)*(x_1-x_0)) / (f(x_1)-f(x_0)) )
    error <- abs(f(x_2))
    
    if( (f(x_0) < g(x_0) && f(x_1) > g(x_1)) | (g(x_0) < f(x_0) && g(x_1) > f(x_1)) ) { #Se cruzan
      
      while(abs(f(x_2)-g(x_2)) > tol) {
        
        if( (f(x_0) < g(x_0) && f(x_2) > g(x_2)) | (g(x_0) < f(x_0) && g(x_2) > f(x_2)) ) { #Se cruzan
          x_0 <- x_0
          x_1 <- x_2
          x_2 <- x_1 - ( (f(x_1)*(x_1-x_0)) / (f(x_1)-f(x_0)) )
          
          if(abs(f(x_2)-g(x_2)) <= tol) {
            raiz <- cant_decimales(x_2, 16)
            raices[[num_raiz]] <- raiz
            #raices[[num_raiz]] <- x_2
            num_raiz <- num_raiz+1
          }
        }
        else {
          x_0 <- x_2  
          x_1 <- x_1     
          x_2 <- x_1 - ( (f(x_1)*(x_1-x_0)) / (f(x_1)-f(x_0)) ) 
          
          if(abs(f(x_2)-g(x_2)) <= tol) {
            raiz <- cant_decimales(x_2, 16)
            raices[[num_raiz]] <- raiz
            #raices[[num_raiz]] <- x_2
            num_raiz <- num_raiz+1
          }
        }
        
        error <- abs(f(x_2))
        cat("\nIt: ", it+1, "\tX: ", cant_decimales(x_2, 8), "\tError: ", cant_decimales(error, 8))
        
        it <- it+1
      }
      
    }
    else{ 
      cat("\nNo se pudo resolver en el intervalo (", a, " , ", b, "), intente con un intervalo diferente")
    }
  }
  else if (f(a) == g(a)) {
    raices[[num_raiz]] <- a
  }
  else {
    raices[[num_raiz]] <- b
  }
  
  if(length(raices) > 0) {
    cat("\n\nLa raiz hayada mediante el método de la falsa posición fue: \n")
    print(raices)
  }
  
  for (i in raices) {
    points(i,0)
    abline(v = i, col = "red")
  }
}

falsaPosicion(2,4)