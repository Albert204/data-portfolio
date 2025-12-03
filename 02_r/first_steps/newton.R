options(digits=9)#mas decimales

ewton_raphson <- function(f, df, x0, tol=1e-8, max_iter=100) {
 
  solutions <- numeric(max_iter)#crea un vector 0 de max_iter valores(100)
  errors <- numeric(max_iter)
  
  h <- f(x0)/df(x0)
  i <- 0
  
  while (abs(h) > tol && i < max_iter) {
    #print(abs(h))
    h <- f(x0)/df(x0)
    
    # Guardar la solución y el error actuales
    solutions[i+1] <- x0
    errors[i+1] <- abs(h)
    
    # x(i+1) = x(i) - f(x(i)) / f'(x(i))
    #print(x0)
    x0 <- x0 - h
    i <- i + 1
  }
  
  # Recortar vectores a la longitud del número necesitado de iteraciones
  solutions <- solutions[1:i]
  errors <- errors[1:i]
  
  if (i == max_iter) {
    cat("El método no convergió después de", max_iter, "iteraciones")
  } else {
    print(x0)
    cat("El método convergió después de", i, "iteraciones")
  }
  
  # Graficas resultados
  plot(1:length(solutions), solutions, type="b", col="blue", xlab="Iteración", ylab="Solución", main="Solución por Iteración")
  plot(1:length(errors), errors, type="b", col="red", xlab="Iteración", ylab="Error", main="Error por Iteración")
  
  plot(1:length(errors), errors, type="b", col="red", xlab="Iteración", ylab="Error", main="Error por Iteración en Escala Logarítmica", log="y")
  return(x0)
}
#----------------validación ecuación--------------------------------------
f <- function(x){
  x^2 - 2}# Función
df <- function(x){
  2*x}# Derivada

x0 <- 2 #x inicial
EJ = newton_raphson(f, df, x0)

#----------------primera ecuación--------------------------------------


f <- function(x) cos(5*x) - x^5
df <- function(x) -5*sin(5*x) - 5*x^4

x0 <- 1

sol = newton_raphson(f, df, x0)

#----------------segunda ecuación--------------------------------------

f <- function(x) x^3 - 2*x^2 + x
df <- function(x) 3*x^2 - 4*x + 1
x0 <- 0.1
newton_raphson(f, df, x0)
x0 <- 1.5
newton_raphson(f, df, x0)
