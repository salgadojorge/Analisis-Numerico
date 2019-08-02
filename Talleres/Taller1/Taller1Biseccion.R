
# -------------------------------------------------------------------------------------
# Código realizado por: 
# * Natalia Navas
# * Jorge Salgado 
# * Sebastián Santamaría
# -------------------------------------------------------------------------------------
# Clase análisis numérico mier-vier 1903
# -------------------------------------------------------------------------------------

biseccion = function(f, xa, xb, tol)
{
  if( sign(f(xa)) == sign(f(xb)) ){ stop("f(xa) y f(xb) tienen el mismo signo") 
  }
  
  a = xa; b = xb
  k = 0
  dx = 0
  
  # Listas que contendrán los valores a graficar
  listaErrorAnt = c()
  listaIteraciones = c()
  listaErrorAct = c()
  
  #Par imprimir estado
  cat("----------------------------------------------------------\n")
  cat(formatC( c("a","b","m","Error est.","Error ant."), width = -15, format = "f", flag = " "), "\n")
  cat("----------------------------------------------------------\n")
  
  repeat
  {
    # Contador de iteraciones
    k=k+1
    m = a + 0.5*(b-a)
    if( f(m)==0 )
    { 
      cat("Cero de f en [",xa,",",xb,"] es: ", m ) 
    }
    if( sign(f(a)) != sign(f(m)) )
    {
      b = m
    } else { a = m }
    eAnterior = dx
    dx = (b-a)/2
    
    #Aquí se inserta el error relativo en la lista 
    if(k>1){
      listaErrorAnt = c(listaErrorAnt , eAnterior)
      listaIteraciones = c(listaIteraciones, k)
      listaErrorAct = c(listaErrorAct, dx)
    }
    
    # imprimir estado
    cat(formatC( c(a,b,m,dx,eAnterior), digits=7, width = -15, format = "f", flag = " "), "\n")
    
    #until
    if( dx < tol )
    {
      cat("----------------------------------------------------------\n\n")
      cat("Cero de f en [",xa,",",xb,"] es approx: ", m, "con error <=", dx, " El numero de iteraciones es: ", k)
      break;
    }
  } #repeat
  
  # Inserción de puntos y líneas en la gráfica
  points(listaIteraciones, listaErrorAnt, col = "red")
  lines(listaIteraciones, listaErrorAnt, col = "blue")
  # Grafica de error actual vs error anterior (Comentar 2 lineas anteriores
  # y descomentar 2 lineas siguientes, cambiar xlim=c(0,0.13))
  # points(listaErrorAct, listaErrorAnt, col = "red")
  # lines(listaErrorAct, listaErrorAnt, col = "blue")
}


## Pruebas
f = function(x) ((x^3)+(5*x)-1)
#curve(f, -2,30, col ="red"); abline(h=0, v=0) #gráfico para decidir un intervalo
plot(f, xlim=c(0,27), ylim=c(0,0.3), col = "white")
abline(h=0,col="black")
biseccion(f, 0, 1, 0.00000001)
