# ejercicios tp4
# Ezequiel Perez

##----------------------------------------------------------------------------##

# Modelos estocasticos


#### ejercicio 1: Calcular la probabilidad de extinción en 30 años para el modelo de crecimiento exponencial 
#### en tiempo discreto con estocasticidad demográfica para R = 1.2 y n(0) = 10.----

# variables
niter = 30    # anios
nreps = 10000   # repeticiones
n     = matrix(0, niter, nreps)
R     = 1.2   # tasa de crecimiento
n[1,] = 10    # pob inicial
# simulaciones de modelo exponencial estocastico
for(j in 1:nreps){
  for(i in 2:niter){
    n[i,j] <- rpois(1, n[i-1, j]*R)
  }
}
# contando la cantidad de simulaciones que se extinguieron.
counts = 0
for (j in 1:nreps){
  if (n[niter, j] <= 0){
  counts = counts + 1
    }
}
# estimando la probabilidad de extincion.
prob = counts/nreps
prob



#### ejercicio 2: Hacer lo mismo para el modelo logístico con estocasticidad demográfica.----

# variables
niter = 30    # anios
nreps = 10000   # repeticiones
n     = matrix(0, niter, nreps)
r     = 0.2   # tasa de crecimiento
n[1,] = 10    # pob inicial
K     = 100
# simulaciones de modelo logistico estocastico
for(j in 1:nreps){
  for(i in 2:niter){
    n[i,j] <- rpois(1, (1 + r * (1 - n[i-1, j]/K)) * n[i-1, j])
  }
}
# contando la cantidad de simulaciones que se extinguieron.
counts = 0
for (j in 1:nreps){
  if (n[niter, j] <= 0){
    counts = counts + 1
  }
}
# estimando la probabilidad de extincion.
prob = counts/nreps
prob



#### ejercicio 3: Ahora comparar el resultado del ejercicio (1) con la probabilidad de extinción a 30 años
#### del modelo de crecimiento exponencial con estocasticidad ambiental.----

# variables
niter = 30    # anios
nreps = 10000   # repeticiones
n     = matrix(0, niter, nreps)
R     = 1.2   # tasa de crecimiento
n[1,] = 10    # pob inicial
Rb    = 1.5
Rm    = 0.5
# simulaciones de modelo exponencial estocastico (estocasticidad ambiental y demografica)
for(j in 1:nreps){
  for(i in 2:niter){
    R = sample(c(Rb, Rm), 1, replace = TRUE, prob = c(0.7, 0.3))
    n[i,j] <- rpois(1, n[i-1, j]*R)
  }
}
# contando la cantidad de simulaciones que se extinguieron.
counts = 0
for (j in 1:nreps){
  if (n[niter, j] <= 0){
    counts = counts + 1
  }
}
# estimando la probabilidad de extincion.
prob = round(counts/nreps, 2)
prob

# la probabilidad de extincion aumenta cuando se le agrega estocasticidad ambiental.



#### ejercicio 4: Mostrar cómo cambia la probabilidad de extinción a 30 años para el modelo usado en
#### (1) y en (3) para distintos valores de n(0), empezando en 2 hasta 16 con aumentos de 2.----


## modelo con estocasticidad demografica
exponencial1 = function(N0){
  # variables
  niter = 30    # anios
  nreps = 1000   # repeticiones
  n     = matrix(0, niter, nreps)
  R     = 1.2   # tasa de crecimiento
  n[1,] = N0   # pob inicial
  # simulaciones de modelo exponencial estocastico
  for(j in 1:nreps){
    for(i in 2:niter){
      n[i,j] <- rpois(1, n[i-1, j]*R)
    }
  }
  # contando la cantidad de simulaciones que se extinguieron.
  counts = 0
  for (j in 1:nreps){
    if (n[niter, j] <= 0){
      counts = counts + 1
    }
  }
  # estimando la probabilidad de extincion.
  prob = counts/nreps
  return(prob)
}



## modelo con estocasticidad demografica y ambiental
  exponencial2 = function(N0){
  # variables
  niter = 30    # anios
  nreps = 1000   # repeticiones
  n     = matrix(0, niter, nreps)
  R     = 1.2   # tasa de crecimiento
  n[1,] = N0   # pob inicial
  Rb    = 1.5
  Rm    = 0.5
  # simulaciones de modelo exponencial estocastico (estocasticidad ambiental y demografica)
  for(j in 1:nreps){
    for(i in 2:niter){
      R = sample(c(Rb, Rm), 1, replace = TRUE, prob = c(0.7, 0.3))
      n[i,j] <- rpois(1, n[i-1, j]*R)
    }
  }
  # contando la cantidad de simulaciones que se extinguieron.
  counts = 0
  for (j in 1:nreps){
    if (n[niter, j] <= 0){
      counts = counts + 1
    }
  }
  # estimando la probabilidad de extincion.
  prob = round(counts/nreps, 2)
  return(prob)
}

  
## probabilidad de extincion segun N0
  
enes = seq(2, 16, 2)
probabilidad1 = numeric(length(enes))
probabilidad2 = numeric(length(enes))
for (N in 1:length(enes)){
  probabilidad1[N] = exponencial1(enes[N])
  probabilidad2[N] = exponencial2(enes[N])
}


## ploteos

op <- par(cex.lab=1, font.lab = 2, bty ="n", las = 1)
plot(enes, probabilidad1, lwd = 2, type = "b", pch = 5, col = 5,
     ylab = "probabilidad de extincion", xlab = "pob. inicial (N0)", ylim = c(0,1))
lines(enes, probabilidad2, lwd = 2, type = "b", pch = 6, col = 6)
legend(x = "topright", legend = c("demografica", "demografica + ambiental"), col = c(5, 6), 
       pch = c(5,6), cex = 0.8, title = "Estocasticidad")
