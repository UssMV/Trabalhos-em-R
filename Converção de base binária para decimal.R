rm(list=ls())

## Algoritmo para conversão de base dercimal inteiro para binária 


y <-23        # escolhendo o número natural positivo
j <- 1        
a <- numeric()
quosciente <- y    


while(quosciente != 0){     # laço de repeticão com While com condição de parada
  a[j] <- quosciente %% 2   # quonciente diferente de 0(resto da divisão)
  quosciente <- quosciente %/% 2
  j <- j+1
  
  
}

j <- j-1        # usando valor de interacoes armazenado em j no laço acima            
b = numeric(j)
b[j] = a[j]

for (i in (j-1): 1) {
  b[i] <- a[i] + 10*b[i+1]
  
  
}


cat("Convercao requerida:",b[i])


##########################################################################

#----------------------------------------------------------------------------

## Algoritmo para conversão de base binária para inteiro positivo


y <-10111       # número binário nao quebrado
j <- 1
a <- numeric()
quosciente <- y


while(quosciente != 0){             # usando a mesma ideia matemática na prática 
                                    # para conversão para números inteiros 
  a[j] <- quosciente %% 10          # positivos
  quosciente <- quosciente %/% 10
  j <- j+1
  
  
}

j <- j-1
b = numeric(j)
b[j] = a[j]

for (i in (j-1): 1) {
  b[i] <- a[i] + 2*b[i+1]
  
  
}


cat("Convercao requerida:",b[i])

