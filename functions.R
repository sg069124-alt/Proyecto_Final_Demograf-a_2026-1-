

lt_abr <- function(x, mx, sex="f", IMR=NA){
  
  m <- length(x)
  n <- c(diff(x), NA)  
  
  ax <- n/2    
  
  # Pag. 4 notas de clase - cuadro
  
  ## Coale y Demeny edades 0 a 1
  
  if(sex=="m"){
    if(mx[1]>=0.107){ ax[1] <- 0.330 }else{
      ax[1] <- 0.045+2.684*mx[1]
    } 
  } else if(sex=="f"){
    if(mx[1]>=0.107){ ax[1] <- 0.350 }else{
      ax[1] <- 0.053+2.800*mx[1]
    }  
  }
  
  ## Coale y Demeny edades 1 a 4
  if(sex=="m"){
    if(mx[1]>=0.107){ ax[2] <- 1.352 }else{
      ax[2] <- 1.651-2.816*mx[1]
    } 
  } else if(sex=="f"){
    if(mx[1]>=0.107){ ax[2] <- 1.361 }else{
      ax[2] <- 1.522-1.518*mx[1]
    }  
  }
  
  # Probabilidad de muerte
  qx <- (n*mx)/(1+(n-ax)*mx)
  qx[m] <- 1
  
  # Proba de sobrevivir
  px <- 1-qx
  
  # l_x
  lx <- 100000 * cumprod(c(1,px[-m]))
  
  # Defunciones
  dx <- c(-diff(lx), lx[m])
  
  # Años persona vividos
  Lx <- n* c(lx[-1], 0) + ax*dx
  Lx[m] <- lx[m]/mx[m]
  
  # Años persona vividos acumulados
  
  Tx <- rev(cumsum(rev(Lx)))
  
  # Esperanza de vida
  ex <- Tx/lx
  
  return(data.table(x, n, mx, ax, qx, px, lx, dx, Lx, Tx, ex))
  
  
}

# Uso la función lt_abr
# lt_abr(x, mx)



# 3. Crecimiento exponencial ----

expo <- function(N_0, N_T, t_0, t_T, t) {
  
  dt <- decimal_date(as.Date(t_T)) - decimal_date(as.Date(t_0))
  r <- log(N_T/N_0)/dt
  
  h <- t - decimal_date(as.Date(t_0))
  N_h <- N_0 * exp(r*h)  
  
  return(N_h)
} 
  # -------- FIN ----------*

