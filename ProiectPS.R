set.seed(123)

n <- 100 #Nr etape
lambda <- runif(n, 0.5, 2) #Rata exp pt fiecare etapa
alpha <- runif(n-1, 0.8, 1) #Prob trecerii la urmatoarea etapa
alpha<-c(1,alpha)#prob ca o persoana sa participe la prima etapa este 100%
nrSimulari <- 1000000 #Nr simulari

simulator <- function() {
  Ti <- rexp(n, rate = lambda)  # Timpul pentru fiecare etapă
  
  
  stop_point <- which(runif(n - 1) > alpha[-1])[1]  #Determinam momentul opririi
  
  #Timpul total va fi cel pana la oprire/finalul vectorului daca nu se opreste
  if (!is.na(stop_point)) {
    return(sum(Ti[1:stop_point])) 
  } else {
    return(sum(Ti))
  }
}

# Simulam 10^6 
valoriT <- replicate(nrSimulari, simulator())

# 1) Aproximarea E[T] și graficul distribuției T
approx_E_T <- mean(valoriT) #facem media vectorului cu valorile fiecarei simulari
hist(valoriT, breaks = 100, main = "Distribuția timpilor T", xlab = "T") #afisam histograma deoarece afiseaza clar rezultatele simularii

cat("Valoarea aproximată a lui E(T):", approx_E_T, "\n")

# 2) Calcul exact al lui E(T)
exact_E_T <- sum(1 / lambda * cumprod(alpha)) #

cat("Valoarea exactă a lui E(T):", exact_E_T, "\n")

# 3) Probabilitatea ca persoana să finalizeze activitatea
prob_finalizare <- mean(valoriT >= sum(1 / lambda))
cat("Probabilitatea de finalizare a activității:", prob_finalizare, "\n")

# 4) Probabilitatea de a finaliza în timp <= sigma
sigma <-94

prob_sigma<- mean(valoriT <= sigma & valoriT >= sum(1 / lambda))
cat("Probabilitatea de finalizare în timp <= sigma:", prob_sigma, "\n")

# 5) Timpul minim și maxim
timpMin <- min(valoriT[valoriT >= sum(1 / lambda)] )
timpMax <- max(valoriT[valoriT >= sum(1 / lambda)] )
cat("Timpul minim:", timpMin, "Timpul maxim:", timpMax, "\n")

hist(valoriT[valoriT >= sum(1 / lambda)], breaks=50, main="Distribuția timpurilor de finalizare (valide)", xlab="Timpul de finalizare (T)", ylab="Frecvența", col="lightblue", border="black")


# 6) Probabilitatea de a se opri înainte de etapa k
k <- 100
PStopK <- mean(sapply(valoriT, function(x) {
 if(x<=sum(1/lambda[1:k-1]))
   return (TRUE)
  else
    return(FALSE)
}))
calculate_PStopBeforeK <- function(valoriT, lambda, alpha, n) {
  # Calculăm timpii cumulativi pentru fiecare etapă
  cumulative_times <- cumsum(1 / lambda)
  
  # Calculăm probabilitățile pentru fiecare k
  PStopBeforeK <- sapply(2:n, function(k) {
    mean(valoriT <= cumulative_times[k - 1])  # Probabilitatea de oprire înainte de etapa k
  })
  
  return(PStopBeforeK)
}
PStopBeforeK <- calculate_PStopBeforeK(valoriT, lambda, alpha, n)


cat("Probabilitatea de oprire înainte de etapa", k, ":", PStopK, "\n")
barplot(PStopBeforeK, names.arg = 2:n, col = "lightblue",
        main = "Probabilitățile de oprire înainte de etapa k",
        xlab = "Etapa k", ylab = "Probabilitatea de oprire",
        xlim = c(1, n), ylim = c(0, 1))


