# Documentație Proiect: Probabilitate și Statistică

**Membri:**  
- Gheorghe Bogdan-Alexandru  
- Andrei Cristian-David  
- Sincari Sebastian-George  
- Cosor Mihail  

**Data:** Ianuarie 2025  

---

## Cuprins

1. [Cerința I](#cerința-i)  
   1.1 [Descrierea Problemei](#descrierea-problemei)  
   1.2 [Simularea în R](#simularea-în-r)  
   1.3 [Rezultate și Interpretări](#rezultate-și-interpretări)  
2. [Cerința II](#cerința-ii)  
3. [Cerința III](#cerința-iii)  

---

## Cerința I

### Descrierea Problemei

Se consideră o activitate care presupune parcurgerea secvențială a $n$ etape. Timpul necesar finalizării etapei $i$ de către o persoană $A$ este o variabilă aleatoare $( T_i \sim \text{Exp}(\lambda_i) )$. După finalizarea etapei $i$, $A$ va trece în etapa $i+1$ cu probabilitatea $\alpha_i$ sau va opri lucrul cu probabilitatea $1 - \alpha_i$ . Fie $T$ timpul total petrecut de persoana $A$ în realizarea activității respective.

### Simularea în R

```r
set.seed(123)
n <- 100 # Nr etape
lambda <- runif(n, 0.5, 2) # Rata exp pt fiecare etapa
alpha <- runif(n-1, 0.8, 1) # Prob trecerii la urmatoarea etapa
alpha <- c(1, alpha) # Prob ca o persoana sa participe la prima etapa este 100%
nrSimulari <- 1000000 # Nr simulari

simulator <- function() {
  Ti <- rexp(n, rate = lambda)  # Timpul pentru fiecare etapa
  stop_point <- which(runif(n - 1) > alpha[-1])[1]  # Determinam momentul opririi
  if (!is.na(stop_point)) {
    return(sum(Ti[1:stop_point])) 
  } else {
    return(sum(Ti))
  }
}

# Simulam 10^6
valoriT <- replicate(nrSimulari, simulator())

```

### Rezultate și Interpretări

**1. Construiți un algoritm în R care simulează $10^6$ valori pentru v.a. $T$ și în baza acestora aproximați $E(T)$. Reprezentați grafic într-o manieră adecvată valorile obținute pentru $T$. Ce puteți spune despre repartiția lui $T$?**

   ```r
   approx_E_T <- mean(valoriT)
   hist(valoriT, breaks = 100, main = "Distributia timpilor T", xlab = "T")
   ```
   - **Rezultat:** $ \text{approx\_E\_T} = 9.51113170988666$
   - **Interpretare:** Distribuția lui $ T $ urmează o distribuție exponențială.

[Diagrama 1](/doc/img/1.png)

2. **Calculul exact al lui $E[T]$:**
   ```r
   exact_E_T <- sum(1 / lambda * cumprod(alpha))
   ```
   - **Rezultat:** $\text{exact\_E\_T} = 9.51299920302413 $
   - **Comparație:** Valoarea exactă este foarte apropiată de cea obținută prin simulare.

3. **Probabilitatea de finalizare:**
   ```r
   prob_finalizare <- mean(valoriT >= sum(1 / lambda))
   ```
   - **Rezultat:** $\text{prob\_finalizare} = 0.00004 $

4. **Probabilitatea de finalizare într-un timp mai mic sau egal cu $ \sigma $:**
   ```r
   sigma <- 94
   prob_sigma <- mean(valoriT <= sigma & valoriT >= sum(1 / lambda))
   ```
   - **Rezultat:** $\text{prob\_sigma} = 0.000007$

5. **Timpul minim și maxim de finalizare:**
   ```r
   timpMin <- min(valoriT[valoriT >= sum(1 / lambda)])
   timpMax <- max(valoriT[valoriT >= sum(1 / lambda)])
   ```
   - **Rezultat:** $timpMin = 92.0171680316328 $, $timpMax = 108.54443085275 $

6. **Probabilitatea de oprire înainte de etapa $ k $:**
   ```r
   k <- 5
   PStopK <- mean(sapply(valoriT, function(x) {
     if(x <= sum(1/lambda[1:k-1])) return (TRUE)
     else return(FALSE)
   }))
   ```
   - **Rezultat:** $\text{PStopK} = 0.284293 $ pentru $ k = 5 $

---

## Cerința II

**Mesaj pentru Sebi:** 

---

## Cerința III

### Descrierea Problemei

Scopul acestui proiect este de a construi o aplicație web interactivă folosind framework-ul **Shiny** din **R**, pentru a reprezenta grafic funcțiile de repartiție (CDF) ale unor variabile aleatoare definite conform cerinței din enunț. Aplicația permite vizualizarea CDF-urilor pentru distribuții normale, exponențiale, binomiale, Poisson.

### Aspecte Teoretice

- **Distribuția Normală:**  
  $( X \sim N(\mu, \sigma^2) )$  
```math
  F(x) = P(X \leq x) = \int_{-\infty}^{x} \frac{1}{\sqrt{2\pi \sigma^2}} e^{-\frac{(t - \mu)^2}{2\sigma^2}} \, dt
  
```
- **Distribuția Exponențială:**  
  $( X \sim \text{Exp}(\lambda) )$
```math  
  F(x) = 
  \begin{cases}
  1 - e^{-\lambda x}, & \text{pentru } x \geq 0 \\
  0, & \text{pentru } x < 0
  \end{cases}
```

- **Distribuția Poisson:**  
  $( X \sim \text{Poisson}(\lambda) )$  
```math
  F(x) = \sum_{k=0}^{\lfloor x \rfloor} \frac{\lambda^k e^{-\lambda}}{k!}
```

- **Distribuția Binomială:**  
  $( X \sim \text{Binomial}(n, p) )$  
```math
  F(x) = \sum_{k=0}^{\lfloor x \rfloor} \binom{n}{k} p^k (1 - p)^{n - k}
```

### Reprezentări Grafice

![Distribuția Normală](/doc/img/4.png)  
![Distribuția Exponențială](/doc/img/5.png)

### Pachete Software Folosite

- **shiny:** Crearea aplicației interactive.
- **bslib:** Personalizarea interfeței grafice.
- **graphics:** Generarea reprezentărilor grafice.

### Codul Aplicației

Aplicația este structurată modular, cu fișiere separate pentru interfața utilizatorului (`ui.R`), logica serverului (`server.R`), și funcții utilitare (`utils.R`). Fiecare distribuție are propriul fișier pentru generarea graficelor.

#### Exemplu de cod pentru distribuția normală:

```r
create_normal_slider <- function() {
  tagList(
    sliderInput(
      inputId = "normal_n",
      label = "Selecteaza valoarea n:",
      min = 1,
      max = 100,
      value = 10,
      step = 1
    ),
    sliderInput(
      inputId = "normal_mu",
      label = "Media (\u03BC):",
      min = 1,
      max = 100,
      value = 10,
      step = 1
    ),
    sliderInput(
      inputId = "normal_sigma",
      label = "Deviatia standard (\u03C3):",
      min = 1,
      max = 100,
      value = 10,
      step = 1
    ),
    radioButtons(
      inputId = "normal_var",
      label = "Selecteaza variabila aleatoare:",
      choices = list(
        "X ~ N(\u03BC, \u03C3^2)" = "var1",
        "3 - 2X ~ N(\u03BC, \u03C3^2)" = "var2",
        "X^2 ~ N(\u03BC, \u03C3^2)" = "var3",
        "Suma cumulativa" = "var4",
        "Suma patratelor" = "var5"
      ),
      selected = "var1"
    )
  )
}
```

### Dificultăți Întâmpinate

- **Ajustarea automată a parametrilor pentru fiecare distribuție:**  
  Fiecare distribuție are un set unic de parametri care trebuie adaptați corect pentru a asigura o reprezentare grafică precisă.

### Probleme Deschise

- **Extinderea aplicației pentru a suporta și alte distribuții:**  
  Momentan, aplicația suportă distribuțiile normală, exponențială, binomială și Poisson, dar ar putea fi extinsă pentru a include și alte distribuții.

### Concluzii

Aplicația oferă o interfață interactivă pentru vizualizarea funcțiilor de repartiție cumulativă (CDF) ale variabilelor normale, exponențiale, binomiale și Poisson. Structura modulară permite integrarea rapidă a altor distribuții și transformări ale variabilelor aleatoare.

---

**Notă:** Imaginile și fișierele menționate în document (de exemplu, `./img/1.png`) trebuie să fie disponibile în același director cu fișierul Markdown pentru a fi afișate corect.
```

Acest fișier Markdown păstrează structura și conținutul documentului LaTeX original, adaptându-le pentru formatul Markdown. Dacă ai nevoie de alte ajustări, nu ezita să întrebi! 😊