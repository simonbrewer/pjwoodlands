library(ggpubr)

###############################################################################
## Exponential (Euler)

exp.euler <- function(t, X0, k, dt) {
  nt = length(t)
  ns = floor((t[2] - t[1])/dt)
  X = rep(NA, nt)
  
  Xt = X0
  tt = t[1]
  
  for (i in 1:nt) {
    X[i] <- Xt
    for (j in 1:ns) {
      tt = tt + dt
      Xt = Xt + dt * k * Xt
    }
  }
  return(data.frame(t,X))
}

t = seq(0,100)
r = 0.25
k = 0.1
X0 = 2
dt = 0.1

plot.df = data.frame(exp.euler(t, X0, k, dt))
ggscatter(plot.df, x = "t", y = "X")

###############################################################################

###############################################################################
## Logistic growth (Euler)
log.euler <- function(t, X0, r, K, dt) {
  nt = length(t)
  ns = floor((t[2] - t[1])/dt)
  X = rep(NA, nt)
  
  Xt = X0
  tt = t[1]
  
  for (i in 1:nt) {
    X[i] <- Xt
    for (j in 1:ns) {
      tt = tt + dt
      Xt = Xt + dt * r * (1 - Xt/K) * Xt
    }
  }
  return(data.frame(t,X))
}

## Example of growth curve
suitability = 0.1
t = seq(0,200)
r = 0.10 * suitability
K = 10 * suitability
X0 = 0.1
dt = 0.1

plot.df = data.frame(log.euler(t=t, X0, r, K, dt))
ggscatter(plot.df, x = "t", y = "X")
###############################################################################

## Example of mortality curve
t = seq(0,1000)
r = 0.015
K = 0.1
X0 = 0.001
dt = 0.1

plot.df = data.frame(log.euler(t=t, X0, r, K, dt))
ggscatter(plot.df, x = "t", y = "X")

## Or as solved equation
m = K / (1 + exp(-r * (t-500)))
plot(t, m)
###############################################################################

## Example of mortality curve
t = seq(0,1000)
r = 0.015
K = 0.1
X0 = 0.001
dt = 0.1

plot.df = data.frame(log.euler(t=t, X0, r, K, dt))
ggscatter(plot.df, x = "t", y = "X")

## Or as solved equation
m = K / (1 + exp(-r * (t-500)))
plot(t, m)

## as solved exponential
## Or as solved equation
k = 0.01
m = exp(k * t)
plot(t, 1 - m)

###############################################################################
