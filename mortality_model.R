library(ggpubr)

###############################################################################
## Model from Camac et al (PNAS, 2018)

mortality <- function(t, dt, alpha, gamma, beta, biomass) {
  gamma + alpha * exp(-beta * biomass) * dt
}

myt = seq(0,500)
dt = 1
myb = curve(exp(-x* 0.05), 0, 500, n = 501) 
myb$y = myb$y * 10


## gamma gives base rate (non-growth)
## alpha gives mortality at very low growth rates
## beta is the sensitivity to growth rate

mym = mortality(myt, dt, alpha = 0.04, gamma = 0.01, beta = 0.5, biomass = myb$y)

data.frame(t = myt, b = myb$y, m = mym) %>%
  ggline(x = "b", y = "m", numeric.x.axis = TRUE)

