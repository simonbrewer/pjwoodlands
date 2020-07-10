library(ggpubr)

###############################################################################
## Model from Camac et al (PNAS, 2018)

mortality <- function(t, dt, alpha, gamma, beta, biomass) {
  gamma + alpha * exp(-beta * biomass) * dt
}

myt = seq(0,500)
dt = 1
myb = curve(exp(-x* 0.01), 0, 500, n = 501) 
myb$y = myb$y * 5

## gamma gives base rate (non-growth)
## alpha gives mortality at very low growth rates
## beta is the sensitivity to growth rate

mym = mortality(myt, dt, alpha = 0.004, gamma = 0.001, beta = 0.6, biomass = myb$y)

data.frame(t = myt, db = myb$y, m = mym) %>%
  ggline(x = "db", y = "m", numeric.x.axis = TRUE)

