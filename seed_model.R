## Seed model

library(ggpubr)

age = seq(0, 500)

meanp = 180
sdp = 50

seedp = dnorm(age, meanp, sdp)

p1 = data.frame(age, seedp) %>%
  ggline(x = "age", y  = "seedp")

seedp = dlnorm(age, log(meanp), log(sdp))

p2 = data.frame(age, seedp = exp(seedp)) %>%
  ggline(x = "age", y  = "seedp")

ggarrange(p1, p2)