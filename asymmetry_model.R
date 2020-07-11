library(ggpubr)

###############################################################################

b = 0.15

biomassA = 1
biomassB = seq(1,1000)
nB = length(biomassB)

propA = rep(NA, nB)

for (i in 1:nB) {
  propA[i] = biomassA^b / (biomassA^b + biomassB[i]^b)
}

data.frame(A = propA, biomassB) %>%
  ggline(x = "A", y = "biomassB", 
         numeric.x.axis = TRUE)

