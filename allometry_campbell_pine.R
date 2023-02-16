set.seed(42)
library(ggpubr)

## Example using correlated bivariate
log_wc_mean = -1.939
log_wc_sd = 0.562

## Parameters derived from NLME equations
diam_asym_mean = 0.336
diam_asym_sd = 0.303
diam_asym_wc = 0.0067

diam_lrc_mean = -3.81
diam_lrc_sd = 1.126
diam_lrc_wc = -0.088

## Parameter correlations (asym vs. lrc)
diam_corr = -0.513

## 1. Generate z1 and z2
z1 = rnorm(1, 0, 1)
z2 = rnorm(1, 0, 1)

## 2. Convert 
z1 = z1
z2 = diam_corr * z1 + sqrt(1 - diam_corr^2) * z2
#plot(z1, z2)

## 3. Convert to real values
asym = z1 * diam_asym_sd + diam_asym_mean
lrc = z1 * diam_lrc_sd + diam_lrc_mean
print(paste(asym, lrc))

## 4. make curve to check
ages <- seq(0, 150)
diam <- asym * ( 1 - exp(- exp( lrc ) * ages ))

## 5. Chojnacky eq (dbh - drc)
stems <- 1
sstems <- ifelse(stems > 1, 1, 0)

pB0 <- -4.9209
pB1 <- 0.9823
pB2 <- 1.8879

#drc <- (pB0 + pB1 * (diam*100) + pB2 * sstems) / 100
drc <- (diam*100 - pB0 - pB2 * sstems) / (pB1 * 100)

## 6. Grier DRC -> Cwood
pB0 <- -2.588
pB1 <- 2.955
lcwood <- pB0 + pB1 * log10(drc * 100)
cwood <- 10^lcwood

par(mfrow = c(1,2))
plot(ages, diam, type = 'l', ylim = c(0, max(c(diam, drc))))
lines(ages, drc, col = 2)
plot(ages, cwood, type = 'l')
