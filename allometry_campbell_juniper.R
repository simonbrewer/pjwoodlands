set.seed(42)
library(ggpubr)

## Example using correlated bivariate
log_wc_mean = -1.939
log_wc_sd = 0.562

## Parameters derived from NLME equations
diam_asym_mean = 0.318
diam_asym_sd = 0.119
diam_asym_wc = -0.0845

diam_lrc_mean = -1.872
diam_lrc_sd = 0.633
diam_lrc_wc = 1.851

## Parameter correlations (asym vs. lrc)
diam_corr = -0.777

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
stems <- 2
sstems <- ifelse(stems > 1, 0, 1)

pB0 <- -6.818
pB1 <- 1.0222
pB2 <- 1.8879

#drc <- (pB0 + pB1 * (diam*100) + pB2 * sstems) / 100
drc <- (diam*100 - pB0 - pB2 * sstems) / (pB1 * 100)
multistem <- function(x, n) {
  return(sqrt(sum(rep(x,n)^2)))
}
drc <- sapply(drc, multistem, stems)
## 6. Grier DRC -> Cwood
pB0 <- -2.297
pB1 <- 2.431
lcwood <- pB0 + pB1 * log10(drc * 100)
cwood <- 10^lcwood

par(mfrow = c(1,2))
plot(ages, diam, type = 'l', ylim = c(0, max(c(diam, drc))))
lines(ages, drc, col = 2)
plot(ages, cwood)

