library(ggpubr)

## Pinyon

## McIntire et al 2022
hgt <- seq(0, 8, length.out = 200)

agb <- 0.971 * hgt^2.563

# plot_df <- data.frame(hgt, agb)
# ggline(plot_df, x = "hgt", y = "agb", 
#        plot_type = "l",
#        numeric.x.axis = TRUE)


## Example using correlated bivariate
log_wc_mean = -1.939
log_wc_sd = 0.562

## Parameters derived from NLME equations
diam_asym_mean = 0.145
diam_asym_sd = 0.078
diam_asym_wc = 0.029

diam_lrc_mean = -3.57
diam_lrc_sd = 0.976
diam_lrc_wc = -0.215

## Parameter correlations (asym vs. lrc)
diam_corr = -0.886

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
plot(ages, diam)