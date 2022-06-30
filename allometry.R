library(ggpubr)

## Pinyon

## McIntire et al 2022
hgt <- seq(0, 8, length.out = 200)

agb <- 0.971 * hgt^2.563

plot_df <- data.frame(hgt, agb)
ggline(plot_df, x = "hgt", y = "agb", 
       plot_type = "l",
       numeric.x.axis = TRUE)
