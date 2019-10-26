#----------------
library(randomNames)
library(dplyr)
library(tidyr)

#---- Genero nombres aleatorios de "pacientes".
## - Estos son algunos de los que coincidirian
nom_coin <- randomNames(50, ethnicity = 4)

## - A los que coincidirian les anado otros puramente aleatorios
mis_datos <- data.frame(
  prog_A = sample(c(nom_coin, randomNames(70, ethnicity = 1)), 100, replace = FALSE),
  prog_B = sample(c(nom_coin, randomNames(70, ethnicity = 1)), 100, replace = FALSE),
  prog_C = sample(c(nom_coin, randomNames(70, ethnicity = 1)), 100, replace = FALSE),
  prog_D = sample(c(nom_coin, randomNames(70, ethnicity = 1)), 100, replace = FALSE)
)

# - Construyo las combinaciones dos a dos de las columnas
res_cols <- as.data.frame(t(combn(names(mis_datos), 2)))
res_comb <- as.data.frame(t(combn(1:ncol(mis_datos), 2)))
res_tot <- cbind(res_cols, res_comb)

# - Comparo las parejas de columnas
for(i in 1:nrow(res_tot)) {
  val_inter <- intersect(mis_datos[, res_comb[i,1]], mis_datos[, res_comb[i,2]])
  res_tot[i, 5] <- length(val_inter)
}
res_out <- res_tot[, c(1,2,5)]
names(res_out)[3] <- c('Qty')

# - Construyo la tabla final
res_table <- res_out %>%
  pivot_wider(names_from = V2, values_from = Qty )
res_table

#----------------