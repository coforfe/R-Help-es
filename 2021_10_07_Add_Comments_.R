#----------------

library(dplyr)
library(tidyr)
library(readr)
library(readxl)

datin <- read_excel("Ejes formativos.xlsx")

datin_long <- datin %>%
  pivot_longer(
    cols = starts_with("eje"),
    values_to = "ejes"
  ) %>%
  rename( area = area_pertenencia) %>%
  select(-name) %>%
  relocate(ejes, .before = area)


#----------------

library(data.table)
library(tidytable)

datin <- fread("Ejes_formativos.csv", encoding = 'Latin-1')

datin_long <- melt(datin, id.vars = c('ID', 'area_pertenencia'), value.name = 'ejes') %>%
  select.(-variable) %>%
  rename.( area = area_pertenencia) %>%
  arrange.(ID)
