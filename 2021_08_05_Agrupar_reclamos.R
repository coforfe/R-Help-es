
library(dplyr)
library(data.table)

datin <- fread('suaci_reclamos.csv')

datin %>%
  group_by(anio) %>%
  mutate(tot_year = sum(n_reclamos)) %>%
  group_by(anio, TIPO_PRESTACION) %>%
  mutate(tot_pres = sum(n_reclamos) ) %>%
  mutate(por_pres_year = (tot_pres/tot_year)*100) %>%
  select(-tot_year, -tot_pres) %>%
  arrange(anio, TIPO_PRESTACION)
