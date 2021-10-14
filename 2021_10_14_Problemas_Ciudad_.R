
library(data.table)
library(tidytable)
library(dplyr)
library(magrittr)
library(ggplot2)
library(forcats)

datos <- fread("1_ProblemasCiudad.txt", header = FALSE)

# dat_freq <- datos %>%
datos_freq <- datos %>%
 summarise.( freq = n(), .by = V1) %>%
  arrange.(-freq) %>%
  #--- Calculo frecuencias sobre el total
  mutate.(per_total = round(100*freq/sum(freq),2)) %>%
  #--- Me quedo con las 10 filas (ya están ordenadas de mayora a menor)
  top_n.(n = 10) %>%
  rename.(Problema = V1)

ggplot(datos_freq, aes(fct_reorder(Problema, per_total), per_total)) +
  geom_col(group = 1, fill = "darkred") +
  coord_flip() +
  labs(
        title = "PROBLEMAS - TOP-10",
        subtitle = "(porcentajes sobre el total)",
        x = "Tipo Problema",
        y = "Procentaje sobre el total de problemas (%)"
  ) +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0, size = 10, face = "bold", color = "blue")) 

