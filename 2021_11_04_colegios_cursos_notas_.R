
library(dplyr)
library(magrittr)
library(tictoc)

tic()

rm(list = ls())
gc()

alumnos_curso <- 100
cursos_val    <- 4

datos_1 <- data.frame(
  colegio = 1,
  curso   = rep(paste("curso_", letters[1:cursos_val], sep = ""), each = alumnos_curso),
  alumno  = rep( 1:alumnos_curso, cursos_val)
)

num_colegios <- 500
datos <- datos_1
for (i in 2:num_colegios) {
  dat_tmp <- datos_1 %>% mutate(colegio = i)
  datos   <- rbind(datos, dat_tmp)
}

#--- Añado las notas de forma aleatoria
datos %<>% mutate(notas = sample(1:10, nrow(datos), replace = TRUE) ) 

#--- Calculo el percentil de las notas de cada colegio/curso
res_final <- datos %>% 
  group_by(colegio, curso) %>%
  mutate( percen = ntile(notas, 100) ) 

#---- Para ordenar el percentil
rev_perc = data.frame(
   per_rev = 1:100,
   rev_per = 100:1
)


#---- Resultado con percentil ordenado.
res_out <- left_join(res_final, rev_perc, by = c("percen" = "per_rev") ) %>%
  select(-percen) %>%
  rename(percentile = rev_per)

res_out
toc()

