
library(dplyr)
library(magrittr)
library(tictoc)

tic()

alumnos_curso <- 100
cursos_val    <- 4
colegios_val  <- 10 


datos_1 <- data.frame(
  colegio = 1,
  curso   = rep(paste("curso_", letters[1:cursos_val], sep = ""), each = alumnos_curso),
  alumno  = rep( 1:alumnos_curso, cursos_val)
)

# datos_2  <- datos_1 %>% mutate(colegio = 2)
# datos_3  <- datos_1 %>% mutate(colegio = 3)
# datos_4  <- datos_1 %>% mutate(colegio = 4)
# datos_5  <- datos_1 %>% mutate(colegio = 5)
# datos_6  <- datos_1 %>% mutate(colegio = 6)
# datos_7  <- datos_1 %>% mutate(colegio = 7)
# datos_8  <- datos_1 %>% mutate(colegio = 8)
# datos_9  <- datos_1 %>% mutate(colegio = 9)
# datos_10 <- datos_1 %>% mutate(colegio = 10)

datos <- datos_1
for ( i in 2: 10) {
  dat_tmp <- datos_1 %>% mutate(colegio = i)
  datos   <- rbind(datos, dat_tmp)
}

# #--- Datos de todos los colegios
# datos <- rbind(datos_1, datos_2, datos_3, datos_4, datos_5, 
#                datos_6, datos_7, datos_8, datos_9, datos_10)

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

toc()

