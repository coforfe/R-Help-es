

library(babynames)
library(dplyr)
library(data.table)
library(tictoc)
library(microbenchmark)

midt <- setDT(copy(babynames))
midf <- as.data.table(midt)

#--- Ejemplo de condiciones que se buscan
# Elizabeth == 8915
# Michael == 88514
# David == 86251

# Selecciono nombres y su frecuencia (primera aparición) aleatoriamente
NUM_NOMBRES <- 1000
all_nam <- unique(midt[ , .(name)])
rnd_idx <- sample(1:nrow(all_nam), NUM_NOMBRES, replace = FALSE)
mi_names <- all_nam[ rnd_idx, .(name)]
mi_value <- midt[ name %chin% mi_names$name, .(n), by = name] %>% 
                 .[ , .SD[1], by = name]

#--- Crear condiciones automáticamente
#-- cond_end == Condiciones para data.table y dplyr
#-- cond_bas == Condiciones para base
NUM_CONDICIONES <- 50
cond_end <- vector() 
cond_bas <- vector()
for (i in 1:NUM_CONDICIONES) {
  name_val <- mi_value$name[i]
  n_val    <- mi_value$n[i]
  if (i < num_cond) {
      mi_cond  <- paste("( name == '" , name_val, "' & n == ", n_val, " ) | ", sep = "")
      mi_cond2 <- paste("( midf$name == '" , name_val, "' & midf$n == ", n_val, " ) | ", sep = "")
  } else {
      mi_cond  <- paste(" ( name == '" , name_val, "' & n == ", n_val, " )", sep = "")
      mi_cond2 <- paste(" ( midf$name == '" , name_val, "' & midf$n == ", n_val, " )", sep = "")
  }
  cond_end <- paste0(cond_end, mi_cond, collapse = "\n")
  cond_bas <- paste0(cond_bas, mi_cond2, collapse = "\n")
}

#------- COMPARACIONES ---------------
#-- data.table
tic()
midt[ eval(parse(text = cond_end )), ]
toc()

#-- dplyr
midf <- as.data.frame(midt)
tic()
midf %>%
  filter( eval(parse(text = cond_end )), )
toc()

#--- base
tic()
midf[ eval(parse(text = cond_bas )), ]
toc()


#------- BENCHMARKINGS --------------
microbenchmark(
  data.table = midt[ eval(parse(text = cond_end )), ] ,
  base       = midf[ eval(parse(text = cond_bas )), ] ,
  dplyr      = midf %>% filter( eval(parse(text = cond_end )), ) ,
  times = 25 
)
