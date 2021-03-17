#----------------------------------
library(dplyr)
library(tidyr)
library(data.table)

datin <- fread('base_enfermedades_dummy.csv')

#Demencia, Cáncer, Enfermedad Cardíaca, Enfermedad pulmonar y Diabetes

to_keep <- c('paciente', 'Demencia', 'Cáncer', 'Enfermedad Cardíaca',
             'Enfermedad Pulmonar' , 'Diabetes')
to_rest <- setdiff(names(datin), to_keep)

datin_rel <- datin %>% relocate(all_of(to_keep), .before = all_of(to_rest))
datinnew <- datin_rel
datrest <- datin_rel[, (length(to_keep)+1):ncol(datin_rel)]

# Conseguir columna "Otros"
datinnew$sum_keep <- rowSums(datin_rel[, 2:length(to_keep)])
datinnew$sum_rest <- rowSums(datin_rel[, (length(to_keep)+1):ncol(datin_rel)])
datinnew$Otros <- ifelse(datinnew$sum_rest > 0, 1, 0)
                                   
#--- Conseguir columna "Enfermedades_otras"
datinnew$Enfermedades_otras <- apply( datrest, 1,  
                                      function(u) paste( names(which(u > 0)), collapse = "," ) )

#----------------------------------
