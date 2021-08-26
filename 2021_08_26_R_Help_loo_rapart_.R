

#-------- Needed Libraries
library(data.table)
library(tidytable)
library(rpart)
library(magrittr)
library(dplyr)

#-------- Read data
data <- fread("Ecoli.csv", drop = c('V1'))

#-------- Add new column with all columns pasted together.
#---- It is needed to separate between numerical and character columns. 
data %<>%
  mutate_rowwise.( concatmp = paste0(c_across.(mcg:alm2), collapse = "_")) %>%
  mutate_rowwise.( concacol = paste(concatmp, loc, sep = "_")) %>%
  select.(-concatmp)

#-------- Manuel's code
preds <- c(0)  # creamos un vector vacío

for (i in 1:nrow(data)) {
  #--- Select row from concatenation
  tmp_row <- data$concacol[i]
  #--- Detect which are equal to the selected to remove for training
  
  #--- Select row OOB and remaining as training
  row_tmp  <- data[i,]
  #--- Remove the rows that are equal to tmp_row. Comparison using concacol col.
  training <- data[ !(concacol %like% tmp_row), ]  %>%
    select.(-concacol)
  
  #--- Model
  fitrp    <- rpart(loc ~ ., data = training)
  
  #--- Predict
  Pred     <- as.vector(predict(fitrp, row_tmp, type="class"))
  preds[i] <- Pred
  print(c(i, nrow(data), Pred, nrow(training)))
}

data$preds<- preds











#----------------------------------------------------


#-------- Needed Libraries
library(data.table)
library(rpart)

#-------- Read data
data <- fread("Ecoli.csv", drop = c('V1'))

#-------- Manuel's code
preds <- c(0)  # creamos un vector vacío

for (i in 1:nrow(data)) {
  #--- Select row OOB and remaining as training
  row_tmp  <- data[i,]
  training <- data[-i, ] 
  #--- Model
  fitrp    <- rpart(loc ~ ., data = training)
  #--- Predict
  Pred     <- as.vector(predict(fitrp, row_tmp, type="class"))
  preds[i] <- Pred
  print(c(i, nrow(data), Pred))
}

data$preds<- preds


