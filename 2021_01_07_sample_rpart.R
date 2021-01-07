library(rpart)

data(car90)
target <- c('Mileage')
vars   <- setdiff(names(car90), target)

num_loops <- 10
for( i in 1:num_loops) {
   num_vars <- 6
   vars_samp <- vars[ sample(1:length(vars), num_vars)]    
   fmla <- as.formula(paste(target, " ~ ", paste(vars_samp, collapse= "+")))
   fit <- rpart(fmla, data = car90)
   print(fit)
}

