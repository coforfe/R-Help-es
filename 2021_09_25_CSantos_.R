
#---- Library loading
suppressPackageStartupMessages({
  library(dplyr)
  library(magrittr)
  library(data.table)
  library(tidytable)
  library(stringi)
})

#----- Data Loading
Lines <- "V0 	V1 	V2 	V3 	V4 	V5 	V6 	V7 	V8 	V9 	V10
1 	63 	1 	1 	145 	A 	233 	1 	2 	3 	0
2 	67 	1 	4 	160 	A 	286 	0 	2 	2 	3
3 	67 	2 	4 	120 	B 	229 	0 	2 	2 	2
4 	37 	1 	3 	130 	C 	250 	0 	0 	3 	0
5 	41 	1 	2 	130 	C 	204 	0 	2 	1 	0
6 	56 	2 	2 	120 	A 	236 	0 	0 	1 	0
7 	62 	1 	4 	140 	B 	268 	0 	2 	3 	2
8 	57 	2 	4 	120 	C 	354 	0 	0 	1 	0
9 	63 	2 	4 	130 	A 	254 	0 	2 	2 	1
10 	53 	1 	4 	140 	B 	203 	1 	2 	3 	0
11 	57 	2 	4 	140 	B 	192 	0 	0 	2 	0
12 	56 	1 	2 	140 	A 	294 	0 	2 	2 	0
13 	56 	2 	3 	130 	C 	256 	1 	2 	2 	1
14 	44 	2 	2 	120 	B 	263 	0 	0 	1 	0
15 	52 	2 	3 	172 	B 	199 	1 	0 	1 	0"

#----- Get initial data.frame
midt <- as.data.table(read.table(textConnection(Lines),  header = TRUE, as.is = TRUE))

#----- Get all pairs combinations
pairs_val <- unique(midt$V5)
comb_tmp <- as.data.frame(combn(pairs_val, 2))

#---- References
ref_val <- c(0.34, 0.66)

#----- Function for distances
fun_dis <- function(x) {
  dis_tmp <- sqrt((x[1] - ref_val[1])^2 + (x[2] - ref_val[2])^2)
  return(dis_tmp)
}


#----- Function to calculate percentages by V2.
fun_compa <- function(comb_tmp, dt_one) {
   for (i in 1:ncol(comb_tmp)) {
     val_one <- comb_tmp[1,i]
     val_two <- comb_tmp[2,i]
     
     dt_one <- midt[ V5 == val_one | V5 == val_two,  ] 
     res_val <- dt_one[ , .(res_sum = sum(V1,V3,V4,V6,V7,V9,V9,V10)) , by = V2] 
     res_val %<>%
       mutate.(res_tot = sum(res_sum)) %>%
       mutate.(res_per = res_sum/res_tot) %>%
       select.(res_per)
     dt_comp <- cbind(dt_comp, res_val)
   }
  return(dt_comp)
}

#------ Process
dt_comp <- data.table()
for (i in 1:ncol(comb_tmp)) {
  res_tmp <- fun_compa(comb_tmp[,1:2], dt_one)
  to_sust <- comb_tmp[, which.max(apply(res_tmp, 2, fun_dis))]
  midt[ , V5 := stri_replace_all_fixed(V5, to_sust[1], to_sust[2])]  
}

midt
