
library(data.table)
library(kit)

data_size <- 1000000

#--- Small Vector
data_size <- 10000
test_vec <- sample(paste0(letters, LETTERS), data_size, TRUE)

bench::mark(
  kit_ifelse = iif(test_vec == "aA", 1, 0),
  dt_ifelse = fifelse(test_vec == "aA", 1, 0),
  kit_case = nif(test_vec == "aA", 1, test_vec == "bB", 1, default = 0),
  kit_casetwo = vswitch(test_vec, values = c('aA', 'bB'), output = c(1,1), default = 0),
  dt_case = fcase(test_vec == "aA", 1, test_vec == "bB", 1, default = 0),
  check = FALSE, iterations = 100
)

#-------------------

#--- Big Vector
data_size <- 1000000
test_vec <- sample(paste0(letters, LETTERS), data_size, TRUE)

bench::mark(
  kit_ifelse = iif(test_vec == "aA", 1, 0),
  dt_ifelse = fifelse(test_vec == "aA", 1, 0),
  kit_case = nif(test_vec == "aA", 1, test_vec == "bB", 1, default = 0),
  kit_casetwo = vswitch(test_vec, values = c('aA', 'bB'), output = c(1,1), default = 0),
  dt_case = fcase(test_vec == "aA", 1, test_vec == "bB", 1, default = 0),
  check = FALSE, iterations = 100
)

#--- Big++ Vector
data_size <- 100000000
test_vec <- sample(paste0(letters, LETTERS), data_size, TRUE)

bench::mark(
  kit_ifelse = iif(test_vec == "aA", 1, 0),
  dt_ifelse = fifelse(test_vec == "aA", 1, 0),
  kit_case = nif(test_vec == "aA", 1, test_vec == "bB", 1, default = 0),
  kit_casetwo = vswitch(test_vec, values = c('aA', 'bB'), output = c(1,1), default = 0),
  dt_case = fcase(test_vec == "aA", 1, test_vec == "bB", 1, default = 0),
  check = FALSE, iterations = 100
)
