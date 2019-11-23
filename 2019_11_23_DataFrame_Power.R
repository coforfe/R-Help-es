

my_pol <- function( n = 100, grad_ini = 1, grad_end = 5) {
  val_rnd <- rnorm(n)
  df <- data.frame(c(0))
  for( i in grad_ini:grad_end) {
       df_tmp <- as.data.frame(val_rnd^i)
       df <- cbind(df, df_tmp)
  }
  df <- df[ , 2:ncol(df)]
  names(df) <- paste("V_", grad_ini:grad_end, sep = "")
  return(df)
}

df_out <- my_pol(10, 3, 8)
head(df_out)

