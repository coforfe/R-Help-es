

library(ggplot2)
library(dplyr)

df <- data.frame( x = rnorm(100), y = rnorm(100))

#------------------
grafico_dispersion<-function(df,x,y){
  x<-as.symbol(x)
  y<-as.symbol(y)
  
  p<-ggplot(df,aes(!!x,!!y)) + 
    geom_point(alpha=0.1) +
    geom_smooth(method = "loess") + 
    theme_bw() 
  
  return(p)  
}

grafico_dispersion(df, "x", "y")

#------------------

df1 <- data.frame( x = abs(rnorm(75)), y = abs(rnorm(75)) )
df2 <- data.frame( x = abs(rnorm(25)*100), y = abs(rnorm(25)*100))
df <- rbind(df1, df2)
df <- df[ sample(1:nrow(df), nrow(df)), ]



grafico_dispersion<-function(df,x,y,p){
  x<-as.symbol(x)
  y<-as.symbol(y)
  
  
  gr <- ggplot(df,aes(!!x,!!y)) +
    geom_point(alpha = 0.75, color = "red") + 
    geom_smooth(method = "loess") + 
    coord_cartesian(xlim = c(0,quantile(df$x,p)),ylim = c(0,quantile(df$y,p))) +
    theme_bw() 
  
  # return(gr)  
  print(gr)
}

grafico_dispersion(df, "x", "y", 0.15)


p <- 0.80
ggplot(df,aes(x,y)) +
  geom_point(alpha=0.1) + 
  geom_smooth(method = "loess") + 
  theme_bw() +
  coord_cartesian(xlim = c(0,quantile(df$x,p)),ylim = c(0,quantile(df$y,p)))



#------------------


grafico_dispersion<-function(df,x,y){
  x<-as.symbol(x)
  y<-as.symbol(y)
  
  p<-ggplot(df,aes(!!x,!!y))+ geom_point(alpha=0.1)+geom_smooth(method = "loess")+ theme_bw() 
  
  return(p)  
}

#----------------------


#--  Data with outliers
df1 <- data.frame( x = abs(rnorm(75)), y = abs(rnorm(75)) )
df2 <- data.frame( x = abs(rnorm(25)*100), y = abs(rnorm(25)*100))
df  <- rbind(df1, df2)
df  <- df[ sample(1:nrow(df), nrow(df)), ]


#-- Function with filter
grafico_dispersion<-function(df,x,y,p){
  x<-as.symbol(x)
  y<-as.symbol(y)
  
  dfnew <- df %>%
    filter(!!x <= quantile(df$x,p) & !!y <= quantile(df$x,p))
  
  gr <- ggplot( dfnew ,aes(!!x,!!y) ) + 
    geom_point(alpha = 0.25) +
    geom_smooth(method = "loess") + 
    theme_bw() 
    # coord_cartesian(xlim = c(0,quantile(select(df,!!x),p)),ylim = c(0,quantile(select(df,!!y),p)))
  
  return(gr)
  
}

#-- Several examples
grafico_dispersion(df, "x", "y", 0.25)
grafico_dispersion(df, "x", "y", 0.50)
grafico_dispersion(df, "x", "y", 0.75)
grafico_dispersion(df, "x", "y", 0.95)

