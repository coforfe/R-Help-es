
library(data.table)
library(ggplot2)

datIn <- data.table(
  Individuo = c(1L,2L,3L,4L,5L,6L,7L,8L,9L,10L,
                11L,12L,13L,14L,15L,16L,17L,18L,19L,20L,21L,22L,23L,
                24L,25L,26L,27L,28L,29L,30L),
  Observada = c(200.21,154.817,514.919,234.068,232.191,
                379.53,297.466,450.94,317.84,308.16,178.317,530.919,
                368.224,251.352,154.866,309.39,291.637,513.398,322.25,
                451.932,612.163,574.745,92.629,451.836,449.408,689.179,
                885.454,484.617,876.734,506.156),
  Estimada = c(180.75719,110.00147,455.28532,226.17628,
               218.58544,346.95982,309.05514,421.53012,276.81604,
               305.13638,182.79552,492.87962,347.28844,255.40259,143.53278,
               307.57602,288.5641,454.39712,323.53048,442.87195,555.30366,
               518.25317,56.79211,413.1442,445.26074,741.37221,735.90541,
               450.47909,742.06702,521.66028)
)

datIn_lg <- melt(datIn, id=1)
names(datIn_lg) <- c('Individuo', 'Tipo', 'Valor')

library(ggpmisc)
formula <- y ~ poly(x, 4, raw = TRUE)
ggplot(datIn_lg, aes( Individuo, Valor, group = Tipo, color = Tipo)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label =  paste(stat(eq.label), stat(adj.rr.label), 
                                  sep = "*\", \"*")),
               formula = formula, parse = TRUE) +
  labs(x = expression(italic(x)), y = expression(italic(y))) +
  facet_wrap(~Tipo) +
  theme_bw()
