

#----- data.table
Lines <- "Monodroga 	UNIDADES 	Precio 	PrecioUnit
aciclovir 	20 	111272 	55.636
aciclovir 	20 	97464 	48.732
aciclovir 	40 	98322 	432
aciclovir 	40 	98322 	324
paracetamol 	1 	19291 	192.91
paracetamol 	1 	24702 	247.02
paracetamol 	1 	21120 	211.2
paracetamol 	10 	9993 	9.993
paracetamol 	10 	10443 	10.443
rosuvastatina 	14 	141134 	100.81
rosuvastatina 	28 	258262 	92.2364286
rosuvastatina 	28 	201590 	71.9964286
rosuvastatina 	30 	183717 	61.239
rosuvastatina 	30 	231935 	77.3116667"

datos <- read.table(textConnection(Lines), as.is = TRUE, header = TRUE)

# droga "aciclovir" necesito solo las filas donde Unidades==20,  en paracetamol==10 y en rosuvastatina==30.

#----- data.table
library(data.table)
datos <- as.data.table(datos)
datos[ 
       (Monodroga == "aciclovir"     & UNIDADES == 20) |
       (Monodroga == "paracetamol"   & UNIDADES == 10) |
       (Monodroga == "rosuvastatina" & UNIDADES == 30) 
      ,
       ][ order(Monodroga, - Precio)]

#----- dplyr
library(dplyr)
datos %>%
  filter(
          (Monodroga == "aciclovir"     & UNIDADES == 20) |
          (Monodroga == "paracetamol"   & UNIDADES == 10) |
          (Monodroga == "rosuvastatina" & UNIDADES == 30)
  ) %>%
  arrange(Monodroga, -Precio)

#----- base
midf <- as.data.frame(datos)
midf[ 
        (midf$Monodroga == "aciclovir"     & midf$UNIDADES == 20) |
        (midf$Monodroga == "paracetamol"   & midf$UNIDADES == 10) |
        (midf$Monodroga == "rosuvastatina" & midf$UNIDADES == 30), 
      ]

#---- sqldf
library(sqldf)
sqldf(
       "select * from midf where 
      (monodroga = 'aciclovir'     AND UNIDADES = 20) OR 
      (monodroga = 'paracetamol'   AND UNIDADES = 10) OR 
      (monodroga = 'rosuvastatina' AND UNIDADES = 30)"
                  )

#----- Benchmark
library(microbenchmark)
microbenchmark(
  datatable = datos[ 
      (Monodroga == "aciclovir"     & UNIDADES == 20) |
      (Monodroga == "paracetamol"   & UNIDADES == 10) |
      (Monodroga == "rosuvastatina" & UNIDADES == 30) 
    ,
  ]
  ,
  dplyr = datos %>%
    filter(
        (Monodroga == "aciclovir"     & UNIDADES == 20) |
        (Monodroga == "paracetamol"   & UNIDADES == 10) |
        (Monodroga == "rosuvastatina" & UNIDADES == 30)
    )
  ,
  base = midf[ 
      (midf$Monodroga == "aciclovir"     & midf$UNIDADES == 20) |
      (midf$Monodroga == "paracetamol"   & midf$UNIDADES == 10) |
      (midf$Monodroga == "rosuvastatina" & midf$UNIDADES == 30), 
  ]
  ,
  sqldf = sqldf(
      "select * from midf where 
      (monodroga = 'aciclovir'     AND UNIDADES = 20) OR 
      (monodroga = 'paracetamol'   AND UNIDADES = 10) OR 
      (monodroga = 'rosuvastatina' AND UNIDADES = 30)"
  )
  
  ,times = 1000
)
