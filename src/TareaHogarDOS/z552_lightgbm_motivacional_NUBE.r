# LightGBM  Motivacional
# para motivar a los alumnos a hacer la  "Tarea Hogar DOS"
# viendo que desde el inicio de la tarea logran ganancias superadoras
# la salida queda en  "./labo/exp/KA552/KA_552_001.csv"

#los DOS puntos novedosos que se ven en este script
# 1. Se entrena  con  POS = { BAJA+1, BAJA+2 }    los BAJA+1 en realidad estan mas enfermos que los BAJA+2
#    Era forzar mucho al algoritmo agrupar los BAJA¿1 con los CONTINUA 
# 2. El punto anterior obliga a buscar una probabilidad de corte DISTINTA  a 1/60

# utilizar la primer semilla propia

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")


ksemilla  <- 200007  #poner aqui la PRIMERA de sus cinco semillas

#Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/")   #Establezco el Working Directory

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasets/data_feature_engineering.csv", stringsAsFactors= TRUE)


#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )


#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset$clase01 )


#genero el modelo con los parametros por default
#estos hiperparametros  salieron de una Optmizacion Bayesiana
modelo  <- lgb.train( data= dtrain,
                      param= list( objective=        "binary",
                                   max_bin=              31,
                                   learning_rate=         0.010503267734194,
                                   num_iterations=      681,
                                   num_leaves=          389,
                                   min_data_in_leaf=   4224,
                                   feature_fraction=      0.541854366118478,
                                   seed=               ksemilla   #aqui se utiliza SU primer semilla
                      )
)

#aplico el modelo a los datos sin clase
dapply  <- fread("./datasets/data_feature_engineering_TEST.csv")

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )


#Genero la entrega para Kaggle
#Atencion ya NO corto por  1/60,  sino que busque el punto de corte optimo
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= as.integer(prediccion > 0.015391907620259)   ) ) #ATENCION  no es  1/60

#guardo el resultado
#creo las carpetas
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/KA5520/", showWarnings = FALSE )
setwd( "./exp/KA5520/" )

archivo_salida  <- "KA_FE_001.csv"

#genero el archivo para Kaggle
fwrite( entrega, 
        file= archivo_salida, 
        sep= "," )


#ahora imprimo la importancia de variables
tb_importancia  <-  as.data.table( lgb.importance(modelo) ) 
archivo_importancia  <- "552_importancia_FE_001.txt"

fwrite( tb_importancia, 
        file= archivo_importancia, 
        sep= "\t" )


#cuento cuantos 1's tiene la prediccion
#cuantos estimulos estoy enviando para retener clientes
entrega[  , sum( Predicted ) ]
