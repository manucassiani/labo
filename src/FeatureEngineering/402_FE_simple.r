#Feature Engineering
#creo nuevas variables dentro del mismo mes

#este script se muestra como esqueleto para que los alumnos agreguen sus propias variables
#ya sea basados en la teoria economica  o en el salvaje empiricismo
# "No es que la nueva variable creada, que funciona, no tenga sentido, lo que sucede es que yo estoy siendo capaz de encontrarselo en este momento"

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")



EnriquecerDataset  <- function( dataset , arch_destino )
{

  #INICIO de la seccion donde se deben hacer cambios con variables nuevas

  #creo un ctr_quarter que tenga en cuenta cuando los clientes hace 3 menos meses que estan
  dataset1[  , ctrx_quarter_normalizado := ctrx_quarter ]
  dataset1[ cliente_antiguedad== 1 , ctrx_quarter_normalizado := ctrx_quarter * 5 ]
  dataset1[ cliente_antiguedad== 2 , ctrx_quarter_normalizado := ctrx_quarter * 2 ]
  dataset1[ cliente_antiguedad== 3 , ctrx_quarter_normalizado := ctrx_quarter * 1.2 ]

  #variable extraida de una tesis de maestria de Irlanda
  dataset[  , mpayroll_sobre_edad  := mpayroll / cliente_edad ]

  #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
  #varias formas de combinar Visa_status y Master_status

  dataset1[ , mv_status01 := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
  dataset1[ , mv_status02 := Master_status +  Visa_status ]
  dataset1[ , mv_status03 := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
  dataset1[ , mv_status04 := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
  dataset1[ , mv_status05 := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]

  dataset1[ , mv_status06 := ifelse( is.na(Visa_status), 
                                    ifelse( is.na(Master_status), 10, Master_status), 
                                    Visa_status) ]

  dataset1[ , mv_status07 := ifelse( is.na(Master_status), 
                                    ifelse( is.na(Visa_status), 10, Visa_status), 
                                    Master_status) ]


  #combino MasterCard y Visa , teniendo en cuenta los NA
  dataset1[ , mv_mfinanciacion_limite  := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]

  dataset1[ , mv_Fvencimiento          := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
  dataset1[ , mv_Finiciomora           := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
  dataset1[ , mv_msaldototal           := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
  dataset1[ , mv_msaldopesos           := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
  dataset1[ , mv_msaldodolares         := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
  dataset1[ , mv_mconsumospesos        := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
  dataset1[ , mv_mconsumosdolares      := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
  dataset1[ , mv_mlimitecompra         := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
  dataset1[ , mv_madelantopesos        := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
  dataset1[ , mv_madelantodolares      := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
  dataset1[ , mv_fultimo_cierre        := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
  dataset1[ , mv_mpagado               := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
  dataset1[ , mv_mpagospesos           := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
  dataset1[ , mv_mpagosdolares         := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
  dataset1[ , mv_fechaalta             := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
  dataset1[ , mv_mconsumototal         := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
  dataset1[ , mv_cconsumos             := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
  dataset1[ , mv_cadelantosefectivo    := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
  dataset1[ , mv_mpagominimo           := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]


  #a partir de aqui juego con la suma de Mastercard y Visa
  dataset1[ , mvr_Master_mlimitecompra := Master_mlimitecompra / mv_mlimitecompra ]
  dataset1[ , mvr_Visa_mlimitecompra   := Visa_mlimitecompra / mv_mlimitecompra ]
  dataset1[ , mvr_msaldototal          := mv_msaldototal / mv_mlimitecompra ]
  dataset1[ , mvr_msaldopesos          := mv_msaldopesos / mv_mlimitecompra ]
  dataset1[ , mvr_msaldopesos2         := mv_msaldopesos / mv_msaldototal ]
  dataset1[ , mvr_msaldodolares        := mv_msaldodolares / mv_mlimitecompra ]
  dataset1[ , mvr_msaldodolares2       := mv_msaldodolares / mv_msaldototal ]
  dataset1[ , mvr_mconsumospesos       := mv_mconsumospesos / mv_mlimitecompra ]
  dataset1[ , mvr_mconsumosdolares     := mv_mconsumosdolares / mv_mlimitecompra ]
  dataset1[ , mvr_madelantopesos       := mv_madelantopesos / mv_mlimitecompra ]
  dataset1[ , mvr_madelantodolares     := mv_madelantodolares / mv_mlimitecompra ]
  dataset1[ , mvr_mpagado              := mv_mpagado / mv_mlimitecompra ]
  dataset1[ , mvr_mpagospesos          := mv_mpagospesos / mv_mlimitecompra ]
  dataset1[ , mvr_mpagosdolares        := mv_mpagosdolares / mv_mlimitecompra ]
  dataset1[ , mvr_mconsumototal        := mv_mconsumototal  / mv_mlimitecompra ]
  dataset1[ , mvr_mpagominimo          := mv_mpagominimo  / mv_mlimitecompra ]

  dataset1[ , variable_prueba := mv_fechaalta / mv_cconsumos]
  

  
  as.numeric(gsub(".","",dietas[,3], fixed = TRUE))
  dataset1 = dataset1[, mrentabilidad := as.numeric(gsub(",",".",mrentabilidad, fixed=TRUE)) ]
  
  dataset1[ , num_cliente_VS_mrentabilidad  := numero_de_cliente * mrentabilidad ]
  dataset1[, c("numero_de_cliente","mrentabilidad","num_cliente_VS_mrentabilidad")]
  
  all_interactions = combn(names(dataset1), 2)  
  for (position in seq(length(all_interactions)/2))
  {
      col1 = all_interactions[,position][1]
      col2 = all_interactions[,position][2]
      col_name = paste(col1,col2,sep="_VS_")
      dataset1[ , toString(col_name)      := as.numeric(gsub(",",".",get(col1),fixed=TRUE)) * as.numeric(gsub(",",".",get(col2),fixed=TRUE)) ]
  }

  #valvula de seguridad para evitar valores infinitos
  #paso los infinitos a NULOS
  infinitos      <- lapply( names(dataset),
                            function(.name) dataset[ , sum(is.infinite( get(.name) )) ]  )
  
  infinitos_qty  <- sum( unlist( infinitos ) )
  if( infinitos_qty > 0 )
  {
    cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
    dataset[mapply(is.infinite, dataset)] <- NA
  }


  #valvula de seguridad para evitar valores NaN  que es 0/0
  #paso los NaN a 0 , decision polemica si las hay
  #se invita a asignar un valor razonable segun la semantica del campo creado
  nans      <- lapply( names(dataset),
                       function(.name) dataset[ , sum( is.nan( get(.name) )) ] )
  
  nans_qty  <- sum( unlist( nans) )
  if( nans_qty > 0 )
  {
    cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
    cat( "Si no te gusta la decision, modifica a gusto el script!\n\n")
    dataset[mapply(is.nan, dataset)] <- 0
  }

  #FIN de la seccion donde se deben hacer cambios con variables nuevas

  #grabo con nombre extendido
  fwrite( dataset,
          file= arch_destino,
          sep= "," )

}
#------------------------------------------------------------------------------

#aqui comienza el programa

#Establezco el Working Directory
setwd( "/home/manuel/Escritorio/ITBA/03-Minería_de_Datos/01-GIT/" )


#lectura de los datasets
dataset1  <- fread("./datasets/paquete_premium_202011.csv")
dataset2  <- fread("./datasets/paquete_premium_202101.csv")


#creo la carpeta donde va el experimento
# FE  representa  Feature Engineering
dir.create( "./labo/exp/",  showWarnings = FALSE ) 
dir.create( "./labo/exp/FE4020/", showWarnings = FALSE )
setwd("D:\\gdrive\\ITBA2022A\\labo\\exp\\FE4020\\")   #Establezco el Working Directory DEL EXPERIMENTO

EnriquecerDataset( dataset1, "paquete_premium_202011_ext.csv" )
EnriquecerDataset( dataset2, "paquete_premium_202101_ext.csv" )

