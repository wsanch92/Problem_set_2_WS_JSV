#Autores: Juan Sebastián Vásquez Acevedo y Walter Leonardo Sánchez
#Curso: Big Data and Machine Learning
#PROBLEM SET 2
#Creación: 27/06/2022

# Limpiar el espacio de trabajo y definir el directorio -----------------------------------------------------
rm(list=ls()) ## limpiar ambiente
cat("\014")   ## limpiar consola

#directorio Walter
setwd("C:/Users/walte/OneDrive/Documentos/Maestría en Economía Aplicada/Big Data/GitHub/Talleres/Problem_set_2_WS_JSV")

# Cargar librerías -----------------------------------------------------------
require(pacman)
p_load(tidyverse,dplyr,here, rvest, tibble,rio,skimr,stargazer,reshape2, class, caret, ROCR, pROC,randomForest,fastAdaboost) ## caret



# Convertir datos desde formato csv a Rds-----------------------------------------------------------
#df_id <- read.csv("Data/submission_template.csv", sep=",", header=TRUE)
#df_test_hog <- read.csv("Data/test_hogares.csv", sep=",", header=TRUE)
#df_test_per <- read.csv("Data/test_personas.csv", sep=",", header=TRUE)
#df_train_hog <- read.csv("Data/train_hogares.csv", sep=",", header=TRUE)
#df_train_per <- read.csv("Data/train_personas.csv", sep=",", header=TRUE)

#saveRDS(object = df, file = "Data/submission_template.rds")
#saveRDS(object = df_test_hog, file = "Data/test_hogares.rds")
#saveRDS(object = df_test_per, file = "Data/test_personas.rds")
#saveRDS(object = df_train_hog, file = "Data/train_hogares.rds")
#saveRDS(object = df_train_per, file = "Data/train_personas.rds")

# Cargar datos -----------------------------------------------------------

df_id <- read_rds("Data/submission_template.rds")
df_test_hog <- read_rds("Data/test_hogares.rds")
df_test_per <- read_rds("Data/test_personas.rds")
df_train_hog_1 <- read_rds("Data/train_hogares.rds")
df_train_per_11 <- read_rds("Data/train_personas.rds")

df_train_per_1 <- df_train_per_11[(df_train_per_11$id!='5bb0cc6363e59a272bf9c1f5'),] 
df_train_per <- df_train_per_1[,colnames(df_test_per)] 
df_train_hog <- df_train_hog_1[(df_train_hog_1$id!="5bb0cc6363e59a272bf9c1f5"),colnames(df_test_hog)] 
#db_c = db %>% subset(is.na(y_total_m)==F)
# Revisión y cruce de bases de datos --------------------------------------
## Existe un error donde hay un hogar unipersonal con una jefe de hogar de 11 años sin actividad.  Decidimos eliminarlo
#View(df_train_per_1[df_train_per_1$id=='5bb0cc6363e59a272bf9c1f5',])
#df_train_per[(df_train_per$id=='5bb0cc6363e59a272bf9c1f5'),]

## Imputamos los ingresos totales en na con 0 (cero) ya que son niños con edad<=11 años que no deberían estar debengando un ingreso
skim(df_train_per_1$Ingtot)

##revisar ingresos nulos
ggplot() +
  geom_histogram(df_train_per_1[is.na(df_train_per_1$Ingtot),],mapping = aes(x=P6040))
skim(df_train_per_1[is.na(df_train_per_1$Ingtot),]$P6040)

#table(df_train_per_1[is.na(df_train_per_1$P6210s1),]$Depto)
#View(df_train_per_1[is.na(df_train_per_1$P6210s1),])

ggplot() +
  geom_histogram(df_train_per_1[is.na(df_train_per_1$P6210s1),],mapping = aes(x=P6040))



#################### Construcción de bases y definicion de variables---------------------------------------------
df_train_per_1 <- df_train_per_1 %>% mutate(Ingtot_fin=ifelse(is.na(Ingtot)==TRUE,
                                                              0,
                                                              Ingtot))

## Ingresos totales por hogar
IngrXhog <- df_train_per_1 %>%  group_by(id) %>%
  summarise(Ingtot_hog = sum(Ingtot_fin))

#nombres de variables de testeo en hogares
names_df_tes_h <- colnames(df_test_hog)
vars_hogares<-c("id","Clase", "Dominio", "num_cuartos_hog", "num_cuartos_exclus_hog", "Ocupacion_vivienda", "pago_amortizacion",
                "pago_estimado_arriendo", "pago_arriendo", "personas_x_hog", "personas_x_Ug", "Li", "Lp", "Fex_c", "Depto", "Fex_dpto")

#nombres de variables de testeo en personas
names_df_tes_p <- colnames(df_test_per)
vars_personas<-c("id"              ,"Orden"          ,"Clase"        ,"Dominio"       ,"sexo"          ,"edad"          ,"parentesco"   ,"afiliacion_salud"  ,"regimen_salud"  ,"niv_educativo",
                 "escolaridad"     ,"actividad"  ,"oficio"       ,"tiempo_empresa","ocupacion_empleo"     ,"ReciHorasExtra","ReciPrimas"   ,"ReciBonificaciones","ReciAuxiAlimenta","ReciAuxiTransp",
                 "ReciSubsidFamil" ,"ReciSubsiEduc"  ,"AlimentoXpago","ViviendaXpago" ,"UsaRutaEmpresa","ReciIngEspecie","ReciPrimaServ","ReciPrimaNavid"    ,"ReciPrimaVaca"   ,"ReciViaticos",
                 "ReciBonificacion","HorasTrabSemana","P6870"        ,"cotiza_pension","P7040"         ,"P7045"         ,"P7050"        ,"P7090"             ,"P7110"           ,"P7120",
                 "P7150"           ,"P7160"          ,"P7310"        ,"P7350"         ,"P7422"         ,"P7472"         ,"P7495"        ,"P7500s2"           ,"P7500s3"         ,"P7505",
                 "P7510s1"         ,"ReciRemesas"    ,"ReciAyudaInst","P7510s5"       ,"P7510s6"       ,"P7510s7"       ,"PET"          ,"Ocupado"           ,"Desocupado"      ,"Inactivo",
                 "Fex_c"           ,"Depto"          ,"Fex_dpto")

# Renombrar variables de las bases
colnames(df_test_per) <- vars_personas
colnames(df_train_per) <- vars_personas
colnames(df_test_hog) <- vars_hogares
colnames(df_train_hog) <- vars_hogares

## Creación de variables en TRAINING PERSONA 

#creación de variables por jefe de hogar
df_train_per <- df_train_per %>% mutate(df_train_per,z=1,
                                        regimen=ifelse(regimen_salud==1 | regimen_salud==2,
                                                       1,
                                                       regimen_salud),
                                        actividad_jef=ifelse(parentesco==1,
                                                             actividad,
                                                             0),
                                        niv_educ_jef=ifelse(parentesco==1,
                                                            niv_educativo,
                                                            0))


## AGRUPAR POR HOGAR PARA VARIABLES DE ACTIVIDAD Y NIVEL EDUC DEL JEFE, EDAD PROMEDIO,
df_train_hog_vars <- df_train_per %>%  group_by(id) %>%
  summarise(edad_media = mean(edad),
            actividad_jef = max(actividad_jef),
            niv_educ_jef = max(niv_educ_jef))



## Variables: Proporción de hombres y mujeres

var_train_pers_sexo <-  dcast(df_train_per[,c('id','sexo','z')], 
                              id ~ sexo, 
                              fun.aggregate = sum, 
                              value.var = "z", 
                              margins="sexo")

var_train_pers_sexo <- var_train_pers_sexo %>% mutate(prop_hombre=var_train_pers_sexo[,"1"]/var_train_pers_sexo[,"(all)"],
                                                      prop_mujer=var_train_pers_sexo[,"2"]/var_train_pers_sexo[,"(all)"])
var_train_pers_sexo <- var_train_pers_sexo[,c("id", "prop_hombre", "prop_mujer")]



## Variables: proporción de regimenes Contributivo o Especial y Regimen Subsidiado
var_train_pers_reg <-  dcast(df_train_per[,c('id','regimen','z')], 
                             id ~ regimen, 
                             fun.aggregate = sum, 
                             value.var = "z", 
                             margins="regimen")

var_train_pers_reg <- var_train_pers_reg %>% mutate(ContEspec=var_train_pers_reg[,"1"]/var_train_pers_reg[,"(all)"],
                                                    Subsidiado=var_train_pers_reg[,"3"]/var_train_pers_reg[,"(all)"])
var_train_pers_reg <- var_train_pers_reg[,c("id", "ContEspec", "Subsidiado")]

## Variables: proporción de Cotiza pensión
var_train_pers_cotiz <-  dcast(df_train_per[,c('id','cotiza_pension','z')], 
                               id ~ cotiza_pension, 
                               fun.aggregate = sum, 
                               value.var = "z", 
                               margins="cotiza_pension")

var_train_pers_cotiz <- var_train_pers_cotiz %>% mutate(prop_si_cotiza=var_train_pers_cotiz[,"1"]/var_train_pers_cotiz[,"(all)"],
                                                        prop_no_cotiza=var_train_pers_cotiz[,"2"]/var_train_pers_cotiz[,"(all)"],
                                                        Pensionado=var_train_pers_cotiz[,"3"]/var_train_pers_cotiz[,"(all)"])
var_train_pers_cotiz <- var_train_pers_cotiz[,c("id", "prop_si_cotiza", "prop_no_cotiza", "Pensionado")]



## Creación de variables en TESTING PERSONA 
df_test_per <- df_test_per %>% mutate(df_test_per,z=1,
                                      regimen=ifelse(regimen_salud==1 | regimen_salud==2,
                                                     1,
                                                     regimen_salud),
                                      actividad_jef=ifelse(parentesco==1,
                                                           actividad,
                                                           0),
                                      niv_educ_jef=ifelse(parentesco==1,
                                                          niv_educativo,
                                                          0))


## AGRUPAR POR HOGAR PARA VARIABLES DE ACTIVIDAD Y NIVEL EDUC DEL JEFE, EDAD PROMEDIO,
df_test_hog_vars <- df_test_per %>%  group_by(id) %>%
  summarise(edad_media = mean(edad),
            actividad_jef = max(actividad_jef),
            niv_educ_jef = max(niv_educ_jef))


## Variables: Proporción de hombres y mujeres
var_test_pers_sexo <-  dcast(df_test_per[,c('id','sexo','z')], 
                             id ~ sexo, 
                             fun.aggregate = sum, 
                             value.var = "z", 
                             margins="sexo")

var_test_pers_sexo <- var_test_pers_sexo %>% mutate(prop_hombre=var_test_pers_sexo[,"1"]/var_test_pers_sexo[,"(all)"],
                                                    prop_mujer=var_test_pers_sexo[,"2"]/var_test_pers_sexo[,"(all)"])
var_test_pers_sexo <- var_test_pers_sexo[,c("id", "prop_hombre", "prop_mujer")]



## Variables: proporción de regimenes Contributivo o Especial y Regegimen Subsidiado
var_test_pers_reg <-  dcast(df_test_per[,c('id','regimen','z')], 
                            id ~ regimen, 
                            fun.aggregate = sum, 
                            value.var = "z", 
                            margins="regimen")

var_test_pers_reg <- var_test_pers_reg %>% mutate(ContEspec=var_test_pers_reg[,"1"]/var_test_pers_reg[,"(all)"],
                                                  Subsidiado=var_test_pers_reg[,"3"]/var_test_pers_reg[,"(all)"])
var_test_pers_reg <- var_test_pers_reg[,c("id", "ContEspec", "Subsidiado")]

## Variables: proporción de Cotiza pensión
var_test_pers_cotiz <-  dcast(df_test_per[,c('id','cotiza_pension','z')], 
                              id ~ cotiza_pension, 
                              fun.aggregate = sum, 
                              value.var = "z", 
                              margins="cotiza_pension")

var_test_pers_cotiz <- var_test_pers_cotiz %>% mutate(prop_si_cotiza=var_test_pers_cotiz[,"1"]/var_test_pers_cotiz[,"(all)"],
                                                      prop_no_cotiza=var_test_pers_cotiz[,"2"]/var_test_pers_cotiz[,"(all)"],
                                                      Pensionado=var_test_pers_cotiz[,"3"]/var_test_pers_cotiz[,"(all)"])
var_test_pers_cotiz <- var_test_pers_cotiz[,c("id", "prop_si_cotiza", "prop_no_cotiza", "Pensionado")]



### Formación final de las Bases de hogar para Train y Test ------------------------------------------
## Train : pegamos la base de hogar train con los ingresos totales de personas a hogares y las variables persona creadas por hogar
df_train_hog_final <- df_train_hog %>% left_join(IngrXhog, by="id") %>% 
  left_join(df_train_hog_vars, by="id") %>%
  left_join(var_train_pers_sexo, by="id") %>%
  left_join(var_train_pers_reg, by="id") %>%
  left_join(var_train_pers_cotiz, by="id") %>%  mutate(Pobre=ifelse(Ingtot_hog<Lp*personas_x_Ug,
                                                                    1,
                                                                    0)) %>%
  mutate(Pobre=factor(Pobre, level=c(1,0), labels=c("Pobre", "No_Pobre")))

skim(df_train_hog_final)


## Test: pegamos la base de hogar test con las variables de persona creadas por hogar
df_test_hog_final <- df_test_hog %>% 
  left_join(df_test_hog_vars, by="id") %>%
  left_join(var_test_pers_sexo, by="id") %>%
  left_join(var_test_pers_reg, by="id") %>%
  left_join(var_test_pers_cotiz, by="id") 

#Proporción de pobres en la base
prop.table(table(df_train_hog_final$Pobre))


#######Creación variables modelo de ingresos#########

##Asignación de variables de ingreso a las bases de personas final
var_model_ing <- c("id","Ingtot","Ingtot_fin")
df_pers_ing <- df_train_per %>% left_join(df_train_hog,by="id") %>% left_join(df_train_per_1[,var_model_ing], by="id")
skim(df_pers_ing)

##Filtro de base para personas menores de 15 años y con ingresos iguales a 0
df_pers_ing_1 <- df_pers_ing[(df_pers_ing$edad >= 15) & (df_pers_ing$Ingtot_fin>0), ]
skim(df_pers_ing_1)

## Variables del modelo de ingresos base training personas ##
##edad,edad^2,sexo,nivel_educativo,actividad,tiempo_empresa, tiempo_empresa^2,ocupacion_empleo, ReciAyudaInst, Ocupacion_vivienda, cotiza_pension, HorasTrabSemana

## Imputación: tiempo_empresa, recibe_ayudas,ocupación_empleo, cotiza_pensión, horas_de_trabjo_a_la_semana ##

## Se imputan los valores N.A. como 0 en la variable de tiempo_empresa, ya que en la actividad reportada manifiestan no estar trabajando
table(df_pers_ing_1[is.na(df_pers_ing_1$tiempo_empresa),]$edad)
table(df_pers_ing_1[is.na(df_pers_ing_1$tiempo_empresa),]$Desocupado)
table(df_pers_ing_1[is.na(df_pers_ing_1$tiempo_empresa),]$actividad)

df_pers_ing_1 <- df_pers_ing_1 %>% mutate(tiempo_empresa=ifelse(is.na(tiempo_empresa),
                                                                0,
                                                                tiempo_empresa),
                                          tiempo_empresa_sqr=tiempo_empresa^2)
skim(df_pers_ing_1$tiempo_empresa)
skim(df_pers_ing_1$tiempo_empresa_sqr)


## imputación de recibe ayudas, 1=sí 0=no
table(df_pers_ing_1$ReciAyudaInst)
df_pers_ing_1 <- df_pers_ing_1 %>% mutate(ReciAyudaInst=ifelse(is.na(ReciAyudaInst)==T | ReciAyudaInst==2
                                                               | ReciAyudaInst==9,
                                                               0,
                                                               1))
skim(df_pers_ing_1$ReciAyudaInst)

## imputación ocupación empleo, se imputan como 0 los N.A. ya que en la actividad reportada manifiestan no estar trabajando
table(df_pers_ing_1[is.na(df_pers_ing_1$ocupacion_empleo),]$actividad)
df_pers_ing_1 <- df_pers_ing_1 %>% mutate(ocupacion_empleo=ifelse(is.na(ocupacion_empleo)==T,
                                                                  0,
                                                                  ocupacion_empleo))
skim(df_pers_ing_1$ocupacion_empleo)


## imputacón cotiza_pension , se imputan como 0 los N.A. ya que en la actividad reportada manifiestan no estar trabajando
table(df_pers_ing_1$cotiza_pension)
table(df_pers_ing_1[is.na(df_pers_ing_1$cotiza_pension),]$actividad)
df_pers_ing_1 <- df_pers_ing_1 %>% mutate(cotiza_pension=ifelse(is.na(cotiza_pension)==T,
                                                                2,
                                                                cotiza_pension))
skim(df_pers_ing_1$cotiza_pension)

## imputación horas de trabajo a la semana, se imputan como 0 los N.A. ya que en la actividad reportada manifiestan no estar trabajando
table(df_pers_ing_1[is.na(df_pers_ing_1$HorasTrabSemana),]$actividad)
df_pers_ing_1 <- df_pers_ing_1 %>% mutate(HorasTrabSemana=ifelse(is.na(HorasTrabSemana)==T,
                                                                 0,
                                                                 HorasTrabSemana))

skim(df_pers_ing_1$HorasTrabSemana)

## Creación de edad^2 y recodificación de sexo

df_pers_ing_1 <- df_pers_ing_1 %>% mutate(edad_sqr=edad^2,
                                          mujer=ifelse(sexo==1,
                                                       0,
                                                       1))

df_pers_ing_1 <- df_pers_ing_1 %>% mutate(Ingtot_log=log(Ingtot_fin))


## Variables del modelo de ingresos base test personas ##

var_model_ing <- c("id")
df_pers_ing_test <- df_test_per %>% left_join(df_test_hog,by="id")
skim(df_pers_ing)

## Imputación de las siguientes varibales: niv_educativo,actividad,tiempo_empresa, ocupacion_empleo,ReciAyudaInst,cotiza_pension,HorasTrabSemana

## imputación actividad. Los N.A de actividad están en un rango de edad de 0 a 11 años, como la mayoria de los niños están estudiando los imputamos como estudiantes y a los  niños de 0 a 2 años los imputamos como otra actividad
table(df_pers_ing_test[(df_pers_ing_test$edad<=11),]$actividad,df_pers_ing_test[(df_pers_ing_test$edad<=11),]$edad)
table(df_pers_ing_test[is.na(df_pers_ing_test$actividad),]$edad)
table(df_test_per[(df_test_per$parentesco==1),]$edad)

df_pers_ing_test <- df_pers_ing_test %>% mutate(actividad=ifelse((is.na(actividad)==T) & (edad>2),
                                                                 3,
                                                                 6))

skim(df_pers_ing_test$actividad)

## Se imputan los valores N.A. como 0 en la variable de tiempo_empresa, ya que en la actividad reportada manifiestan no estar trabajando.
table(df_pers_ing_test[is.na(df_pers_ing_test$tiempo_empresa),]$edad)
table(df_pers_ing_test[is.na(df_pers_ing_test$tiempo_empresa),]$Desocupado)
table(df_pers_ing_test[is.na(df_pers_ing_test$tiempo_empresa),]$actividad)

df_pers_ing_test <- df_pers_ing_test %>% mutate(tiempo_empresa=ifelse(is.na(tiempo_empresa),
                                                                      0,
                                                                      tiempo_empresa),
                                                tiempo_empresa_sqr=tiempo_empresa^2)
skim(df_pers_ing_test$tiempo_empresa)
skim(df_pers_ing_test$tiempo_empresa_sqr)

## imputacion nivel educativo, se imputan en 0 ya que las edades de los individuos son de 0 a 2 años
table(df_pers_ing_test[is.na(df_pers_ing_test$niv_educativo),]$edad)

df_pers_ing_test <- df_pers_ing_test %>% mutate(niv_educativo=ifelse(is.na(niv_educativo),
                                                                     0,
                                                                     niv_educativo))

skim(df_pers_ing_test$niv_educativo)


## imputación de recibe ayudas, 1=sí 0=no
table(df_pers_ing_test$ReciAyudaInst)
df_pers_ing_test <- df_pers_ing_test %>% mutate(ReciAyudaInst=ifelse(is.na(ReciAyudaInst)==T | ReciAyudaInst==2
                                                                     | ReciAyudaInst==9,
                                                                     0,
                                                                     1))
skim(df_pers_ing_test$ReciAyudaInst)

## imputación ocupación empleo, se imputan como 0 ya que dentro de las actividades reportadas manifiestan no estar trabajando
table(df_pers_ing_test[is.na(df_pers_ing_test$ocupacion_empleo),]$actividad)
df_pers_ing_test <- df_pers_ing_test %>% mutate(ocupacion_empleo=ifelse(is.na(ocupacion_empleo)==T,
                                                                        0,
                                                                        ocupacion_empleo))
skim(df_pers_ing_test$ocupacion_empleo)

## imputacón cotiza_pension 
table(df_pers_ing_test$cotiza_pension)
table(df_pers_ing_test[is.na(df_pers_ing_test$cotiza_pension),]$actividad)
table(df_pers_ing_test[(is.na(df_pers_ing_test$cotiza_pension) ) & (df_pers_ing_test$actividad==1),]$ocupacion_empleo)

df_pers_ing_test <- df_pers_ing_test %>% mutate(cotiza_pension=ifelse(is.na(cotiza_pension)==T & actividad!=1,
                                                                      2,
                                                                      ifelse(is.na(cotiza_pension)==T & actividad==1 & (ocupacion_empleo==1|ocupacion_empleo==5),
                                                                             1,
                                                                             ifelse(is.na(cotiza_pension)==T & actividad==1 & (ocupacion_empleo!=1 & ocupacion_empleo!=5),
                                                                                    2,
                                                                                    ifelse(is.na(cotiza_pension)==T,
                                                                                           2,
                                                                                           cotiza_pension)))))
skim(df_pers_ing_test$cotiza_pension)

## Creación de edad^2 y recodificación de sexo

df_pers_ing_test <- df_pers_ing_test %>% mutate(edad_sqr=edad^2,
                                                mujer=ifelse(sexo==1,
                                                             0,
                                                             1))


#### 1) Problema de clasificación:  POBREZA -----------------------------------------------------

set.seed(202207) ## fijo la semilla

## particiones de la muestra

split1 <- createDataPartition(df_train_hog_final$Pobre, p = .7)[[1]]
length(split1)

other <- df_train_hog_final[-split1,]
nrow(other)
training <- df_train_hog_final[split1,] ## La muestra de entrenamiento "1"

##para usar la evaluación y testeo

#set.seed(202208)
split2 <- createDataPartition(other$Pobre, p = 1/3)[[1]]
evaluation <- other[split2,] ## La muestra de evaluación "2"
testing <- other[-split2,]   ## La muestra de testeo "3"


prop.table(table(training$Pobre))

prop.table(table(testing$Pobre))

prop.table(table(evaluation$Pobre))


### Crear modelo K-vecinos cercanos k=1 hasta k=13--------------------------------

# Re escalar variables
var_knn <- c("num_cuartos_exclus_hog", "Ocupacion_vivienda","personas_x_Ug","edad_media","niv_educ_jef","prop_hombre","prop_mujer","ContEspec","Subsidiado","prop_si_cotiza","prop_no_cotiza","Pensionado")

x <- scale(df_train_hog_final[,var_knn]) ## re escalar media 0 sd 1 
apply(x,2,sd) ## comprobamos que sd=1

model_knn <- c(1,5) #,7,10,13)
KNN <- data.frame()
for (i in model_knn){
  k <- knn(train=x[split1,], ## base de entrenamiento estan todas menos las de testeo
           test=x[-split1,],   ## base de testeo
           cl=df_train_hog_final$Pobre[split1], ## outcome
           k=i)        ## vecinos 
  
  
  #data.frame(df_test_hog_final$Pobre[-training],k1)
  
  ## matriz de confusión esta me permite hacer la matriz
  cm_k <- confusionMatrix(data=k , 
                          reference=df_train_hog_final$Pobre[-split1] , 
                          mode="sens_spec" , 
                          positive="Pobre")$table
  cm <- data.frame(cbind(modelo = paste0("Knn_", i),cm_k))
  KNN<-rbind(KNN, cm)
}
### Todas las matices de confusión de kNN
KNN


