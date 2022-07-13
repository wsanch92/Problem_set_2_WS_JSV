#Autores: Juan Sebastián Vásquez Acevedo y Walter Leonardo Sánchez
#Curso: Big Data and Machine Learning
#PROBLEM SET 2
#Creación: 27/06/2022

# Limpiar el espacio de trabajo y definir el directorio -----------------------------------------------------
rm(list=ls()) ## limpiar ambiente
cat("\014")   ## limpiar consola

#directorio Walter
#setwd("C:/Users/walte/OneDrive/Documentos/Maestría en Economía Aplicada/Big Data/GitHub/Talleres/Problem_set_2_WS_JSV")

##directorio Juan
#setwd("/Users/usuario/Desktop/Problem_set_2_WS_JSV")
dir()

# Cargar librerías -----------------------------------------------------------
require(pacman)
p_load(gtsummary,tidyverse,dplyr,here, rvest, tibble,rio,skimr,stargazer,reshape2, class, caret, ROCR, pROC,randomForest,fastAdaboost) ## caret



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
#skim(df_train_per_1[is.na(df_train_per_1$Ingtot),]$P6040)

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

#skim(df_train_hog_final)


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
var_model_ing <- c("id","Orden","Ingtot","Ingtot_fin")
df_pers_ing <- df_train_per %>% left_join(df_train_per_1[,var_model_ing], by=c("id","Orden")) %>% left_join(df_train_hog,by="id")



#skim(df_pers_ing)

##Filtro de base para personas menores de 15 años y con ingresos iguales a 0
df_pers_ing_1 <- df_pers_ing[(df_pers_ing$edad >= 15) & (df_pers_ing$Ingtot_fin>0), ]
#skim(df_pers_ing_1)


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
#skim(df_pers_ing_1$tiempo_empresa)
#skim(df_pers_ing_1$tiempo_empresa_sqr)


## imputación de recibe ayudas, 1=sí 0=no
table(df_pers_ing_1$ReciAyudaInst)
df_pers_ing_1 <- df_pers_ing_1 %>% mutate(ReciAyudaInst=ifelse(is.na(ReciAyudaInst)==T | ReciAyudaInst==2
                                                               | ReciAyudaInst==9,
                                                               0,
                                                               1))
#skim(df_pers_ing_1$ReciAyudaInst)

## imputación ocupación empleo, se imputan como 0 los N.A. ya que en la actividad reportada manifiestan no estar trabajando
table(df_pers_ing_1[is.na(df_pers_ing_1$ocupacion_empleo),]$actividad)
df_pers_ing_1 <- df_pers_ing_1 %>% mutate(ocupacion_empleo=ifelse(is.na(ocupacion_empleo)==T,
                                                                  0,
                                                                  ocupacion_empleo))
#skim(df_pers_ing_1$ocupacion_empleo)


## imputacón cotiza_pension , se imputan como 0 los N.A. ya que en la actividad reportada manifiestan no estar trabajando
table(df_pers_ing_1$cotiza_pension)
table(df_pers_ing_1[is.na(df_pers_ing_1$cotiza_pension),]$actividad)
df_pers_ing_1 <- df_pers_ing_1 %>% mutate(cotiza_pension=ifelse(is.na(cotiza_pension)==T,
                                                                2,
                                                                cotiza_pension))
#skim(df_pers_ing_1$cotiza_pension)

## imputación horas de trabajo a la semana, se imputan como 0 los N.A. ya que en la actividad reportada manifiestan no estar trabajando
table(df_pers_ing_1[is.na(df_pers_ing_1$HorasTrabSemana),]$actividad)
df_pers_ing_1 <- df_pers_ing_1 %>% mutate(HorasTrabSemana=ifelse(is.na(HorasTrabSemana)==T,
                                                                 0,
                                                                 HorasTrabSemana))

#skim(df_pers_ing_1$HorasTrabSemana)

## Creación de edad^2 y recodificación de sexo

df_pers_ing_1 <- df_pers_ing_1 %>% mutate(edad_sqr=edad^2,
                                          mujer=ifelse(sexo==1,
                                                       0,
                                                       1))

df_pers_ing_1 <- df_pers_ing_1 %>% mutate(Ingtot_log=log(Ingtot_fin))



## Variables del modelo de ingresos base test personas ###############################################################

var_model_ing <- c("id")
df_pers_ing_test <- df_test_per %>% left_join(df_test_hog,by="id")
#skim(df_pers_ing)

## Imputación de las siguientes varibales: niv_educativo,actividad,tiempo_empresa, ocupacion_empleo,ReciAyudaInst,cotiza_pension,HorasTrabSemana

## imputación actividad. Los N.A de actividad están en un rango de edad de 0 a 11 años, como la mayoria de los niños están estudiando los imputamos como estudiantes y a los  niños de 0 a 2 años los imputamos como otra actividad
table(df_pers_ing_test[(df_pers_ing_test$edad<=11),]$actividad,df_pers_ing_test[(df_pers_ing_test$edad<=11),]$edad)
table(df_pers_ing_test[is.na(df_pers_ing_test$actividad),]$edad)
table(df_test_per[(df_test_per$parentesco==1),]$edad)

df_pers_ing_test <- df_pers_ing_test %>% mutate(actividad=ifelse((is.na(actividad)==T) & (edad>2),
                                                                 3,
                                                                 6))

#skim(df_pers_ing_test$actividad)

## Se imputan los valores N.A. como 0 en la variable de tiempo_empresa, ya que en la actividad reportada manifiestan no estar trabajando.
table(df_pers_ing_test[is.na(df_pers_ing_test$tiempo_empresa),]$edad)
table(df_pers_ing_test[is.na(df_pers_ing_test$tiempo_empresa),]$Desocupado)
table(df_pers_ing_test[is.na(df_pers_ing_test$tiempo_empresa),]$actividad)

df_pers_ing_test <- df_pers_ing_test %>% mutate(tiempo_empresa=ifelse(is.na(tiempo_empresa),
                                                                      0,
                                                                      tiempo_empresa),
                                                tiempo_empresa_sqr=tiempo_empresa^2)
#skim(df_pers_ing_test$tiempo_empresa)
#skim(df_pers_ing_test$tiempo_empresa_sqr)

## imputacion nivel educativo, se imputan en 0 ya que las edades de los individuos son de 0 a 2 años
table(df_pers_ing_test[is.na(df_pers_ing_test$niv_educativo),]$edad)

df_pers_ing_test <- df_pers_ing_test %>% mutate(niv_educativo=ifelse(is.na(niv_educativo),
                                                                     0,
                                                                     niv_educativo))

#skim(df_pers_ing_test$niv_educativo)


## imputación de recibe ayudas, 1=sí 0=no
table(df_pers_ing_test$ReciAyudaInst)
df_pers_ing_test <- df_pers_ing_test %>% mutate(ReciAyudaInst=ifelse(is.na(ReciAyudaInst)==T | ReciAyudaInst==2
                                                                     | ReciAyudaInst==9,
                                                                     0,
                                                                     1))
#skim(df_pers_ing_test$ReciAyudaInst)

## imputación ocupación empleo, se imputan como 0 ya que dentro de las actividades reportadas manifiestan no estar trabajando
table(df_pers_ing_test[is.na(df_pers_ing_test$ocupacion_empleo),]$actividad)
df_pers_ing_test <- df_pers_ing_test %>% mutate(ocupacion_empleo=ifelse(is.na(ocupacion_empleo)==T,
                                                                        0,
                                                                        ocupacion_empleo))
#skim(df_pers_ing_test$ocupacion_empleo)

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
#skim(df_pers_ing_test$cotiza_pension)

## Creación de edad^2 y recodificación de sexo

df_pers_ing_test <- df_pers_ing_test %>% mutate(edad_sqr=edad^2,
                                                mujer=ifelse(sexo==1,
                                                             0,
                                                             1))

## imputación horas de trabajo a la semana, se imputan como 0 los N.A. ya que en la actividad reportada manifiestan no estar trabajando
table(df_pers_ing_test[is.na(df_pers_ing_test$HorasTrabSemana),]$actividad)
df_pers_ing_test <- df_pers_ing_test %>% mutate(HorasTrabSemana=ifelse(is.na(HorasTrabSemana)==T,
                                                                 0,
                                                                 HorasTrabSemana))

#### 1) Problema de clasificación:  POBREZA -----------------------------------------------------

set.seed(777) ## fijo la semilla

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

#### TABLAS Y GRÁFICOS --------------------------------------------------------------------------------------
## tabla de estadísticas de las variales del modelo de Clasificación
var_clas_tabla <- c("Pobre","actividad_jef","niv_educ_jef","Ocupacion_vivienda","edad_media","personas_x_Ug","num_cuartos_exclus_hog","prop_hombre",
                    "prop_mujer","ContEspec","Subsidiado","prop_si_cotiza","prop_no_cotiza","Pensionado")
tbl_summary(df_train_hog_final[,var_clas_tabla], statistic = list(all_continuous()~ "{mean} ({sd})" ),
            label=list(Pobre ~ "Pobreza", actividad_jef ~"Actividad del jefe de hogar", niv_educ_jef~"Nivel educativo del jefe de Hogar",
                       Ocupacion_vivienda ~ "Ocupación de la vivienda", edad_media ~ "Edad promedio del hogar",personas_x_Ug ~ "Personas por Unidad de gasto",
                       num_cuartos_exclus_hog ~ "Número de cuartos exclusivos",prop_hombre~"Proporción de hombres",prop_mujer ~ "Proporción de mujeres",
                       ContEspec ~ "Proporción de personas en régimen contributivo y especial", Subsidiado ~ "Proporción de personas en régimen Subsidiado",
                       prop_si_cotiza~"Proporción de personas que cotizan pensión",prop_no_cotiza~"Proporción de personas que No cotizan pensión",
                       Pensionado ~ "Proporción de personas pensionadas"))

## tabla de estadísticas de las variales del modelo de Ingresos
var_ingreso_tabla <- c("sexo","actividad","ocupacion_empleo","cotiza_pension","Ocupacion_vivienda","ReciAyudaInst",
                    "Ingtot","Ingtot_fin","edad","tiempo_empresa")
tbl_summary(df_pers_ing[,var_ingreso_tabla], statistic = list(all_continuous()~ "{mean} ({sd})" ),
            label=list(sexo ~ "Género", actividad ~"Actividad", ocupacion_empleo~"Posición ocupacional",
                       Ocupacion_vivienda ~ "Ocupación de la vivienda",ReciAyudaInst ~ "Recibe ayudas institucionales",
                       Ingtot ~ "Ingreso total sin imputar", Ingtot_fin ~ "Ingreso total imputado",
                       edad ~ "Edad promedio del hogar", tiempo_empresa~ "Horas trabajadas al mes",
                       cotiza_pension ~ "Cotizante a pensión"))





### Crear modelo K-vecinos cercanos k=1 hasta k=13--------------------------------

# Re escalar variables
var_knn <- c("num_cuartos_exclus_hog", "Ocupacion_vivienda","personas_x_Ug","edad_media","niv_educ_jef","prop_hombre","prop_mujer","ContEspec","Subsidiado","prop_si_cotiza","prop_no_cotiza","Pensionado")

x <- scale(df_train_hog_final[,var_knn]) ## re escalar media 0 sd 1 
apply(x,2,sd) ## comprobamos que sd=1

model_knn <- c(1,5,7,10,13)

KNN <- data.frame()
KNN_sensibility <- data.frame()
#ejecutar loop para k vecinos
for (i in model_knn){
  k <- knn(train=x[split1,], ## base de entrenamiento estan todas menos las de testeo
           test=x[-split1,],   ## base de testeo
           cl=df_train_hog_final$Pobre[split1], ## outcome
           k=i)        ## vecinos 
  
  
  ## matriz de confusión esta me permite hacer la matriz
  cm_k <- confusionMatrix(data=k , 
                          reference=df_train_hog_final$Pobre[-split1] , 
                          mode="sens_spec" , 
                          positive="Pobre")
  cm <- data.frame(cbind(modelo = paste0("Knn_", i),cm_k$table))
  KNN<-rbind(KNN, cm)
  KNN_sens <- data.frame(cbind(Modelo = paste0("Knn_",i),Sensibility = cm_k[["byClass"]][["Sensitivity"]]))
  KNN_sensibility <- data.frame(rbind(KNN_sensibility, KNN_sens))
}
### Todas las matices de confusión de kNN
KNN
KNN_sensibility

### Modelo  Logit

## solver packages conflicts
predict <- stats::predict

## Logit normal -----------------------------------------------------------------------------

## Modelo a ajustar
model <- as.formula("Pobre~factor(actividad_jef)+factor(niv_educ_jef)+personas_x_Ug+factor(Ocupacion_vivienda)+num_cuartos_exclus_hog+ContEspec+
      Subsidiado+prop_si_cotiza+prop_no_cotiza+Pensionado+factor(Depto)+edad_media+prop_hombre+prop_mujer")

## Entrenamiento
glm_logit <- glm(model , family=binomial(link="logit") , data=training)

## Predicción
testing$predict_logit <- predict(glm_logit , testing , type="response")



## Logit CV--------------------------

predict <- stats::predict

## define control de entrenamiento
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
control <- trainControl(method = "cv", number = 5,
                        summaryFunction = fiveStats, 
                        classProbs = TRUE,
                        verbose=FALSE,
                        savePredictions = T)
## Entrenamiento

logit_cv = train(model,
                 data=training,
                 method="glm",
                 trControl = control,
                 family = "binomial",
                 preProcess = c("center", "scale"))
logit_cv

## predicción
testing$p_logit_cv <- predict(logit_cv , testing , type="prob")[1] 

## ROC
pred <- prediction(testing$p_logit_cv , testing$Pobre)

roc_ROCR_logit_cv <- performance(pred,"tpr","fpr")

auc_roc = performance(pred, measure = "auc")
auc_roc@y.values[[1]] #0.8803616

## Encontrar punto de corte óptimo


evalResults <- data.frame(Pobre = evaluation$Pobre)

evalResults$Roc_logid_cv <- predict(logit_cv, newdata = evaluation, type = "prob")[,1]

rfROC <- roc(evalResults$Pobre, evalResults$Roc_logid_cv, levels = rev(levels(evalResults$Pobre)))

rfTresh_log_cv <- coords(rfROC, x = "best", best.method = "closest.topleft")

rfTresh_log_cv[1] # 0.2658217

## Lasso- Ridge Logit y ElasticNet Cross-Validation---------------------------

## Definición de funciones de estadísticos y Grilla de lambda
set.seed(777)
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
ctrl <- trainControl(method = "cv",
                     number = 5,
                     summaryFunction = fiveStats,
                     classProbs = TRUE,
                     verbose=FALSE,
                     savePredictions = T)

lambda_grid <- 10~ seq(-4,0.01, length = 100) ## en la practica se utilizan grillas de 100, 200 o 300

lambda_grid #Grilla de lambda



## modelos por correr de lasso, ridge y elastic net -------------------------------------------------

#vector de modelos 
modelos_reg <-c('NULL', 'expand.grid(alpha =0, lambda=lambda_grid)', 'expand.grid(alpha =1, lambda=lambda_grid)') 

sensibili <- data.frame()     # dataframe para guardar estadísticos
#predicciones <- data.frame()  # dataframe para guardar predicciones
auc <- data.frame()           # dataframe para guardar las  áreas bajo la curva
curvas_ROC <- list()          # Lista para guardar las curvas ROC
rfThresh_fin <- data.frame()  #dataframe para guardar threshold

# Loop para entrenar, predecir y encontrar puntos de corte de lambda de los modelos lasso, ridge y elastic net
for (met in modelos_reg) {
  
  #entrenamiento de los modelos para cada elemento de la lista modelos_reg
  mylogit_lasso_acc <- train(
    model,
    data = training,
    method = "glmnet",  ## para lasso
    trControl = ctrl,
    family = "binomial",
    metric = "Sens", ## me va a elegir el lambda que me maximiza la la sensibilidad
    tunGrid = met, ## si esta parte es Null se hace un ElasticNEt, con alpha==0 es un Ridge y con alpha=1 es un lasso
    preProcess = c("center", "scale")
  )
  
  #condicionales para nombrer modelos
  if ( met == 'NULL') {
    mod <- c('Elasticnet')
  } else if ( met == 'expand.grid(alpha =0, lambda=lambda_grid)') {
    mod <- c('Lasso_logit')
  } else if ( met == 'expand.grid(alpha =1, lambda=lambda_grid)') {
    mod <- c('Ridge_logit')
  }
  # Se guarda los valores de sensibilidad, Especificidad y lambda
  x<-as.data.frame(cbind(Modelo = mod,alpha =mylogit_lasso_acc[["results"]][["alpha"]],lambda = mylogit_lasso_acc[["results"]][["lambda"]], Sensibility=mylogit_lasso_acc[["results"]][["Sens"]],Specify=mylogit_lasso_acc[["results"]][["Spec"]]))
  sens <- x[x$lambda==mylogit_lasso_acc[["bestTune"]][["lambda"]] & x$alpha==mylogit_lasso_acc[["bestTune"]][["alpha"]],]
  
  # dataframe con las estadísticas anteriores
  sensibili <- rbind(sensibili, sens)
  
  #Predicciones de los modelos
  pred <- predict(mylogit_lasso_acc, testing, type="prob")[1]
  #agregar pred a la base de testing hogares
  testing <- cbind(testing, pred$Pobre)
  
  colnames(testing)[ncol(testing)] <- paste0('p_',mod) #renombrar variable pred en testing
  preds <- prediction(testing[,ncol(testing)] , testing$Pobre) # predicción de pobreza para roc
  
  # curva ROC y AUC
  roc_ROCR <- list(mod = performance(preds,"tpr","fpr"))
  curvas_ROC <- append(curvas_ROC, roc_ROCR) 
  
  auc_roc = performance(preds, measure = "auc")
  auc <- rbind(auc, auc_roc@y.values[[1]])
  cat("Salió: ",mod,"\n")
  
  ## Evaluación del Threshold en evaluation para estos modelos
  
  evalResults <- data.frame(Pobre = evaluation$Pobre) #agregamos la variable pobre observada
  
  evalResults$Roc_logid_lasso <- predict(mylogit_lasso_acc, newdata = evaluation, type = "prob")[,1]
  
  rfROC <- roc(evalResults$Pobre, evalResults$Roc_logid_lasso, levels = rev(levels(evalResults$Pobre)))
  
  rfThresh <- coords(rfROC, x = "best", best.method = "closest.topleft") %>% mutate(modelo = mod)
  
  rfThresh_fin <- rbind(rfThresh_fin,rfThresh)
  cat("Salió: ",mod,"\n")
}

rfThresh_fin ## puntos de corte óptimos para modelos del loop


## Random Forest ------------------------------------------------------


## Este modelo toma un tiempo en ejecutarse por lo que guardamos el modelo para estimar luego
#set.seed(202207)
#fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
#ctrl <- trainControl(method = "cv",
#                     number = 5,
#                     summaryFunction = fiveStats,
#                     classProbs = TRUE,
#                     verbose=FALSE,
#                     savePredictions = T)
#forest <- train(
#  model,
#  data=training,
#  method ="rf",
#  trControl = ctrl,
#  family = "binomial",
#  metric="Sens",
#  #preProcess = c("center", "scale")
#)

## Guardamos el modelo para uso posterior 
#saveRDS(forest, "Data/modelo_forest.rds")


## uso del modelo Random Forest------------------

myforest <- readRDS("Data/modelo_forest.rds") ## se trae el modelo RF para predecir 

x<-as.data.frame(cbind(Modelo = c("Random_forest"),alpha = myforest[["results"]][["mtry"]],lambda = c(""), Sensibility=myforest[["results"]][["Sens"]],Specify=myforest[["results"]][["Spec"]]))
sens <- x[x$alpha==myforest[["bestTune"]][["mtry"]],]

# dataframe con las estadísticas anteriores
sensibili <- rbind(sensibili, sens)


## Predicciones
pred <- predict(myforest, testing, type="prob")[1]
testing <- cbind(testing, pred$Pobre)
colnames(testing)[ncol(testing)] <- c("pr_forest")
preds <- prediction(testing$pr_forest , testing$Pobre)

# curva ROC y AUC
roc_ROCR_rf <- performance(preds,"tpr","fpr")
curvas_ROC <- append(curvas_ROC, roc_ROCR_rf)

## Area bajo la curva
auc_roc_rf <- performance(preds, measure = "auc")
auc <- rbind(auc, auc_roc_rf@y.values[[1]])
colnames(auc)<- c("AUC")

## Evaluación del Threshold en evaluation del random forest

evalResults <- data.frame(Pobre = evaluation$Pobre)
evalResults$rROC_forest <- predict(myforest, newdata = evaluation, type = "prob")[,1]
#colnames(testing)[ncol(testing)] <- paste0('pr_',mod)
rfROC <- roc(evalResults$Pobre, evalResults$rROC_forest, levels = rev(levels(evalResults$Pobre)))

rfThresh <- coords(rfROC, x = "best", best.method = "closest.topleft") %>% mutate(modelo = "Random_forest")

rfThresh_fin <- rbind(rfThresh_fin,rfThresh)


##Curvas de ROC
plot(roc_ROCR_logit_cv, main = "ROC curve", colorize = FALSE, col="black")
plot(curvas_ROC[[1]], add=TRUE,  colorize = FALSE, col="red")
plot(curvas_ROC[[2]], add=TRUE, colorize = FALSE, col="blue")
plot(curvas_ROC[[3]], add=TRUE, colorize = FALSE, col= "green")
plot(curvas_ROC[[4]], add=TRUE, colorize = FALSE, col= "yellow")
abline(a = 0, b = 1)

rfThresh_fin[,-(2:3)]
auc
sensibili
colnames(auc) <- c("modelos")


## Clasificaciones

# KNN: ya esta en la base de testing
KNN

#sensibility knn
KNN_sensibility


## Logit 

#Predicción Logit
testing <- testing %>% 
  mutate(p_logit_bayes=ifelse(predict_logit>0.5,1,0) %>% 
           factor(.,levels=c(1,0),labels=c("Pobre","No_Pobre")))

## Matriz de confusion de logit normal
cm_logit <- confusionMatrix(data=testing$p_logit_bayes, 
                               reference=testing$Pobre , 
                               mode="sens_spec" , positive="Pobre")

cm_logit$table
s1 <- cm_logit[["byClass"]][["Sensitivity"]] # sebilidad de logit


## Logit CV


## Matriz de confusion de logit CV k-fold
testing <- testing %>% 
  mutate(p_logit_cv_bayes=ifelse(p_logit_cv>0.5,1,0) %>% 
           factor(.,levels=c(1,0),labels=c("Pobre","No_Pobre")))

cm_logit_cv <- confusionMatrix(data=testing$p_logit_cv_bayes, 
                            reference=testing$Pobre , 
                            mode="sens_spec" , positive="Pobre")
cm_logit_cv$table
s2 <- cm_logit_cv[["byClass"]][["Sensitivity"]] #sensibilidad logid CV k-Fold


## logit CV k-fold con Thresh
testing <- testing %>% 
  mutate(p_logit_cv_th=ifelse(p_logit_cv>rfTresh_log_cv[1,1],1,0) %>% 
           factor(.,levels=c(1,0),labels=c("Pobre","No_Pobre")))

## Matriz de confusión de logit CV k-fold con Thresh
cm_logit_cv_th <- confusionMatrix(data=testing$p_logit_cv_th, 
                               reference=testing$Pobre , 
                               mode="sens_spec" , positive="Pobre")
cm_logit_cv_th$table
s3 <- cm_logit_cv_th[["byClass"]][["Sensitivity"]] ## sensibilidad con logit CV k-fold con Thresh


# lasso-logit
testing <- testing %>% 
  mutate(p_Lasso_logit_bayes=ifelse(p_Lasso_logit>0.5,1,0) %>% 
           factor(.,levels=c(1,0),labels=c("Pobre","No_Pobre")))

## Matriz de confusion de lasso CV k-fold
cm_lasso <- confusionMatrix(data=testing$p_Lasso_logit_bayes, 
                            reference=testing$Pobre , 
                            mode="sens_spec" , positive="Pobre")
cm_lasso$table
s4 <- cm_lasso[["byClass"]][["Sensitivity"]] # sensibilidad con lasso CV k-fold


## Lasso logit CV k-fold con Thresh
testing <- testing %>% 
  mutate(p_Lasso_logit_th=ifelse(p_Lasso_logit>rfThresh_fin[2,1],1,0) %>% 
           factor(.,levels=c(1,0),labels=c("Pobre","No_Pobre")))

## Matriz de confusion de Lasso logit CV k-fold con Thresh
cm_lasso_th <- confusionMatrix(data=testing$p_Lasso_logit_th, 
                            reference=testing$Pobre , 
                            mode="sens_spec" , positive="Pobre")
cm_lasso_th$table
s5 <- cm_lasso_th[["byClass"]][["Sensitivity"]] ## Sensibilidad con Lasso logit CV k-fold con Thresh


## Ridge-logit
testing <- testing %>% 
  mutate(p_Ridge_logit_bayes=ifelse(p_Ridge_logit>0.5,1,0) %>% 
           factor(.,levels=c(1,0),labels=c("Pobre","No_Pobre")))

## Matriz de confusion de Ridge logit CV k-fold 
cm_ridge <- confusionMatrix(data=testing$p_Ridge_logit_bayes, 
                         reference=testing$Pobre , 
                         mode="sens_spec" , positive="Pobre")
cm_ridge$table
s6 <- cm_ridge[["byClass"]][["Sensitivity"]] # sensibilidad con Ridge logit CV k-fold


## Ridge-logit CV k-fold con Thresh 
testing <- testing %>% 
  mutate(p_Ridge_logit_th=ifelse(p_Ridge_logit>rfThresh_fin[3,1],1,0) %>% 
           factor(.,levels=c(1,0),labels=c("Pobre","No_Pobre")))

## Matriz de confusion de Ridge logit CV k-fold con Thresh
cm_ridge_th <- confusionMatrix(data=testing$p_Ridge_logit_th, 
                            reference=testing$Pobre , 
                            mode="sens_spec" , positive="Pobre")
cm_ridge_th$table
s7 <- cm_ridge_th[["byClass"]][["Sensitivity"]] # Sensibilidad con Ridge logit CV k-fold con Thresh


# Elastic Net
testing <- testing %>% 
  mutate(p_Elasticnet_bayes=ifelse(p_Elasticnet>0.5,1,0) %>% 
           factor(.,levels=c(1,0),labels=c("Pobre","No_Pobre")))

## Matriz de confusion de Elastic Net
cm_EN <- confusionMatrix(data=testing$p_Elasticnet_bayes, 
                         reference=testing$Pobre , 
                         mode="sens_spec" , positive="Pobre")
cm_EN$table
s8 <- cm_EN[["byClass"]][["Sensitivity"]] # Sensibilidad con Elastic Net


# Elastic Net con thresh
testing <- testing %>% 
  mutate(p_Elasticnet_th=ifelse(p_Elasticnet>rfThresh_fin[1,1],1,0) %>% 
           factor(.,levels=c(1,0),labels=c("Pobre","No_Pobre")))

## Matriz de confusion de Elastic Net con Thresh
cm_EN_th <- confusionMatrix(data=testing$p_Elasticnet_th, 
                         reference=testing$Pobre , 
                         mode="sens_spec" , positive="Pobre")
cm_EN_th$table
s9 <- cm_EN_th[["byClass"]][["Sensitivity"]] # sensibilidad con Elastic Net con thresh


# Random Forest
testing <- testing %>% 
  mutate(pr_forest_bayes=ifelse(pr_forest>0.5,1,0) %>% 
           factor(.,levels=c(1,0),labels=c("Pobre","No_Pobre")))

## Matriz de confusion de Random Forest
cm_rf <- confusionMatrix(data=testing$pr_forest_bayes, 
                reference=testing$Pobre , 
                mode="sens_spec" , positive="Pobre")
cm_rf$table
s10 <- cm_rf[["byClass"]][["Sensitivity"]] # sensibilidad con Random Forest


# Random Forest con Thresh
testing <- testing %>% 
  mutate(pr_forest_th=ifelse(pr_forest>rfThresh_fin[4,1],1,0) %>% 
           factor(.,levels=c(1,0),labels=c("Pobre","No_Pobre")))

## Matriz de confusion de Random Forest con Thresh
cm_rf_th <- confusionMatrix(data=testing$pr_forest_th, 
                reference=testing$Pobre , 
                mode="sens_spec" , positive="Pobre")

cm_rf_th$table
s11 <-  cm_rf_th[["byClass"]][["Sensitivity"]] # sensibilidad con Random Forest con Thresh
s11

##Matriz de confusión Random Forest
cm_rf_th$table

#tabla de sensibilidades y AUC
vector_mod <- c("Logit","Logit-cv","Lasso_logit","Lasso_logit_Thres","Ridge_logit","Ridge_logit_Thres","Elastic_Net","Elastic_Net_thresh","Random_Forest","Random_Forest_Thresh")
tabla_sens <- data.frame(Modelo = vector_mod, Sensibility = rbind(s2,s3,s4,s5,s6,s7,s8,s9,s10,s11))
tabla_sens <- rbind(tabla_sens, KNN_sensibility) %>% cbind(c("AUC"))
tabla_sens[2,3]<-auc_roc@y.values[[1]] 
tabla_sens[3,3]<-auc[1,1]
tabla_sens[5,3]<-auc[2,1]
tabla_sens[7,3]<-auc[3,1]
tabla_sens[9,3]<-auc[4,1]
tabla_sens




##  Entrenamiento del mejor modelo de clasificación-------------------------------------------


best_model_clasi <- as.formula("Pobre~factor(actividad_jef)+factor(niv_educ_jef)+personas_x_Ug+factor(Ocupacion_vivienda)+num_cuartos_exclus_hog+ContEspec+
      Subsidiado+prop_si_cotiza+prop_no_cotiza+Pensionado+factor(Depto)+edad_media+prop_hombre+prop_mujer")

#mejor modelo: random forest
myforest <- readRDS("Data/modelo_forest.rds") ## se trae el modelo RF para predecir

colnames(df_test_hog)
## Predicción modelo Randon forest

df_test_hog_final <- df_test_hog_final[(df_test_hog_final$id!="7f3338d5ae1e7b783617ff44"),] %>% mutate(Pobre=1)
nrow(df_test_hog_final)
ncol(df_test_hog_final)

df_test_hog_final$pred_rf_best <- predict(myforest, df_test_hog_final, type="prob")[1]


BASE_PREDICT <- df_test_hog_final[,c("id","pred_rf_best")] %>% mutate(Pobre_classification=ifelse(pred_rf_best>rfThresh_fin[4,1],1,0) %>% 
                                                             factor(.,levels=c(1,0),labels=c("Pobre","No_Pobre")))


### CLASIFICACIÓN DE POBRES predicha
prop.table(table(BASE_PREDICT$Pobre_classification))



## MODELO DE PREDICCIÓN DE INGRESOS---------------------------------------------------------------------------------
## Estrutura del modelo de ingreso 
var_mod_ingre <- c("id","Ingtot_log","edad","edad_sqr","sexo","niv_educativo","actividad","tiempo_empresa", "tiempo_empresa_sqr","ocupacion_empleo", "ReciAyudaInst", "Ocupacion_vivienda", "cotiza_pension", "HorasTrabSemana","mujer")

## Se corren 11 modelos para así encontrar el que mejor se ajuste por MSE meidante CV K-FOLD

modelos <- list(Ingtot_log~edad+edad_sqr,
                Ingtot_log~edad+edad_sqr+tiempo_empresa+tiempo_empresa_sqr,
                Ingtot_log~edad+edad_sqr+tiempo_empresa+tiempo_empresa_sqr+mujer,
                Ingtot_log~edad+edad_sqr+tiempo_empresa+tiempo_empresa_sqr+mujer+mujer:niv_educativo,
                Ingtot_log~edad+edad_sqr+tiempo_empresa+tiempo_empresa_sqr+mujer+niv_educativo+mujer:niv_educativo,
                Ingtot_log~edad+edad_sqr+tiempo_empresa+tiempo_empresa_sqr+mujer+niv_educativo+mujer:niv_educativo+factor(actividad),
                Ingtot_log~edad+edad_sqr+tiempo_empresa+tiempo_empresa_sqr+mujer+niv_educativo+mujer:niv_educativo+factor(actividad)+factor(ocupacion_empleo),
                Ingtot_log~edad+edad_sqr+tiempo_empresa+tiempo_empresa_sqr+mujer+niv_educativo+mujer:niv_educativo+factor(actividad)+factor(ocupacion_empleo)+ReciAyudaInst,
                Ingtot_log~edad+edad_sqr+tiempo_empresa+tiempo_empresa_sqr+mujer+niv_educativo+mujer:niv_educativo+factor(actividad)+factor(ocupacion_empleo)+ReciAyudaInst+factor(Ocupacion_vivienda),
                Ingtot_log~edad+edad_sqr+tiempo_empresa+tiempo_empresa_sqr+mujer+niv_educativo+mujer:niv_educativo+factor(actividad)+factor(ocupacion_empleo)+ReciAyudaInst+factor(Ocupacion_vivienda)+factor(cotiza_pension))

## Cálculo de MSE por CV

## definición de semilla 
set.seed(777)

## Loop para entrenamiento de modelos de ingreso
MSE_CV <- c()
for (i in modelos){
  model<-train(i,
               data = df_pers_ing_1[,var_mod_ingre],
               trControl = trainControl(method = "cv", number = 5),
               method = "null") # specifying regression model
  
  mse_cv <- model[["results"]][["RMSE"]]
  MSE_CV<-c(MSE_CV,mse_cv)
}

MSE_CV

ScaleXmod<-c('m1', 'm2', 'm3' ,'m4', 'm5','m6','m7','m8','m9','m10')
grafica_mse <- data.frame(modelos=c(1,2,3,4,5,6,7,8,9,10),
                          MSE=MSE_CV[1:10])
rownames(grafica_mse)<-c(1,2,3,4,5,6,7,8,9,10)

## gráfica de los MSE de los modelos entrenados
ggplot(grafica_mse, aes(x=modelos,y=MSE, group = 1)) + 
  geom_line() +
  theme_classic() + 
  scale_x_continuous(breaks = c(1:10),
                     labels= ScaleXmod)



##  Entrenamiento del mejor modelo de ingreso------------------------------------

best_model <- as.formula("Ingtot_log~edad+edad_sqr+tiempo_empresa+tiempo_empresa_sqr+mujer+niv_educativo+mujer:niv_educativo+factor(actividad)+factor(ocupacion_empleo)+ReciAyudaInst+factor(Ocupacion_vivienda)")

set.seed(777)

model_m9<-train(best_model,
                data = df_pers_ing_1[,var_mod_ingre],
                trControl = trainControl(method = "cv", number = 5),
                method = "null") # specifying regression model


## Predicción final modelo m11 

predict <- stats::predict

#Ejecutar modelo
model_m9 <- lm(best_model, df_pers_ing_1[,var_mod_ingre])

##Guardar predichos
df_test_per$pred_ing <- exp(predict(model_m9, df_pers_ing_test))

df_test_hog_fin <- df_test_per[,c("id","pred_ing")] %>%  group_by(id) %>%
  summarise(pred_ingreso_fin = sum(pred_ing)) 

df_test_hog <- df_test_hog %>% inner_join(df_test_hog_fin, by="id") %>%
  mutate(Pobre_income=ifelse(pred_ingreso_fin<Lp*personas_x_Ug,
                      1,
                      0)) %>%
  mutate(Pobre_income=factor(Pobre_income, level=c(1,0), labels=c("Pobre", "No_Pobre")))

df_test_hog_final_ing <- df_test_hog[,c("id","Pobre_income")]

#colnames(df_test_hog_final_ing) <- c("id","Pobre_income")

### cLASIFICACIÓN DE POBRES predicha por ingresos
prop.table(table(df_test_hog_final_ing$Pobre_income))


## base final de predicciones
BASE_PREDICT_FIN <- BASE_PREDICT[,c("id","Pobre_classification")] %>% inner_join(df_test_hog_final_ing, by="id") 
prop.table(table(BASE_PREDICT_FIN$Pobre_classification))
prop.table(table(BASE_PREDICT_FIN$Pobre_income))

saveRDS(BASE_PREDICT_FIN,"Data/Base_predicciones.rds")
export(BASE_PREDICT_FIN,"Data/Base_predicciones.csv")
