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




