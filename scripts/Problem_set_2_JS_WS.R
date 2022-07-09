#Autores: Juan Sebastián Vásquez Acevedo y Walter Leonardo Sánchez
#Curso: Big Data and Machine Learning
#PROBLEM SET 2
#Creación: 27/06/2022

# Limpiar el espacio de trabajo y definir el directorio -----------------------------------------------------
rm(list=ls())
cat("\014")
setwd("C:/Users/walte/OneDrive/Documentos/Maestría en Economía Aplicada/Big Data/GitHub/Talleres/Problem_set_2_WS_JSV")
dir()

# Cargar librerías -----------------------------------------------------------
require(pacman)
p_load(tidyverse,dplyr,here, rvest, tibble,rio,skimr,caret,stargazer)


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
df_train_hog <- read_rds("Datatrain_hogare.rds")
df_train_per <- read_rds("Data/train_personas.rds")


# Revisión y cruce de bases de datos --------------------------------------


names_df_tes_h <- colnames(df_test_hog)
names_df_tes_p <- colnames(df_test_per)
names_df_tai_h <- colnames(df_train_hog)
names_df_tai_p <- colnames(df_train_per)

names_df_tai_h
names_df_tes_p
names_df_tai_h
names_df_tai_p



