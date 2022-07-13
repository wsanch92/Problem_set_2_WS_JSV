# Problem_set_2_WS_JSV
#Autores: Juan Sebastian Vásquez y Walter Leonardo Sánchez
#Fecha de creación: 28/06/2022

## PROBLEM SET

# Paquetes necesarios:
	#pacman: este paquete contiene una funcionalidad llamada 'p_load', que permite llamar las librerías que requerimos sino, intenta instalarlas en la medida de lo posible.
	#tidyverse
	#rvest
	#tibble
	#skimr
	#caret
	#stargazer
	#boot
	#gtsummary
	#skimr
	#reshape2
	#class
	#ROCR
	#pROC
	#randomForest

# Objetivos: 
		1) Predecir la clasificación de pobreza monetaria mediante modelos de Clasificación y de regresión con ingresos

# El Script:
	Se construye un script en R que estructura las bases necesarias para el entrenamiento, testeo y evaluación de los modelos y sus métricas, de tal manera que se encuentre el mejor modelo para estimar la pobreza en la Gran Encuesta Integrada de Hogares - GEIH del año 2018.
	Se masean los datos de la GEIH imputando valores en missing para algunas variables y en ceros para otras, como el ingreso. 

## Bases para modelos de Clasificación:
A partir de las bases de entrenamiento y testeo para personas y hogares se construyen bases alternas y finales donde se estimaran y ejecutaran los modelos seleccionado para la clasificación y/o predicción de pobreza.
Para la bases de entrenamiento de personas se crean variables de proporciones y por jefe del hogara para ser agregado a nivel de hogares. Esta se une a la base de hogares de entranamiento como insumo para los modelos de clasificación: "df_train_hog_final".
Se particiona la base de  hogares en tres partes: "training", "testing" y "evaluation". En la primera de éstas, se entrenas los modelos, en la segunda se predicen las probabilidades de ser pobre de los hogares y, en la tercera y última, se evaluan los puntos de corte óptimos para definir sí un hogar es pobre o no.

Luego de seleccionar el modelo se calculas las predicciones en "df_test_hog_final":

## Base para modelos de Regresión con Ingresos:

Se crean las variables y se imputan valores nulos en la base "df_pers_ing_1" y se estiman los modelos calculando el MSE mediante Cross Validation K-fold. 
El mejor modelo se utilza para predecir los ingresos en la base "df_pers_ing_test", que posteriormente se colapsa a nivel de hogar, sumando los ingresos predichos por persona después de imputaciones.
Finalmente se pegan las predicciones en la base "df_test_hog_final_ing" que se contratarán con la línea de pobreza y la cantidad de unidades de gasto. 

En sintesis, se entrenan los modelos, se prediccen las clasificaciones por probabilodiades y por ingresos, y se guardan en "BASE_PREDICT_FIN" como factores que referencian los pobres y no pobres

## Los modelos de clasificación Utilizados:
	K-Vecinos Cercanos (1, 5, 7, 10, 13 vecinos)
	Logit
	Logit CV K-fold
	Lasso CV K-fold
	Ridge CV K-fold
	Randon Forest CV K-fold

## Nota!!: 
	1) Tener en cuenta qué, en las siguientes líneas de código se debe cambiar la ruta por una ruta de directorio del equipo local : 11
	2) Para el entrenamiento modelo de Random Forest: este modelo toma uin tiempo de ejecución elevado por limitación de capacidad de procesamiento, por lo tanto, se comentan las líneas 651 a 671 de script.
	Si se desea entrenar el modelo, se debe descomentar estas líneas, de lo contrario se guarda un archivo .RDS con el modelo entrenado para su posterior uso en la predicción. El archivo tiene el 
	nombre  "modelo_forest.rds" y se ubica en la carpeta del repositorio: "/Data"

### Repositorio
El Problem Set 2, está compuesto por un repositorio que se compone de 4 carpetas con sus respectivos contenidos en las cuales se encontrará la información necesaria para replicar el documento, de la siguiente manera:

	# Data: Donde se guarda las base del Web Scraping para ser cargada posteriormente y el modelod de random forest.
	# Documentos: Donde se almacena el documento final y algunos archivos de interés.
	# Scripts: Ubicación del script de R llamado "Problem_set_2_JS_WS.R", que contiene toda la codificación para replicar los resultados, tablas y gráficos utilizados en la elaboración del documento.
	# Tablas_graficos: Ruta de almacenamiento de las gráficas y tablas de interés generadas por el script.
