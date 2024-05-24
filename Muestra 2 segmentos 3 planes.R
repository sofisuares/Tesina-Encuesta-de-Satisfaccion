# Última actualización: 23/05/2024
# Autora: Sofía Suares

#-------------------------------------------------- Descripción -------------------------------------------------- #
# Este programa se utiliza para obtener el tamaño de la muestra en la Encuesta de Satisfacción de Asociados anual  #
# de Avalian, junto con el listado de GF seleccionados para responder la encuesta.                                 #
#                                                                                                                  #
# Se utiliza el paquete R2BEAT, por tratarse de un diseño muestral estratificado simple al azar de GF, con         #
# adjudicación de la muestra siguiendo un enfoque multivariado y multidominio.                                     #
# Se utiliza la librería sampling para obtener el listado de GF.                                                   #
#                                                                                                                  #
# Se calcula el tamaño de muestra aplicando la técnica de sobremuestreo, para contemplar la no respuesta.          #
#                                                                                                                  #   
#----------------------------------------------------------------------------------------------------------------- #

#--------------------------------------------------   Resumen   -------------------------------------------------- #
# Estimadores: proporción satisfechos, promotores y detractores (3 variables)                                      #
# Dominios de estimación:                                                                                          #
# - DOM1= PLAN (A - B - C)                                                                                         #
# - DOM2= NIVEL USO (Bajo - Medio - Alto)                                                                          #
# - DOM3= REGION (A - B - P - L - N - C)                                                                           #
# - DOM4= SEGMENTO (F - G)                                                                                         #
# - DOM5= GLOBAL (Total)                                                                                           #
# ---------------------------------------------------------------------------------------------------------------- #

# Carga de paquetes a usar
library(R2BEAT) #Para obtener tamaño de muestra en diseño estratificado multivariado y multidominio
library(readxl) #Para cargar datos de excel
library(sampling) #Para obtener listado de GF (usaremos función strata)

# Configuración de Working Directory
setwd("C:/Users/ssuares/Desktop/Encuestas/Encuesta de satisfacción/Muestra/Muestra al 25-10")

#-------------------------------------   Obtención tamaño de muestra   ------------------------------------------- #
# Carga de datos:

# STRATUM
STRATUM4 <- read_excel("STRATUM - 2 segmentos 3 planes.xlsx", 
                       sheet = "STRATUM")
STRATUM4 <- as.data.frame(STRATUM4) # necesario que sea dataframe

# CV: Distintas propuestas de precisión.
#PROPUESTA 1: 3% Global - 5% resto
cv1 <- as.data.frame(list(DOM=c("DOM1","DOM2","DOM3","DOM4","DOM5"),
                          CV1=c(0.05,0.05,0.05,0.05,0.03),    
                          CV2=c(0.05,0.05,0.05,0.05,0.03),
                          CV3=c(0.05,0.05,0.05,0.05,0.03)
))
allocation_1 <- beat.1st(stratif=STRATUM4, errors= cv1, minnumstrat=2, maxiter=200, maxiter1=25, epsilon=10^(-11))
allocation_1$sensitivity
sum(allocation_1$n) #5937

#PROPUESTA 2: 3% global - 5% region - 10% resto
cv2 <- as.data.frame(list(DOM=c("DOM1","DOM2","DOM3","DOM4","DOM5"),
                          CV1=c(0.1,0.1,0.05,0.1,0.03),    
                          CV2=c(0.1,0.1,0.05,0.1,0.03),
                          CV3=c(0.1,0.1,0.05,0.1,0.03)
))
allocation_2 <- beat.1st(stratif=STRATUM4, errors= cv2, minnumstrat=2, maxiter=200, maxiter1=25, epsilon=10^(-11))
allocation_2$sensitivity
sum(allocation_2$n) #5453

#PROPUESTA 3: 3% Global - 7% resto
cv3 <- as.data.frame(list(DOM=c("DOM1","DOM2","DOM3","DOM4","DOM5"),
                          CV1=c(0.07,0.07,0.07,0.07,0.03),    
                          CV2=c(0.07,0.07,0.07,0.07,0.03),
                          CV3=c(0.07,0.07,0.07,0.07,0.03)
))
allocation_3 <- beat.1st(stratif=STRATUM4, errors= cv3, minnumstrat=2, maxiter=200, maxiter1=25, epsilon=10^(-11))
allocation_3$sensitivity
sum(allocation_3$n) #3182

#PROPUESTA 4: 3% global - 7% region - 10% resto
cv4 <- as.data.frame(list(DOM=c("DOM1","DOM2","DOM3","DOM4","DOM5"),
                          CV1=c(0.1,0.1,0.07,0.1,0.03),    
                          CV2=c(0.1,0.1,0.07,0.1,0.03),
                          CV3=c(0.1,0.1,0.07,0.1,0.03)
))
allocation_4 <- beat.1st(stratif=STRATUM4, errors= cv4, minnumstrat=2, maxiter=200, maxiter1=25, epsilon=10^(-11))
allocation_4$sensitivity
sum(allocation_4$n) #2936

#PROPUESTA 5: 3% global - 7% region - 12% plan segmento nivel uso
cv5 <- as.data.frame(list(DOM=c("DOM1","DOM2","DOM3","DOM4","DOM5"),
                          CV1=c(0.12,0.12,0.07,0.12,0.03),    
                          CV2=c(0.12,0.12,0.07,0.12,0.03),
                          CV3=c(0.12,0.12,0.07,0.12,0.03)
))
allocation_5 <- beat.1st(stratif=STRATUM4, errors= cv5, minnumstrat=2, maxiter=200, maxiter1=25, epsilon=10^(-11))
allocation_5$sensitivity
sum(allocation_5$n) #2911

#PROPUESTA 6: 3% global - 10% region - 12% plan segmento nivel uso
cv6 <- as.data.frame(list(DOM=c("DOM1","DOM2","DOM3","DOM4","DOM5"),
                          CV1=c(0.12,0.12,0.1,0.12,0.03),    
                          CV2=c(0.12,0.12,0.1,0.12,0.03),
                          CV3=c(0.12,0.12,0.1,0.12,0.03)
))
allocation_6 <- beat.1st(stratif=STRATUM4, errors= cv6, minnumstrat=2, maxiter=200, maxiter1=25, epsilon=10^(-11))
allocation_6$sensitivity
sum(allocation_6$n)  #2602

#PROPUESTA 7: 5% global - 5% region - 10% plan segmento nivel de uso
cv7<- as.data.frame(list(DOM=c("DOM1","DOM2","DOM3","DOM4","DOM5"),
                         CV1=c(0.1,0.1,0.05,0.1,0.05),    
                         CV2=c(0.1,0.1,0.05,0.1,0.05),
                         CV3=c(0.1,0.1,0.05,0.1,0.05)
))
allocation_7 <- beat.1st(stratif=STRATUM4, errors= cv7, minnumstrat=2, maxiter=200, maxiter1=25, epsilon=10^(-11))
allocation_7$sensitivity
sum(allocation_7$n) #5453

#PROPUESTA 8: 5% Global - 7% resto
cv8 <- as.data.frame(list(DOM=c("DOM1","DOM2","DOM3","DOM4","DOM5"),
                          CV1=c(0.07,0.07,0.07,0.07,0.05),    
                          CV2=c(0.07,0.07,0.07,0.07,0.05),
                          CV3=c(0.07,0.07,0.07,0.07,0.05)
))
allocation_8 <- beat.1st(stratif=STRATUM4, errors= cv8, minnumstrat=2, maxiter=200, maxiter1=25, epsilon=10^(-11))
allocation_8$sensitivity
sum(allocation_8$n) #3177

#PROPUESTA 9: 5% Global - 7% región - 10% plan segmento nivel de uso
cv9<- as.data.frame(list(DOM=c("DOM1","DOM2","DOM3","DOM4","DOM5"),
                         CV1=c(0.1,0.1,0.07,0.1,0.05),    
                         CV2=c(0.1,0.1,0.07,0.1,0.05),
                         CV3=c(0.1,0.1,0.07,0.1,0.05)
))
allocation_9 <- beat.1st(stratif=STRATUM4, errors= cv9, minnumstrat=2, maxiter=200, maxiter1=25, epsilon=10^(-11))
allocation_9$sensitivity
sum(allocation_9$n) #2929

#PROPUESTA 10: 5% Global - 7% región - 12% plan segmento nivel de uso
cv10<- as.data.frame(list(DOM=c("DOM1","DOM2","DOM3","DOM4","DOM5"),
                          CV1=c(0.12,0.12,0.07,0.12,0.05),    
                          CV2=c(0.12,0.12,0.07,0.12,0.05),
                          CV3=c(0.12,0.12,0.07,0.12,0.05)
))
allocation_10 <- beat.1st(stratif=STRATUM4, errors= cv10, minnumstrat=2, maxiter=200, maxiter1=25, epsilon=10^(-11))
allocation_10$sensitivity
sum(allocation_10$n) #2904

#PROPUESTA 11: 5% Global - 10% resto
cv11 <- as.data.frame(list(DOM=c("DOM1","DOM2","DOM3","DOM4","DOM5"),
                           CV1=c(0.1,0.1,0.1,0.1,0.05),    
                           CV2=c(0.1,0.1,0.1,0.1,0.05),
                           CV3=c(0.1,0.1,0.1,0.1,0.05)
))
allocation_11 <- beat.1st(stratif=STRATUM4, errors= cv11, minnumstrat=2, maxiter=200, maxiter1=25, epsilon=10^(-11))
allocation_11$sensitivity
sum(allocation_11$n) #1.622

#PROPUESTA 12: 5% Global - 10% región  plan y segmento - 12% nivel de uso
cv12<- as.data.frame(list(DOM=c("DOM1","DOM2","DOM3","DOM4","DOM5"),
                          CV1=c(0.1,0.12,0.1,0.1,0.05),    
                          CV2=c(0.1,0.12,0.1,0.1,0.05),
                          CV3=c(0.1,0.12,0.1,0.1,0.05)
))
allocation_12 <- beat.1st(stratif=STRATUM4, errors= cv12, minnumstrat=2, maxiter=200, maxiter1=25, epsilon=10^(-11))
allocation_12$sensitivity
sum(allocation_12$n) #1.597

#PROPUESTA 13: 5% Global - 10% región - 12% plan y segmento, nivel de uso
cv13<- as.data.frame(list(DOM=c("DOM1","DOM2","DOM3","DOM4","DOM5"),
                          CV1=c(0.12,0.12,0.1,0.12,0.05),    
                          CV2=c(0.12,0.12,0.1,0.12,0.05),
                          CV3=c(0.12,0.12,0.1,0.12,0.05)
))
allocation_13 <- beat.1st(stratif=STRATUM4, errors= cv13, minnumstrat=2, maxiter=200, maxiter1=25, epsilon=10^(-11))
allocation_13$sensitivity
sum(allocation_13$n) #1527

#PROPUESTA 14: 5% Global - 12% resto
cv14 <- as.data.frame(list(DOM=c("DOM1","DOM2","DOM3","DOM4","DOM5"),
                           CV1=c(0.12,0.12,0.12,0.12,0.05),    
                           CV2=c(0.12,0.12,0.12,0.12,0.05),
                           CV3=c(0.12,0.12,0.12,0.12,0.05)
))
allocation_14 <- beat.1st(stratif=STRATUM4, errors= cv14, minnumstrat=2, maxiter=200, maxiter1=25, epsilon=10^(-11))
allocation_14$sensitivity
sum(allocation_14$n) #1173

#-------------------------------------   Adjudicación de la muestra   ------------------------------------------- #
# Data set STRATUM con columna n
STRATUM4b <- allocation_14$file_strata #agregar columna n a STRATUM con adjudicación

# Se calcula una muestra inflada suponiendo tasa de respuesta del 50%
# Creación de la nueva columna "ninflado" en STRATUM. Condición para que en los casos en los que N sea menor o 
# igual a 10, n inflado sea igual a n (por ser un censo)
STRATUM4b$ninflado <- ifelse(STRATUM4b$N <= STRATUM4b$n, STRATUM4b$n, STRATUM4b$n / 0.5)
sum(STRATUM4b$ninflado) #TOTAL MUESTRA: 2346 GF

#-------------------------------------   Obtener muestra de asociados   ------------------------------------------- #
# Carga del marco muestral 
Marco4 <- read_excel("STRATUM - 2 segmentos 3 planes.xlsx", 
                     sheet = "MM transformado")
Marco4 <- as.data.frame(Marco4) #N=87.148 GF

# Ordeno STRATUM y Marco según estratos para que coincida el nombre del estrato en Marco4 con el tamaño muestral en Stratum1b

#Ordeno STRATUM4b ascendente según estrato
STRATUM4b <- STRATUM4b[order(STRATUM4b$ESTRATOS),]

#Ordeno Marco4 ascendente según estrato
Marco4 <- Marco4[order(Marco4$ESTRATOS),]
Marco4$ESTRATOS <- factor(Marco4$ESTRATOS)

# Obtener muestra: listado GF
Muestra4 <- strata(Marco4, stratanames= c("ESTRATOS"),size=STRATUM4b$ninflado, method="srswor")
summary(Muestra4$ESTRATOS) #Controlar que haya el tamaño de muestra requerido en cada estrato
Muestra <- Marco4[Muestra4$ID_unit,]  #Para obtener el número de GF. De Marco1 me quedo con las filas que indica ID_unit

# Exportar muestra a excel
#install.packages("openxlsx")
library(openxlsx)
write.xlsx(Muestra, "Muestra2Segmentos3planes.xlsx")
