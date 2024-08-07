# Última actualización: 02/07/2024
# Autora: Sofía Suares

#-------------------------------------------------- Descripción -------------------------------------------------- #
# Este programa se utiliza para realizar el análisis descriptivo de los grupos familiares que conforman la         #
# población objetivo en la Encuesta de Satisfacción de Asociados anual de la empresa Avalian.                      #
# Las variables consideradas son:                                                                                  #
# - Número de integrantes en el grupo familiar                                                                     #
# - Credencial                                                                                                     #
# - Segmento: con cinco y dos categorías                                                                           #
# - Región                                                                                                         #
# - Nivel de Uso                                                                                                   #
# Se realizan gráficos de bastones (número de integrantes), gráficos de sectores (credencial y segmento) y gráficos#
# de barras (región y nivel de uso).                                                                               #
#----------------------------------------------------------------------------------------------------------------- #

# Carga de librerías
library(readxl) # Para cargar datos desde archivos de Excel
library(ggplot2) # Para crear gráficos y visualizaciones
library(dplyr) # Para manipulación y transformación de datos
library(scales) # Para formateo de ejes y etiquetas en los gráficos de torta

# Configuración de Working Directory
setwd("C:/Users/ssuares/Desktop/Encuestas/Encuesta de satisfacción/Descriptivo/Descriptivo 25-10")

# Definir colores personalizados paleta Avalian
colores <- c("#00986b","darkgray", "#96D966", "#0D0D0D", "#93D9A4")

#------------------------------- Variable: número de integrantes en el GF -------------------------------------- #
# Carga de datos
Integrantes_GF <- read_excel("Para armar el descriptivo.xlsx", 
                             sheet = "IntegrantesR")

# Diagrama de Bastones para el % de GF
ggplot(Integrantes_GF) +
  aes(x = as.factor(Integrantes), y = Porcentaje) +
  geom_bar(stat = "identity", width = 0.08, col = "#00986b", fill = "#00986b") +
  #scale_x_discrete(breaks = 0:15) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.6),
                     breaks = seq(0, 0.6, by = 0.05), expand = expansion(mult = c(0, 0.05))) +
  xlab("Número de integrantes en el GF") +
  ylab("Porcentaje de GF") +
  theme_classic()  

#-------------------------------------- Variable: Credencial ------------------------------------------------- # 
# Carga de datos
Credencial_GF <- read_excel("Para armar el descriptivo.xlsx", 
                            sheet = "CredencialR")

# Gráfico de sectores
ggplot(Credencial_GF, aes(x="",y=Porcentaje, fill=Credencial, label = paste0(format(percent(Porcentaje), big.mark = ".", decimal.mark = ","))))+
  geom_bar(stat = "identity",color="white", width = 1)+
  geom_text(aes(label = paste0(format(percent(Porcentaje),big.mark=".",decimal.mark = ","))), 
            position = position_stack(vjust = 0.5),color="white",size=3)+
  coord_polar(theta = "y", direction=-1)+
  scale_fill_manual(values=colores)+
  theme_void()+
  theme(legend.position = "right",
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#434343"))

#--------------------------------- Variable: Segmento 5 categorías ------------------------------------------- # 
# Carga de datos
Segmento_GF <- read_excel("Para armar el descriptivo.xlsx", 
                          sheet = "SegmentoR")

# Gráfico de sectores
ggplot(Segmento_GF, aes(x="",y=Porcentaje, fill=Segmento, label = paste0(round(Porcentaje), "%")))+
  geom_bar(stat = "identity",color="white", width = 1)+
  geom_text(aes(label = paste0(format(percent(Porcentaje),big.mark=".",decimal.mark = ","))), 
            position = position_stack(vjust = 0.5),color="white",size=3)+
  coord_polar(theta = "y",direction=-1)+
  scale_fill_manual(values=colores)+
  theme_void()+
  # labs(title="Figura 3. Distribución de GF según segmento (5 categorías) al 25/10/2023")+
  theme(legend.position = "right",
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#434343")) 


#--------------------------------- Variable: Segmento 2 categorías ------------------------------------------- # 
# Carga de datos
Segmento_GF_2 <- read_excel("Para armar el descriptivo.xlsx", 
                            sheet = "SegmentoEncuestaR")

# Gráfico de sectores
ggplot(Segmento_GF_2, aes(x="", y=Porcentaje, fill=Segmento, label = paste0(round(Porcentaje * 100, 1), "%"))) +
  geom_bar(stat = "identity", color="white", width = 1) +
  geom_text(position = position_stack(vjust = 0.5), color="white", size=3.5) +
  coord_polar(theta = "y", direction=-1) +
  scale_fill_manual(values=colores) +
  theme_void() +
  theme(legend.position = "right",
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#434343"))
#--------------------------------------- Variable: Región --------------------------------------------------- # 
# Carga de datos
Regiones_GF <- read_excel("Para armar el descriptivo.xlsx", 
                          sheet = "RegionesR")

# Gráfico de barras
ggplot(Regiones_GF) +
  aes(x = reorder(Regiones, Porcentaje), y = Porcentaje, label = scales::percent(Porcentaje, accuracy = 0.1)) +
  geom_bar(stat = "identity", width = 0.75, col = "#00986b", fill = "#00986b") +
  geom_text(position = position_dodge(width = 0.75), hjust = -0.1, vjust = 0.5, size = 3, color = "#434343") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.4),
                     breaks = seq(0, 0.4, by = 0.05), expand = expansion(mult = c(0, 0.05))) +
  xlab("Regiones") +
  ylab("Porcentaje de GF") +
  coord_flip() +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, color = "#434343"),
        axis.title.x = element_text(color = "#434343"),
        axis.title.y = element_text(color = "#434343"))

#------------------------------------ Variable: Nivel de uso ------------------------------------------- # 
# Carga de datos
NivelUso_GF <- read_excel("Para armar el descriptivo.xlsx", 
                          sheet = "GastoR")

# Establecer niveles Bajo, Medio, Alto del nivel de uso
NivelUso_GF$Gasto <- factor(NivelUso_GF$Gasto, levels = c("Bajo", "Medio", "Alto"))

# Gráfico de barras
ggplot(NivelUso_GF) +
  aes(x = as.factor(Gasto), y = Porcentaje, label = scales::percent(Porcentaje, accuracy = 0.1)) +
  geom_bar(stat = "identity", width = 0.75, col = "#00986b", fill = "#00986b") +
  geom_text(position = position_dodge(width = 0.75), hjust = 0.5, vjust = -0.5, size = 3, color = "#434343") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.6),
                     breaks = seq(0, 0.6, by = 0.05), expand = expansion(mult = c(0, 0.05))) +
  xlab("Nivel de uso") +
  ylab("Porcentaje de GF") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, color = "#434343"),
        axis.title.x = element_text(color = "#434343"),
        axis.title.y = element_text(color = "#434343"))
