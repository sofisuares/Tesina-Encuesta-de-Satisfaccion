# Última actualización: 14/05/2024
# Autora: Sofía Suares

#-------------------------------------------------- Descripcion -------------------------------------------------- #
# Este programa se utiliza para realizar el análisis descriptivo de los grupos familiares que conforman la         #
# población objetivo en la Encuesta de Satisfacción de Asociados Anual de la empresa Avalian.                      #
# Las variables consideradas son:                                                                                  #
# Número de integrantes en el grupo familiar                                                                       #
# Credencial                                                                                                       #
# Segmento: con cinco y dos categorías                                                                             #
# Región                                                                                                           #
# Nivel de Uso                                                                                                     #
# Se realizan gráficos de bastones (número de integrantes), gráficos de donas (credencial y segmento) y gráficos   #
# de barras (región y nivel de uso).                                                                               #
#----------------------------------------------------------------------------------------------------------------- #

# Carga de librerías
library(readxl) #Para cargar datos
library(ggplot2) #Para gráficos
library(dplyr)
library(scales) #Para gráfico de torta

# Configuración de Working Directory
setwd("C:/Users/ssuares/Desktop/Encuestas/Encuesta de satisfacción/Descriptivo/Descriptivo 25-10")

# Definir colores personalizados paleta Avalian
colores <- c("#00986b","darkgray", "#96D966", "#0D0D0D", "#93D9A4")

### Variable: número de integrantes en el GF
#Carga de datos
Integrantes_GF <- read_excel("Para armar el descriptivo.xlsx", 
                       sheet = "IntegrantesR")

#Diagrama de Bastones para el % de GF
ggplot(Integrantes_GF) +
  aes(x = Integrantes, y = Porcentaje) +
  geom_bar(stat = "identity", width = 0.08, col = "#00986b", fill = "#00986b") +
  scale_x_continuous(breaks = 0:15) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.6),
                     breaks = seq(0, 0.6, by = 0.05), expand = expansion(mult = c(0, 0.05))) +
  xlab("Número de integrantes en el GF") +
  ylab("Porcentaje de GF") +
  labs(title="Figura 1. Distribución de GF según el número de integrantes en el GF al 25/10/2023")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, color = "#434343"),
        axis.title.x = element_text(color = "#434343"),
        axis.title.y = element_text(color = "#434343")) 


### Variable: Credencial
# Carga de datos
Credencial_GF <- read_excel("Para armar el descriptivo.xlsx", 
                             sheet = "CredencialR")

# Gráfico de dona
ggplot(Credencial_GF, aes(x=2,y=Porcentaje, fill=Credencial, label = paste0(round(Porcentaje), "%")))+
  geom_bar(stat = "identity",color="white", width = 1)+
  ##geom_text(aes(label=percent(Porcentaje)),
    ##        position=position_stack(vjust=0.5),color="white",size=5)+
  geom_text(aes(label = paste0(format(percent(Porcentaje),big.mark=".",decimal.mark = ","))), 
            position = position_stack(vjust = 0.5),color="white",size=4)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=colores)+
  theme_void()+
  labs(title="Figura 2. Distribución de GF según credencial al 25/10/2023")+
  xlim(0.5,2.5)+
  theme(legend.position = "right",
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#434343")) 


### Variable: segmento (5 Categorías)
Segmento_GF <- read_excel("Para armar el descriptivo.xlsx", 
                          sheet = "SegmentoR")

# Gráfico de dona
ggplot(Segmento_GF, aes(x=2,y=Porcentaje, fill=Segmento, label = paste0(round(Porcentaje), "%")))+
  geom_bar(stat = "identity",color="white", width = 1)+
  geom_text(aes(label = paste0(format(percent(Porcentaje),big.mark=".",decimal.mark = ","))), 
            position = position_stack(vjust = 0.5),color="white",size=3.5)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=colores)+
  theme_void()+
  labs(title="Figura 3. Distribución de GF según segmento (5 categorías) al 25/10/2023")+
  xlim(0.5,2.5)+
  theme(legend.position = "right",
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#434343")) 


### Variable: segmento (2 Categorías)
Segmento_GF_2 <- read_excel("Para armar el descriptivo.xlsx", 
                          sheet = "SegmentoEncuestaR")

# Gráfico de dona
ggplot(Segmento_GF_2, aes(x=2,y=Porcentaje, fill=Segmento, label = paste0(round(Porcentaje), "%")))+
  geom_bar(stat = "identity",color="white", width = 1)+
  geom_text(aes(label = paste0(format(percent(Porcentaje),big.mark=".",decimal.mark = ","))), 
            position = position_stack(vjust = 0.5),color="white",size=3.5)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=colores)+
  theme_void()+
  labs(title="Figura 4. Distribución de GF según segmento (2 categorías) al 25/10/2023")+
  xlim(0.5,2.5)+
  theme(legend.position = "right",
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#434343")) 


### Variable: región
Regiones_GF <- read_excel("Para armar el descriptivo.xlsx", 
                          sheet = "RegionesR")

ggplot(Regiones_GF) +
  aes(x = reorder(Regiones, Porcentaje), y = Porcentaje, label = scales::percent(Porcentaje)) +
  geom_bar(stat = "identity", width = 0.75, col = "#00986b", fill = "#00986b") +
  geom_text(position = position_dodge(width = 0.75),hjust=-0.1, vjust = 0.5, size = 3, color = "#434343") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.4),
                     breaks = seq(0, 0.4, by = 0.05), expand = expansion(mult = c(0, 0.05))) +
  xlab("Regiones") +
  ylab("Porcentaje de GF") +
  coord_flip() +
  labs(title = "Figura 5. Distribución de GF según región al 25/10/2023") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, color = "#434343"),
        axis.title.x = element_text(color = "#434343"),
        axis.title.y = element_text(color = "#434343"))


### Variable: nivel de uso
NivelUso_GF <- read_excel("Para armar el descriptivo.xlsx", 
                          sheet = "GastoR")

NivelUso_GF$Gasto <- factor(NivelUso_GF$Gasto, levels = c("Bajo", "Medio", "Alto"))

# Gráfico
ggplot(NivelUso_GF) +
  aes(x = as.factor(Gasto), y = Porcentaje, label = scales::percent(Porcentaje)) +
  geom_bar(stat = "identity", width = 0.75, col = "#00986b", fill = "#00986b") +
  geom_text(position = position_dodge(width = 0.75), hjust = 0.5, vjust = -0.5, size = 3, color = "#434343") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.6),
                     breaks = seq(0, 0.6, by = 0.05), expand = expansion(mult = c(0, 0.05))) +
  xlab("Nivel de uso") +
  ylab("Porcentaje de GF") +
  labs(title = "Figura 6. Distribución de GF según el nivel de uso al 25/10/2023") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, color = "#434343"),
        axis.title.x = element_text(color = "#434343"),
        axis.title.y = element_text(color = "#434343"))

