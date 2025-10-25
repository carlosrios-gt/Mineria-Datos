# Carga de librerias
library(readxl)
library(arules)
library(ggplot2)
library(dplyr)

# Carga de datos
data <- read_excel("D:\\Documents\\USAC\\MIICC\\MIICC408 - Introducción a la Minería de Datos\\03 K-Means\\base-de-datos-violencia-intrafamiliar-ano-2024_v3.xlsx")

# Creando Data Frame
data_fp_CR <- data[, c("VIC_EDAD", "VIC_ESCOLARIDAD", "VIC_TRABAJA", "VIC_REL_AGR" , "VIC_OCUP" , "TOTAL_HIJOS")]
data_fp_gr <- data[, c("VIC_EDAD", "VIC_ESCOLARIDAD", "HEC_DEPTO", "VIC_TRABAJA", "AGR_EDAD", "AGR_ESCOLARIDAD" )]
data_fp_guatemala <- subset(data_fp_gr, HEC_DEPTO==1)

# Revisando estado de los datos
lapply(data_fp_CR, function(x) sort(unique(x)))

# Revisando la integridad TOTAL_HIJOS
table(data_fp_CR$TOTAL_HIJOS)
data_fp_CR2 = data_fp_CR %>% 
  filter(TOTAL_HIJOS != 99)

# Revisando la integridad VIC_TRABAJA
table(data_fp_CR$VIC_TRABAJA)
data_fp_CR3 <- data_fp_CR2 %>%
  filter(VIC_TRABAJA != 9)

# Asignando valor numerico a los datos nulos
data_fp_CR3[is.na(data_fp_CR3)] <- -1
data_fp_guatemala[is.na(data_fp_guatemala)] <- -1 

# Creando los cálculos k-Means
cluster <- kmeans(data_fp_CR3, centers = 3)
cluster_gt <- kmeans(data_fp_guatemala, center = 3)

# VICTIMA EDAD vs AGRESOR EDAD
# Trazando los gráficos
ggplot(data_fp_guatemala, aes(x = VIC_EDAD, y = AGR_EDAD, 
  color =as.factor(cluster_gt$cluster)))+
  geom_point()+
  geom_point(data = as.data.frame(cluster_gt$centers), 
  aes(x = VIC_EDAD, y = AGR_EDAD), color="black", 
  size=4, shape=17)+
  labs(title = "Edad de la Victima vs Edad del Agresor") +
  theme_minimal()

# VIC EDAD vs TOTAL HIJOS
# Trazando los gráficos
ggplot(data_fp_CR3, aes(x = VIC_EDAD, y = TOTAL_HIJOS, 
  color =as.factor(cluster$cluster)))+
  geom_point()+
  geom_point(data = as.data.frame(cluster$centers), 
             aes(x = VIC_EDAD, y = TOTAL_HIJOS), color="black", 
             size=4, shape=17)+
  labs(title = "Edad vs Cantidad Hijos") +
  theme_minimal()

# VIC EDAD vs TRABAJA
# Trazando los gráficos
ggplot(data_fp_CR3, aes(x = VIC_TRABAJA, y = VIC_EDAD, 
                        color =as.factor(cluster$cluster)))+
  geom_point()+
  geom_point(data = as.data.frame(cluster$centers), 
             aes(x = VIC_TRABAJA, y = VIC_EDAD), color="black", 
             size=4, shape=17)+
  labs(title = "Trabaja vs Edad") +
  theme_minimal()
