install.packages("arules")
library(arules)
install.packages("readxl")
library(readxl)

datos <- read_excel("C:\\Users\\kevin\\Downloads\\graduados-superior-2023.xlsx")

reglas <- apriori(datos, parameter = list(support=0.2, confidence = 0.5)) 

inspect(reglas[0:130])

data.frame(1:ncol(datos), colnames(datos))
datos <- datos[, -1]
reglas_2 <- apriori(datos, parameter = list(support=0.2, confidence = 0.5)) 
inspect(reglas_2[0:44])


datos_tecnico <- subset(datos, Nivel_Educativo == "Técnico")
data.frame(1:ncol(datos_tecnico), colnames(datos))
datos_tecnico <- datos_tecnico[, -3]
reglas_3 <- apriori(datos_tecnico, parameter = list(support=0.2, confidence = 0.5)) 
inspect(reglas_3[0:25])

datos_licenciatura <- subset(datos, Nivel_Educativo == "Licenciatura")
data.frame(1:ncol(datos_licenciatura), colnames(datos_licenciatura))
datos_licenciatura <- datos_licenciatura[, -7]
reglas_4 <- apriori(datos_licenciatura, parameter = list(support=0.2, confidence = 0.4)) 
inspect(reglas_4[0:16])


datos_meastria <- subset(datos, Nivel_Educativo == "Maestría")
data.frame(1:ncol(datos_meastria), colnames(datos_meastria))
datos_meastria <- datos_meastria[, -7]
reglas_5 <- apriori(datos_meastria, parameter = list(support=0.2, confidence = 0.5)) 
inspect(reglas_5[0:36])


datos_doctorado <- subset(datos, Nivel_Educativo == "Doctorado")
data.frame(1:ncol(datos_doctorado), colnames(datos_doctorado))
datos_doctorado <- datos_doctorado[, -3]
reglas_6 <- apriori(datos_doctorado, parameter = list(support=0.15, confidence = 0.5)) 
inspect(reglas_6[0:150])