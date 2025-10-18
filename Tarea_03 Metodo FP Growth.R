
install.packages("arules")
library(arules)

install.packages("pkgbuild")
pkgbuild::has_build_tools(debug = TRUE)
system("where make")

install.packages(c("Rcpp", "RcppArmadillo", "remotes"))

Sys.setenv(MAKEFLAGS = "-j4")  # opcional: compilar en paralelo
remotes::install_github("mhahsler/fim4r",
                        build_vignettes = FALSE,
                        upgrade = "never",
                        INSTALL_opts = "--no-multiarch")

# Cargando la libreria
library(arules)
library(fim4r)

data("Groceries", package = "arules")

# Reglas con FPGrowth vía fim4r
rules <- fim4r(Groceries,
               method = "fpgrowth",
               target = "rules",
               supp = 0.01,
               conf = 0.5)

inspect(head(rules, 5))

# 
library(readxl)
install.packages("D:\\Documents\\USAC\\MIICC\\MIICC408 - Introducción a la Minería de Datos\\02 FP Growth\\fim4r_1.8.tar.gz")

data <- read_excel(violencia-intrafamiliar.xlsx)

data_fp <- data[,c("HEC_MES",)]
reglas_fp <- firm4r(data, method="fpgrowth", target="rules", sipp=.2, conf=.5)

# Instalacion de Libreria
library(arules)
?fim4r  # ver ayuda
data("Groceries")
rules <- fim4r(Groceries, method = "fpgrowth", supp = 0.01, conf = 0.5)
inspect(head(rules, 5))


install.packages("readxl")
library(readxl)
pkgbuild::find_rtools()

install.packages("pkgbuild")
library(pkgbuild)
pkgbuild::find_rtools()

install.packages("arules")
library(arules)

install.packages("readxl")
library(readxl)

library(fim4r)

# Tarea 3
# Algoritmo FP Growth

datos <- read.csv("D:\\Documents\\USAC\\MIICC\\MIICC408 - Introducción a la Minería de Datos\\02 FP Growth\\base_datos_violencia_intrafamiliar.csv", encoding = "UTF-8")

# Creando el Data Frame
data_fp_CR <- datos[, c("VIC_EDAD", "VIC_ESCOLARIDAD", "VIC_TRABAJA", "VIC_REL_AGR" , "VIC_OCUP" , "TOTAL_HIJOS")]

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

# Creacion de las reglas
reglas_fp <- fim4r(data_fp_CR3, method="fpgrowth", target ="rules", supp =.2, conf=.5)
rf <- as(reglas_fp, "data.frame")
