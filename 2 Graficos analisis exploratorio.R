# GRAFICOS DE ANALISIS EXPLORATORIO

# Paquetes
library(ggplot2)

# Parámetros
ruta_datos_limpios = "../Datos Limpios/"
csv_observaciones = "observaciones.csv"

# Cargar tabla de observaciones en data frame
df_observaciones = read.csv2(paste0(ruta_datos_limpios, csv_observaciones), stringsAsFactors = F)

# Gráfico de Ingresos versus Edad
g = ggplot(df_observaciones, aes(y = Ingresos, x = Edad))
g = g + geom_point(colour = "red")
g

# Gráfico de Ingresos versus Categoría
g = ggplot(df_observaciones, aes(y = Ingresos, x = Categoria))
g = g + geom_point(colour = "red")
g