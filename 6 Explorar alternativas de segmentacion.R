# EXPLORAR ALTERNATIVAS DE SEGMENTACION
# Usar los valores de k obtenidos en los ejercicios anteriores para evaluar su utilidad para el negocio.

# Paquetes
library(dplyr)

# Parámetros
ruta_datos_limpios = "../Datos Limpios/"
csv_observaciones_dummy = "observaciones_dummy.csv"
csv_observaciones = "observaciones.csv"
cols_df_cluster = c("Cluster", "Edad", "Ingresos", "Nivel_Ingresos", "EstadoCivil", "Categoria", "Calidad") # Orden de las columnas para mostrar al final

# Cargar tabla de observaciones en data frame
df_observaciones_dummy = read.csv2(paste0(ruta_datos_limpios, csv_observaciones_dummy), stringsAsFactors = F)
df_observaciones = read.csv2(paste0(ruta_datos_limpios, csv_observaciones), stringsAsFactors = F)

# Generar modelo con k-means y escribir columna Cluster
k = 6
set.seed(0)
modelo = kmeans(df_observaciones_dummy, centers = k)
df_observaciones$Cluster = modelo$cluster

# Generar modelo con clusterización jerárquica y escribir columna Cluster
set.seed(0)
d = dist(df_observaciones_dummy)
arbol = hclust(d)
df_observaciones$Cluster = cutree(arbol, k = 6)

# Resumir
df_cluster = group_by(df_observaciones, Cluster, Categoria, EstadoCivil) %>% summarise(mean(Ingresos), mean(Edad))
names(df_cluster)[4] = "Ingresos"
names(df_cluster)[5] = "Edad"

# Crear nuevas columnas para mejor interpretación
cuartiles_ingresos = quantile(df_cluster$Ingresos)
df_cluster$Nivel_Ingresos = "Medio"
df_cluster[df_cluster$Ingresos <= cuartiles_ingresos[2], "Nivel_Ingresos"] = "Bajo"
df_cluster[df_cluster$Ingresos > cuartiles_ingresos[4], "Nivel_Ingresos"] = "Alto"
df_cluster$Calidad = "Básica"
df_cluster[df_cluster$Nivel_Ingresos == "Alto", "Calidad"] = "Alta"

# Mostrar segmentos
df_cluster = df_cluster[, cols_df_cluster] # Reordenar las columnas
df_cluster
