# CLUSTERIZACION JERARQUICA

# Parámetros
ruta_datos_limpios = "../Datos Limpios/"
csv_observaciones_dummy = "observaciones_dummy.csv"

# Cargar tabla de observaciones en data frame
df_observaciones_dummy = read.csv2(paste0(ruta_datos_limpios, csv_observaciones_dummy))

# Crear matriz de distancias
d = dist(df_observaciones_dummy)

# Crear árbol de clusterización
arbol = hclust(d)

plot(arbol)