# K-MEANS METODO DEL CODO

# Parámetros
ruta_datos_limpios = "../Datos Limpios/"
csv_observaciones_dummy = "observaciones_dummy.csv"

# Cargar tabla de observaciones en data frame
df_observaciones_dummy = read.csv2(paste0(ruta_datos_limpios, csv_observaciones_dummy), stringsAsFactors = F)

# Crear y poblar data frame para guardar sumas de cuadrados de distancias dentro de centroides
df_totwithinss = data.frame(  k = 1:10
                            , tot.withinss = NA
                            )

for (k in 1:10){
  modelo = kmeans(df_observaciones_dummy, centers = k)
  df_totwithinss[k, "tot.withinss"] = modelo$tot.withinss
}

g = ggplot(df_totwithinss, aes(y = tot.withinss, x = k))
g = g + geom_line(colour = "red")
g = g + scale_x_continuous(breaks = 1:10)
g = g + ggtitle("Método del Codo")
g
