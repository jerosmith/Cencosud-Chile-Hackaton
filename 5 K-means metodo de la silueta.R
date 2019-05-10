# K-MEANS METODO DE LA SILUETA

# Paquetes
library(cluster)
library(ggplot2)

# Parámetros
ruta_datos_limpios = "../Datos Limpios/"
csv_observaciones_dummy = "observaciones_dummy.csv"

# Cargar tabla de observaciones en data frame
df_observaciones_dummy = read.csv2(paste0(ruta_datos_limpios, csv_observaciones_dummy), stringsAsFactors = F)

# Crear y poblar data frame para guardar sumas de cuadrados de distancias dentro de centroides
df_silinfoavgwidth = data.frame(  k = 2:10
                                , silinfo.avg.width = NA
                                )

for (k in 2:10){
  modelo = pam(df_observaciones_dummy, k = k)
  df_silinfoavgwidth[k-1, "silinfo.avg.width"] = modelo$silinfo$avg.width
}

g = ggplot(df_silinfoavgwidth, aes(y = silinfo.avg.width, x = k))
g = g + geom_line(colour = "red")
g = g + scale_x_continuous(breaks = 2:10)
g = g + ggtitle("Método de la Silueta")
g
