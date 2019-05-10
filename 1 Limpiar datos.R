# LIMPIAR DATOS

# OBJETIVO: Tabla de observaciones. Cada fila es un cliente; cada columna es un atributo del cliente.

# Rut  Calidad del Producto Principal  Edad  Categoría del Producto Principal  Estado Civil
# ---  ------------------------------  ----  --------------------------------  ------------
# 300 filas

# TABLAS DE ORIGEN:
# Clientes - 300 filas
# Productos - 6 filas
# Boleta - 1000 filas
# Boleta_Detalle - 10000 filas

# Paquetes
library(dplyr)
library(dummies)

# Parámetros
ruta_datos_originales = "../Datos Originales/"
ruta_datos_limpios = "../Datos Limpios/"
csv_observaciones = "observaciones.csv"
csv_observaciones_dummy = "observaciones_dummy.csv"
cols_observaciones_dummy = c("Celulares", "Terrazas", "Electrodomesticos", "Ingresos", "Edad", "Casado", "Soltero")

# Cargar tablas de origen en data frames
df_clientes = read.csv2(paste0(ruta_datos_originales, "clientes.csv"), stringsAsFactors = F)
df_productos = read.csv2(paste0(ruta_datos_originales, "productos.csv"), stringsAsFactors = F)
df_boleta = read.csv2(paste0(ruta_datos_originales, "boleta.csv"), stringsAsFactors = F)
df_boleta_detalle = read.csv2(paste0(ruta_datos_originales, "boleta_detalle.csv"), stringsAsFactors = F)

# 1) Para cada cliente, encontrar el producto principal. 
# La definición de esto es el producto de mayor monto de venta en la historia del cliente.

# Agregar Rut a df_boleta_detalle mediante un merge
df_boleta_detalle = merge(df_boleta_detalle, df_boleta, by = "NBoleta")

# Calcular Monto = Cantidad x Precio de cada fila
df_boleta_detalle$Monto = df_boleta_detalle$Cantidad * df_boleta_detalle$Precio

# Obtener el monto máximo para cada cliente
df_cliente_montomaximo = group_by(df_boleta_detalle, Rut) %>% summarise(max(Monto))
names(df_cliente_montomaximo)[2] = "Monto"

# Combinar con df_boleta_detalle
df_cliente_productoprincipal = merge(df_boleta_detalle, df_cliente_montomaximo, by = c("Rut", "Monto"))
df_cliente_productoprincipal = group_by(df_cliente_productoprincipal, Rut) %>% summarise(max(SKU_Producto))
names(df_cliente_productoprincipal)[2] = "ProductoPrincipal"
# nrow(df_cliente_productoprincipal)


# 2)	Para cada cliente, crear columna "Categoría del Producto Principal".
# Agregar columna Categoría mediante un merge con df_productos
df_cliente_productoprincipal = merge(df_cliente_productoprincipal, df_productos, by.x = "ProductoPrincipal", by.y = "SKU_Producto")


# 3)	Para cada producto, calcular su calidad, definida como:
# Calidad = Precio / min(Precio) de su categoría

# Generar data frame de precio mínimo por categoría
df_categoria = group_by(df_productos, Categoria) %>% summarise(min(Precio))
names(df_categoria)[2] = "PrecioMinimo"

# Unir PrecioMinimo a df_productos mediante un merge
df_productos = merge(df_productos, df_categoria, by = "Categoria")

# Calcular Calidad
df_productos$Calidad = df_productos$Precio / df_productos$PrecioMinimo


# 4) En tabla de observaciones, crear columnas Categoria e Ingresos = Calidad (ya que es un proxy)

# Inicializar tabla de observaciones
df_observaciones = merge(df_cliente_productoprincipal, df_productos, by.x = "ProductoPrincipal", by.y = "SKU_Producto")
df_observaciones = df_observaciones[, c("Rut", "Categoria.x", "Calidad")]
names(df_observaciones)[2] = "Categoria"
names(df_observaciones)[3] = "Ingresos"
# nrow(df_observaciones)


# 5) En tabla de observaciones, crear columna Edad.

# Crear función que calcula la edad en base al Rut
edad = function(rut){
  return(587.77 - 33.81*log(rut))
}

# Aplicar función en tabla de observaciones
df_observaciones$Edad = edad(df_observaciones$Rut)


# 6)	En tabla de observaciones, crear columna EstadoCivil.
df_observaciones = merge(df_observaciones, df_clientes, by = "Rut")
# str(df_observaciones)
# nrow(df_observaciones)

# 8) Crear tabla de observaciones con variables categóricas convertidas a dummy y normalizadas.

# Crear variables dummy
df_observaciones_dummy = df_observaciones
dummy("Categoria", data = df_observaciones_dummy)
df_observaciones_dummy = dummy.data.frame(df_observaciones_dummy)
names(df_observaciones_dummy)[2] = "Celulares"
names(df_observaciones_dummy)[3] = "Terrazas"
names(df_observaciones_dummy)[4] = "Electrodomesticos"
names(df_observaciones_dummy)[7] = "Casado"
names(df_observaciones_dummy)[8] = "Soltero"
df_observaciones_dummy = df_observaciones_dummy[, cols_observaciones_dummy]
# str(df_observaciones_dummy)

# Normalizar todas las variables
df_observaciones_dummy = scale(df_observaciones_dummy)
summary(df_observaciones_dummy)

# 9)	Guardar tablas de observaciones en archivos csv.
write.csv2(df_observaciones, file = paste0(ruta_datos_limpios, csv_observaciones), row.names = F)
write.csv2(df_observaciones_dummy, file = paste0(ruta_datos_limpios, csv_observaciones_dummy), row.names = F)
