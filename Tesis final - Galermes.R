# Establecer la semilla aleatoria
set.seed(123)

if (!require(dplyr)) {
  install.packages("dplyr")
}
library(dplyr)

# Creación de las variables de forma aleatorias con 100 filas
datos <- data.frame(
  Total_Sales = runif(n = 100, min = 10000, max = 500000),
  Total_Cost_of_Goods = runif(n = 100, min = 5000, max = 450000)
)

# Generar más variables
n <- 100  # Número de filas
datos$Total_Operating_Expenses <- runif(n, min = 10000, max = 50000)
datos$Investment_Income <- runif(n, min = 500, max = 5000)
datos$Interest_Income <- runif(n, min = 200, max = 2000)
datos$Less_Interest_Expense <- runif(n, min = 100, max = 1000)
datos$Net_Income <- datos$Total_Sales - datos$Total_Cost_of_Goods - datos$Total_Operating_Expenses + datos$Investment_Income + datos$Interest_Income - datos$Less_Interest_Expense
datos$Cash_and_Cash_Equivalents <- runif(n, min = 1000, max = 10000)
datos$Short_Term_Investments <- runif(n, min = 5000, max = 20000)
datos$Inventario <- runif(n, min = 1000, max = 5000)
datos$Accounts_Receivable <- runif(n, min = 500, max = 1000)
datos$Total_Current_Assets <- datos$Cash_and_Cash_Equivalents + datos$Short_Term_Investments + datos$Accounts_Receivable + runif(n, min = 1000, max = 5000)
datos$Property_and_Plant_Equipment <- runif(n, min = 50000, max = 200000)
datos$Investments <- runif(n, min = 2000, max = 10000)
datos$Amortization <- runif(n, min = 0, max = 5000)
datos$Depreciation <- runif(n, min = 0, max = 5000)
datos$Total_Non_Current_Assets <- datos$Property_and_Plant_Equipment + datos$Investments + runif(n, min = 1000, max = 5000)
datos$Total_Intangibles <- runif(n, min = 1000, max = 5000)
datos$Total_Assets <- datos$Total_Current_Assets + datos$Total_Non_Current_Assets + datos$Total_Intangibles
datos$Net_Cash_provided_used_by_Operating_Activities <- runif(n, min = -5000, max = 5000)
datos$Notes_Payable <- runif(n, min = 10000, max = 50000)
datos$Total_Current_Liabilities <- datos$Notes_Payable + runif(n, min = 1000, max = 5000)
datos$Notes_Payable_Non_current <- runif(n, min = 5000, max = 20000)
datos$Total_Non_Current_Liabilities <- datos$Notes_Payable_Non_current + runif(n, min = 1000, max = 5000)
datos$Total_Liabilities <- datos$Total_Current_Liabilities + datos$Total_Non_Current_Liabilities
datos$Retained_Earnings <- runif(n, min = 1000, max = 5000)
datos$Total_Owner_s_Equity <- datos$Total_Assets - datos$Total_Liabilities
datos$Current_Ratio <- datos$Total_Current_Assets / datos$Total_Current_Liabilities
datos$Net_Working_Capital <- datos$Total_Current_Assets - datos$Total_Current_Liabilities
datos$Debt_to_Tangible_Equity <- runif(n, min = 0.1, max = 1)
datos$EBITDA <- datos$Net_Income + datos$Interest_Income + datos$Depreciation + datos$Amortization
datos$Margen_EBITDA <- (datos$EBITDA / datos$Total_Sales) * 100
datos$PDC <- datos$Margen_EBITDA / datos$Total_Assets
datos$Z_Score <- (datos$EBITDA/datos$Total_Assets)*6.72+1.05*(datos$Total_Owner_s_Equity/datos$Total_Liabilities)+6.56*(datos$Net_Working_Capital/datos$Total_Assets)+3.26*(datos$Retained_Earnings/datos$Total_Assets)
datos$Periodo_de_Cobro <- (datos$Accounts_Receivable/datos$Total_Sales) * 365
datos$PKT <- datos$Net_Working_Capital / datos$Total_Sales * 100
datos$ROE <- datos$Net_Income / datos$Total_Owner_s_Equity
datos$UODI <- datos$Net_Income - datos$Total_Operating_Expenses
datos$ANDEO <- (datos$Total_Current_Assets + datos$Total_Non_Current_Assets) - datos$Total_Current_Liabilities
datos$RAN <- datos$UODI / datos$ANDEO
datos$Prueba_Acida <- (datos$Total_Current_Assets -datos$Inventario) / datos$Total_Current_Liabilities
datos$Maxima_Altura_de_Mora <- runif(n, min = 0, max = 365)
datos$Contador_de_Mora_15_dias <- sample(0:5, n, replace = TRUE)
datos$Reestructuraciones <- sample(0:2, n, replace = TRUE)
datos$Central_de_Riesgo <- ifelse(datos$Maxima_Altura_de_Mora > 180, 1, 0)

datos$Posicion_Competitiva <- factor(sample(c("Líder", "Competidor", "Seguidor"), n, replace = TRUE))
levels(datos$Posicion_Competitiva) <- c("1", "2", "3")

datos$Planes_Crecimiento <- factor(sample(c("Corto Plazo", "Mediano Plazo", "Largo Plazo"), n, replace = TRUE))
levels(datos$Planes_Crecimiento) <- c("1", "2", "3")

datos$Diversificacion_Ingresos <- factor(sample(c("Geográfica", "Por Productos", "Por Líneas de Negocio"), n, replace = TRUE))
levels(datos$Diversificacion_Ingresos) <- c("1", "2", "3")

datos$Calidad_Administracion <- factor(sample(c("Baja", "Media", "Alta"), n, replace = TRUE))
levels(datos$Calidad_Administracion) <- c("1", "2", "3")

datos$Gestion_Financiera_Riesgos <- factor(sample(c("Débil", "Aceptable", "Fuerte"), n, replace = TRUE))
levels(datos$Gestion_Financiera_Riesgos) <- c("1", "2", "3")

datos$Innovaciones_Tecnologicas <- factor(sample(c("Baja", "Media", "Alta"), n, replace = TRUE))
levels(datos$Innovaciones_Tecnologicas) <- c("1", "2", "3")

datos$Politicas_Control_Auditoria <- factor(sample(c("Débil", "Aceptable", "Fuerte"), n, replace = TRUE))
levels(datos$Politicas_Control_Auditoria) <- c("1", "2", "3")

datos$Poder_Negociacion_Clientes <- factor(sample(c("Débil", "Aceptable", "Fuerte"), n, replace = TRUE))
levels(datos$Poder_Negociacion_Clientes) <- c("1", "2", "3")

datos$Poder_Negociacion_Proveedores <- factor(sample(c("Débil", "Aceptable", "Fuerte"), n, replace = TRUE))
levels(datos$Poder_Negociacion_Proveedores) <- c("1", "2", "3")

datos$Necesidades_Inversion <- factor(sample(c("Baja", "Media", "Alta"), n, replace = TRUE))
levels(datos$Necesidades_Inversion) <- c("1", "2", "3")

datos$Transparencia_Calidad_Informacion <- factor(sample(c("Baja", "Media", "Alta"), n, replace = TRUE))
levels(datos$Transparencia_Calidad_Informacion) <- c("1", "2", "3")

datos$Certificacion_Analista <- factor(sample(c("Sí", "No"), nrow(datos), replace = TRUE))
datos$Antiguedad_Estados_Financieros <- factor(sample(c("Anual", "Cuatrimestral", "Mensual", "Trimestral"), nrow(datos), replace = TRUE))

datos$Riesgo_Pais <- runif(n, min = 1, max = 1700) # Valores posibles entre 1 y 3000 para reflejar la realidad de América Latina
datos$Nivel_Riesgo_Pais <- cut(datos$Riesgo_Pais, breaks = c(0, 450, 900, 1700), labels = c("Bajo", "Medio", "Alto"), include.lowest = TRUE)

datos$Estructura_Sector <- factor(sample(c("Concentrado", "Competitivo", "Fragmentado"), n, replace = TRUE))
levels(datos$Estructura_Sector) <- c("1", "2", "3")

datos$Competencia <- factor(sample(c("Baja", "Media", "Alta"), n, replace = TRUE))
levels(datos$Competencia) <- c("1", "2", "3")

datos$Riesgos_Propios_Actividad <- factor(sample(c("Bajos", "Moderados", "Altos"), n, replace = TRUE))
levels(datos$Riesgos_Propios_Actividad) <- c("1", "2", "3")

# Normalizar la columna de Score para que esté en un rango entre 0 y 3
# Definir la función para normalizar y asignar categorías de riesgo
normalizar_a_rango_0_3 <- function(Z_Score) {
  # Función para asignar la categoría de riesgo
  asignar_categoria <- function(x) {
    if (x < 1.10) {
      return("High Risk")
    } else if (x <= 2.60) {
      return("Neutral")
    } else {
      return("Low Risk")
    }
  }
  
  # Normalizar los valores de Z_Score al rango de 0 a 3
  valores_normalizados <- numeric(length(Z_Score))
  for (i in seq_along(Z_Score)) {
    if (Z_Score[i] < 1.10) {
      valores_normalizados[i] <- (Z_Score[i] - min(Z_Score)) * (1 / (1.10 - min(Z_Score)))
    } else if (Z_Score[i] <= 2.60) {
      valores_normalizados[i] <- (Z_Score[i] - 1.10) * (1 / (2.60 - 1.10)) + 1
    } else {
      valores_normalizados[i] <- (Z_Score[i] - 2.60) * (1 / (max(Z_Score) - 2.60)) + 2.6
    }
  }
  
  # Asignar categoría de riesgo a cada valor
  categorias_riesgo <- sapply(Z_Score, asignar_categoria)
  
  return(list(valores_normalizados, categorias_riesgo))
}

resultado <- normalizar_a_rango_0_3(datos$Z_Score)
valores_normalizados <- resultado[[1]]
categorias_riesgo <- resultado[[2]]

# Asegurar que los valores normalizados estén dentro del rango de 0 a 3
valores_normalizados[valores_normalizados < 0] <- 0
valores_normalizados[valores_normalizados > 3] <- 3

# Mostrar los valores normalizados y las categorías de riesgo
print(valores_normalizados)
print(categorias_riesgo)

print(datos$Z_Score)

# Reemplazar los Z-Score originales con los nuevos valores normalizados
datos$Z_Score <- valores_normalizados

# Verificar que se haya realizado correctamente el reemplazo
print(datos$Z_Score)

# Convertir la matriz o array a un data.frame
datos <- as.data.frame(datos)

sapply(datos, class)

# Convertir todas las columnas a numéricas
datos_numericos <- sapply(datos, as.numeric)

#GRAFICAR FUNCIONES
#VARIABLES NUMÉRICAS

colnames(datos)

boxplot(datos$Total_Sales)
boxplot(datos$Total_Cost_of_Goods)
boxplot(datos$Total_Operating_Expenses)
boxplot(datos$Investment_Income)
boxplot(datos$Interest_Income)
boxplot(datos$Less_Interest_Expense)
boxplot(datos$Net_Income)
boxplot(datos$Cash_and_Cash_Equivalents)
boxplot(datos$Short_Term_Investments)
boxplot(datos$Inventario)
boxplot(datos$Accounts_Receivable)
boxplot(datos$Total_Current_Assets)
boxplot(datos$Property_and_Plant_Equipment)
boxplot(datos$Investments)
boxplot(datos$Amortization)
boxplot(datos$Depreciation)
boxplot(datos$Total_Non_Current_Assets)
boxplot(datos$Total_Intangibles)
boxplot(datos$Total_Assets)
boxplot(datos$Net_Cash_provided_used_by_Operating_Activities)
boxplot(datos$Notes_Payable)
boxplot(datos$Total_Current_Liabilities)
boxplot(datos$Notes_Payable_Non_current)
boxplot(datos$Total_Non_Current_Liabilities)
boxplot(datos$Total_Liabilities)
boxplot(datos$Retained_Earnings)
boxplot(datos$Total_Owner_s_Equity)
boxplot(datos$Current_Ratio)
boxplot(datos$Net_Working_Capital)
boxplot(datos$Debt_to_Tangible_Equity)
boxplot(datos$EBITDA)
boxplot(datos$Margen_EBITDA)
boxplot(datos$PDC)
boxplot(datos$Z_Score)
boxplot(datos$Periodo_de_Cobro)
boxplot(datos$PKT)
boxplot(datos$ROE)
boxplot(datos$UODI)
boxplot(datos$ANDEO)
boxplot(datos$RAN)

#VARIABLES CATEGORICAS
library(ggplot2)

# Lista de variables categóricas
variables_categoricas <- c("Prueba_Acida", "Maxima_Altura_de_Mora", "Contador_de_Mora_15_dias",
                           "Reestructuraciones", "Central_de_Riesgo", "Posicion_Competitiva",
                           "Planes_Crecimiento", "Diversificacion_Ingresos", "Calidad_Administracion",
                           "Gestion_Financiera_Riesgos", "Innovaciones_Tecnologicas", "Politicas_Control_Auditoria",
                           "Poder_Negociacion_Clientes", "Poder_Negociacion_Proveedores", "Necesidades_Inversion",
                           "Transparencia_Calidad_Informacion", "Certificacion_Analista", "Antiguedad_Estados_Financieros",
                           "Riesgo_Pais", "Nivel_Riesgo_Pais", "Estructura_Sector", "Competencia",
                           "Riesgos_Propios_Actividad")

# Crear un gráfico de barras para cada variable categórica
for (variable in variables_categoricas) {
  # Crear un dataframe con la frecuencia de cada categoría
  frecuencias <- data.frame(table(datos[[variable]]))
  
  # Ordenar las categorías por frecuencia descendente
  frecuencias <- frecuencias[order(-frecuencias$Freq), ]
  
  # Crear un gráfico de barras
  plot <- ggplot(frecuencias, aes(x = Var1, y = Freq)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = variable, x = "Categoría", y = "Frecuencia")
  
  # Mostrar el gráfico
  print(plot)
}

#MODELO GORDON-SHAPIRO
set.seed(123)
# Generar 100 muestras de la tasa de crecimiento aleatoria utilizando una distribución normal
datos_gordon <- data.frame(
  Tasa_Crecimiento_Ventas = rnorm(n, mean = 0, sd = 0.10),
  Total_Sales = datos$Total_Sales,
  Net_Income = datos$Net_Income,
  Total_Assets = datos$Total_Assets,
  Total_Owner_s_Equity = datos$Total_Owner_s_Equity,
  Current_Ratio = datos$Current_Ratio,
  Debt_to_Tangible_Equity = datos$Debt_to_Tangible_Equity,
  EBITDA = datos$EBITDA,
  Margen_EBITDA = datos$Margen_EBITDA,
  Periodo_de_Cobro = datos$Periodo_de_Cobro,
  ROE = datos$ROE,
  Central_de_Riesgo = datos$Central_de_Riesgo,
  Nombres_Empresas = replicate(100, paste(sample(LETTERS, 5, replace = TRUE), collapse = ""))
)
print(datos_gordon$Nombres_Empresas)

# Defino la tasa de descuento
tasa_descuento <- 0.05  # 5%

# Calcular el valor presente de las ganancias futuras para cada empresa
valor_presente_gordon_shapiro <- function(total_sales, tasa_crecimiento, margen_ebitda, periodo_cobro, tasa_descuento) {
  # Utilizar el modelo de Gordon-Shapiro para calcular el valor presente
  valor_presente <- (total_sales * margen_ebitda * periodo_cobro) / (tasa_descuento - tasa_crecimiento)
  
  # Tomar el valor absoluto del resultado para asegurar que sea positivo
  valor_presente <- abs(valor_presente)
  
  return(valor_presente)
}

# Calcular el valor presente de las ganancias futuras para cada empresa en la muestra
valor_presente_gordon_shapiro <- valor_presente_gordon_shapiro(datos_gordon$Total_Sales, datos_gordon$Tasa_Crecimiento_Ventas, datos_gordon$Margen_EBITDA, datos_gordon$Periodo_de_Cobro, tasa_descuento)

# Mostrar los resultados
print(valor_presente_gordon_shapiro)

sapply(valor_presente_gordon_shapiro, class)

datos_gordon$ValorGordon_Shapiro <- valor_presente_gordon_shapiro

print(datos_gordon)

# Establecer los umbrales para cada métrica
umbral_crecimiento_ventas <- 0.2  # 10%
umbral_valor_presente <- 10000000
umbral_margen_ebitda <- 10  # 10%
umbral_roe <- 0.5  # 5%
umbral_deuda_patrimonio <- 0.7

# Filtrar las empresas que cumplen con los criterios
empresas_unicornio <- datos_gordon[
  datos_gordon$Tasa_Crecimiento_Ventas > umbral_crecimiento_ventas &
    datos_gordon$ValorGordon_Shapiro > umbral_valor_presente &
    datos_gordon$Margen_EBITDA > umbral_margen_ebitda &
    datos_gordon$ROE > umbral_roe &
    datos_gordon$Debt_to_Tangible_Equity < umbral_deuda_patrimonio,
]

# Mostrar las empresas que cumplen con los criterios
print(empresas_unicornio)

if (!require(caret)) {
  install.packages("caret")
}
library(caret)

colnames(datos)

# ELIMINAR VARIABLES ALTAMENTE CORRELACIONADAS
datos_cor <- cor(datos_numericos)
highly_correlated <- findCorrelation(datos_cor, cutoff = 0.7)
print(highly_correlated)

# Mantener Z_Score en las variables altamente correlacionadas si está presente
if ("Z_Score" %in% rownames(datos_cor)) {
  highly_correlated <- setdiff(highly_correlated, which(rownames(datos_cor) == "Central_de_Riesgo"))
}

# Mantener Central_de_Riesgo en las variables altamente correlacionadas si está presente
if ("Central_de_Riesgo" %in% rownames(datos_cor)) {
  highly_correlated <- setdiff(highly_correlated, which(rownames(datos_cor) == "Z_Score"))
}

# Asegurar que se mantengan estas variables
variables_a_mantener <- c("Total_Sales", 
                          "Net_Income", 
                          "Net_Cash_provided_used_by_Operating_Activities", 
                          "Net_Working_Capital", 
                          "Z_Score", 
                          "Central_de_Riesgo")

# Eliminar las variables altamente correlacionadas excepto las requeridas
highly_correlated <- setdiff(highly_correlated, which(colnames(datos_cor) %in% variables_a_mantener))
datos_filtered <- datos[, -highly_correlated]

# Agregar las variables a mantener al conjunto de datos filtrados
variables_a_agregar <- setdiff(variables_a_mantener, colnames(datos_filtered))
datos_filtered <- cbind(datos_filtered, datos[, variables_a_agregar])
print(datos_filtered)

# Convertir todas las columnas a numéricas
datos <- as.data.frame(lapply(datos_filtered, as.numeric))

# Verificar si la conversión fue exitosa
if (all(sapply(datos_numericos, is.numeric))) {
  print("Todas las columnas se han convertido correctamente a numéricas.")
} else {
  print("Ha ocurrido un error en la conversión. Revisa tus datos.")
}

sapply(datos, class)

datos <- as.data.frame(datos)

# Calcular los límites para identificar valores atípicos en todas las variables numéricas
limites_atipicos <- apply(datos[, !names(datos) %in% "Z_Score"], 2, function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  limite_superior <- Q3 + 1.5 * IQR
  limite_inferior <- Q1 - 1.5 * IQR
  return(list(limite_superior = limite_superior, limite_inferior = limite_inferior))
})

# Identificar valores atípicos en todas las variables numéricas excepto Z_Score
valores_atipicos <- lapply(1:ncol(datos[, !names(datos) %in% "Z_Score"]), function(i) {
  x <- datos[, !names(datos) %in% "Z_Score"][, i]
  limite_superior <- limites_atipicos[[i]]$limite_superior
  limite_inferior <- limites_atipicos[[i]]$limite_inferior
  return(x[x < limite_inferior | x > limite_superior])
})

print(valores_atipicos)

# Convertir factores a variables dummy
datos_dummy <- as.data.frame(model.matrix(~., datos[, !names(datos) %in% "Z_Score"]))

# Identificar valores atípicos
limites_atipicos <- lapply(datos_dummy, function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  limite_superior <- Q3 + 1.5 * IQR
  limite_inferior <- Q1 - 1.5 * IQR
  list(limite_superior = limite_superior, limite_inferior = limite_inferior)
})

# Reemplazar los valores atípicos con NA en todas las columnas excepto Z_Score
for (i in 1:ncol(datos_dummy)) {
  limite_superior <- limites_atipicos[[i]]$limite_superior
  limite_inferior <- limites_atipicos[[i]]$limite_inferior
  datos_dummy[datos_dummy[, i] > limite_superior | datos_dummy[, i] < limite_inferior, i] <- NA
}

# Instalar y cargar el paquete zoo si no está instalado
if (!require(zoo)) {
  install.packages("zoo")
}
library(zoo)


# Aplicar la técnica de winsorización excepto a Z_Score
datos_winsorizados <- datos_dummy

# Obtener el índice de la columna de Z_Score
z_score_index <- which(colnames(datos) == "Z_Score")

for (i in 1:ncol(datos_dummy)) {
  Q1 <- quantile(datos_dummy[, i], 0.25, na.rm = TRUE)
  Q3 <- quantile(datos_dummy[, i], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  limite_superior <- Q3 + 1.5 * IQR
  limite_inferior <- Q1 - 1.5 * IQR
  if (i != z_score_index) {
    datos_winsorizados[, i][datos_dummy[, i] > limite_superior] <- limite_superior
    datos_winsorizados[, i][datos_dummy[, i] < limite_inferior] <- limite_inferior
  }
}

# Reasignar la columna de Z_Score
datos_winsorizados <- cbind(datos_winsorizados, Z_Score = datos[, "Z_Score"])

# Completar los valores NA con la mediana de cada columna excepto Z_Score
datos_winsorizados <- na.aggregate(datos_winsorizados, FUN = median, na.rm = FALSE)

# Imprimir los datos winsorizados
print(datos_winsorizados)

# Convertir todas las columnas a numéricas
datos <- as.data.frame(lapply(datos_winsorizados, as.numeric))

# Verificar si la conversión fue exitosa
if (all(sapply(datos_numericos, is.numeric))) {
  print("Todas las columnas se han convertido correctamente a numéricas.")
} else {
  print("Ha ocurrido un error en la conversión. Revisa tus datos.")
}

sapply(datos, class)

datos <- as.data.frame(datos)

colnames(datos)
# Definir las variables a transformar
variables_a_transformar <- c("Total_Sales","Total_Cost_of_Goods", "Total_Operating_Expenses", "Investment_Income",
                             "Interest_Income", "Less_Interest_Expense", "Net_Income", "Cash_and_Cash_Equivalents",
                             "Short_Term_Investments", "Inventario", "Accounts_Receivable",
                             "Property_and_Plant_Equipment", "Investments", "Amortization",
                             "Depreciation", "Total_Intangibles",
                             "Net_Cash_provided_used_by_Operating_Activities", "Notes_Payable",
                             "Notes_Payable_Non_current", "Retained_Earnings",  "Net_Working_Capital", "Debt_to_Tangible_Equity",
                             "Periodo_de_Cobro", "PKT", "ROE", "Contador_de_Mora_15_dias", "Reestructuraciones",
                             "Posicion_Competitiva", "Planes_Crecimiento", "Diversificacion_Ingresos",
                             "Calidad_Administracion", "Gestion_Financiera_Riesgos", "Innovaciones_Tecnologicas",
                             "Politicas_Control_Auditoria", "Poder_Negociacion_Clientes", "Poder_Negociacion_Proveedores",
                             "Necesidades_Inversion", "Transparencia_Calidad_Informacion", "Certificacion_Analista",
                             "Antiguedad_Estados_Financieros", "Estructura_Sector", "Competencia",
                             "Riesgos_Propios_Actividad")

# Ajustar los valores negativos o ceros
for (variable in variables_a_transformar) {
  # Obtener el valor mínimo de la variable
  min_value <- min(datos[[variable]])
  
  # Verificar si el valor mínimo es negativo o cero
  if (min_value <= 0) {
    # Sumar 1 más el valor absoluto del valor mínimo para que todos los valores sean positivos
    datos[[variable]] <- datos[[variable]] - min_value + 1
  }
}

print(datos)

# Aplicar transformación logarítmica a las variables numéricas
for (variable in variables_a_transformar) {
  if (is.numeric(datos[[variable]])) {
    datos[[paste0(variable, "_log")]] <- log(datos[[variable]])
  }
}


print(datos)

# Obtener los índices de las columnas que tienen "_log" al final de su nombre
indices_log <- grep("_log$", colnames(datos))

# Conservar la columna "Central_de_Riesgo"
indices_central_de_riesgo <- grep("Central_de_Riesgo", colnames(datos))

# Combinar los índices de las columnas "_log", "Z_Score" y "Central_de_Riesgo"
indices_a_mantener <- c(indices_log, indices_central_de_riesgo, which(colnames(datos) == "Z_Score"))

# Seleccionar solo las columnas que cumplen con los criterios
datos_filtrados <- datos[, indices_a_mantener]

# Verificar el resultado
print(datos_filtrados)

datos <- datos_filtrados

print(datos)

datos <- as.data.frame(datos)

if (!require(dplyr)) {
  install.packages("dplyr")
}
library(dplyr)

if (!require(rpart)) {
  install.packages("rpart")
}
if (!require(rpart.plot)) {
  install.packages("rpart.plot")
}

library(rpart)
library(rpart.plot)

#ARBOL DE DECISION
set.seed(123)

# Dividir los datos en conjuntos de entrenamiento y prueba
indices_entrenamiento <- createDataPartition(datos$Central_de_Riesgo, p = 0.7, list = FALSE)
datos_entrenamiento <- datos[indices_entrenamiento, ]
datos_prueba <- datos[-indices_entrenamiento, ]

# Convertir la variable de respuesta a factor en ambos conjuntos de datos
datos_entrenamiento$Central_de_Riesgo <- factor(datos_entrenamiento$Central_de_Riesgo, levels = c("0", "1"))
datos_prueba$Central_de_Riesgo <- factor(datos_prueba$Central_de_Riesgo, levels = c("0", "1"))

# Calculamos los pesos de clase
pesos_clase <- ifelse(datos_entrenamiento$Central_de_Riesgo == "0", 0.7, 0.3)

# Definir el esquema de validación cruzada
control <- trainControl(method = "repeatedcv", 
                        number = 5,           # Número de repeticiones de validación cruzada
                        repeats = 1)          # Número de repeticiones del proceso completo de validación cruzada

# Ajustar el modelo de árbol de decisión con los datos de entrenamiento y validación cruzada
modelo_arbol <- train(Central_de_Riesgo ~ .,
                      data = datos_entrenamiento, 
                      method = "rpart",     # Método de ajuste del modelo: árbol de decisión
                      trControl = control, # Especificar el esquema de validación cruzada
                      tuneGrid = expand.grid(cp = seq(0.01, 0.5, by = 0.01)), # Rango de valores para el parámetro cp
                      weights = pesos_clase) # Asignar pesos de clase

# Realizar predicciones en los datos de prueba
predicciones <- predict(modelo_arbol, newdata = datos_prueba)

# Calcular la matriz de confusión nuevamente
conf_matrix <- confusionMatrix(predicciones, datos_prueba$Central_de_Riesgo)
conf_matrix

# Extraer la sensibilidad y especificidad de la matriz de confusión
sensibilidad <- conf_matrix$byClass['Sensitivity']
especificidad <- conf_matrix$byClass['Specificity']

# Imprimir los resultados
print(paste("Sensibilidad:", sensibilidad))
print(paste("Especificidad:", especificidad))

if (!require(DescTools)) {
  install.packages("DescTools")
}
library(DescTools)

# Convertir las predicciones a numérico
predicciones_numeric <- as.numeric(as.character(predicciones))

# Convertir los datos de prueba a numérico
datos_prueba_numeric <- as.numeric(as.character(datos_prueba$Central_de_Riesgo))

# Calcular el índice de Gini
gini_arbol <- Gini(ifelse(predicciones == "1", 1, 0), ifelse(datos_prueba$Central_de_Riesgo == "1", 1, 0))
print(paste("Índice de Gini del Árbol de Decisiones:", gini_arbol))

# Evaluar el rendimiento del modelo (Curvas ROC)
if (!require(pROC)) {
  install.packages("pROC")
}
library(pROC)

# Construir la curva ROC
roc_curve <- roc(ifelse(predicciones == "1", 1, 0), as.numeric(datos_prueba$Central_de_Riesgo))

# Plotear la curva ROC
plot(roc_curve, main = "Curva ROC", print.auc = TRUE)

#Gradient Boosting

set.seed(123)
if (!require(gbm)) {
  install.packages("gbm")
}
library(gbm)

# Dividir los datos en conjuntos de entrenamiento y prueba
indices_entrenamiento_gbm <- createDataPartition(datos$Central_de_Riesgo, p = 0.7, list = FALSE)
datos_entrenamiento_gbm <- datos[indices_entrenamiento_gbm, ]
datos_prueba_gbm <- datos[-indices_entrenamiento_gbm, ]

# Entrenar el modelo de Gradient Boosting
modelo_gbm <- gbm(Central_de_Riesgo ~ ., 
                  data = datos_entrenamiento_gbm, 
                  distribution = "bernoulli", 
                  n.trees = 100, 
                  interaction.depth = 3, 
                  shrinkage = 0.1) # Ajusta la tasa de aprendizaje según sea necesario

# Obtener las predicciones del modelo en los datos de prueba
predicciones_gbm <- predict(modelo_gbm, datos_prueba_gbm, n.trees = 100, type = "response")

# Convertir las predicciones a clases (0 o 1)
predicciones_clases_gbm <- ifelse(predicciones_gbm > 0.5, 1, 0)

# Calcular la matriz de confusión
conf_matrix_gbm <- table(predicciones_clases_gbm, datos_prueba_gbm$Central_de_Riesgo)

# Calcular la sensibilidad y especificidad
sensibilidad_gbm <- conf_matrix_gbm[2, 2] / sum(conf_matrix_gbm[2, ])
especificidad_gbm <- conf_matrix_gbm[1, 1] / sum(conf_matrix_gbm[1, ])

# Imprimir los resultados
print(paste("Sensibilidad:", sensibilidad_gbm))
print(paste("Especificidad:", especificidad_gbm))

# Calcular el índice de Gini
gini <- Gini(as.numeric(datos_prueba_gbm$Central_de_Riesgo), predicciones_gbm)

# Imprimir el índice de Gini
print(paste("Índice de Gini Gradient:", gini))

# Calcular la curva ROC del Gradient
roc_curve_gbm <- roc(as.numeric(datos_prueba_gbm$Central_de_Riesgo), as.numeric(predicciones_clases_gbm))

# Graficar la curva ROC del Gradient
plot(roc_curve_gbm, main = "Curva ROC", print.auc = TRUE)


#Redes Neuronales

set.seed(123)
if (!require(neuralnet)) {
  install.packages("neuralnet")
}
library(neuralnet)

# Dividir los datos en conjuntos de entrenamiento y prueba
indices_entrenamiento_ann <- createDataPartition(datos$Central_de_Riesgo, p = 0.7, list = FALSE)
datos_entrenamiento_ann <- datos[indices_entrenamiento_ann, ]
datos_prueba_ann <- datos[-indices_entrenamiento_ann, ]

# Entrenar la red neuronal artificial
modelo_ann <- neuralnet(Central_de_Riesgo ~ ., data = datos_entrenamiento_ann, hidden = c(5, 2), linear.output = FALSE)

# Realizar predicciones en los datos de prueba
predicciones_ann <- predict(modelo_ann, datos_prueba_ann)

# Convertir las predicciones a clases (0 o 1)
predicciones_clases_ann <- ifelse(predicciones_ann > 0.5, 1, 0)

# Calcular la matriz de confusión
conf_matrix_ann <- table(predicciones_clases_ann, datos_prueba_ann$Central_de_Riesgo)
conf_matrix_ann

# Calcular la sensibilidad y especificidad
sensibilidad_ann <- conf_matrix_ann[2, 2] / sum(conf_matrix_ann[2, ])
especificidad_ann <- conf_matrix_ann[1, 1] / sum(conf_matrix_ann[1, ])

# Imprimir los resultados
print(paste("Sensibilidad:", sensibilidad_ann))
print(paste("Especificidad:", especificidad_ann))

# Calcular la curva ROC de las Redes
roc_curve_ann <- roc(as.numeric(datos_prueba_ann$Central_de_Riesgo), as.numeric(predicciones_clases_ann))

# Graficar la curva ROC de las Redes
plot(roc_curve_ann, main = "Curva ROC", print.auc = TRUE)

#Support Vector Machines (SVM)

set.seed(123)
if (!require(e1071)) {
  install.packages("e1071")
}
library(e1071)

# Dividir los datos en conjuntos de entrenamiento y prueba
indices_entrenamiento_svm <- createDataPartition(datos$Central_de_Riesgo, p = 0.7, list = FALSE)
datos_entrenamiento_svm <- datos[indices_entrenamiento_svm, ]
datos_prueba_svm <- datos[-indices_entrenamiento_svm, ]

# Entrenar el modelo SVM
modelo_svm <- svm(Central_de_Riesgo ~ ., data = datos_entrenamiento_svm, kernel = "radial")

# Realizar predicciones en los datos de prueba
predicciones_svm <- predict(modelo_svm, datos_prueba_svm)

# Calcular la matriz de confusión
conf_matrix_svm <- table(predicciones_svm, datos_prueba_svm$Central_de_Riesgo)
conf_matrix_svm

# Calcular la sensibilidad y especificidad
sensibilidad_svm <- conf_matrix_svm[2, 2] / sum(conf_matrix_svm[2, ])
especificidad_svm <- conf_matrix_svm[1, 1] / sum(conf_matrix_svm[1, ])

# Imprimir los resultados
print(paste("Sensibilidad:", sensibilidad_svm))
print(paste("Especificidad:", especificidad_svm))

# Calcular la curva ROC de SVM
roc_curve_svm <- roc(as.numeric(datos_prueba_svm$Central_de_Riesgo), as.numeric(predicciones_svm))

# Graficar la curva ROC de SVM
plot(roc_curve_svm, main = "Curva ROC", print.auc = TRUE)

#REGRESION RIDGE & LASSO

set.seed(123)
if (!require(glmnet)) {
  install.packages("glmnet")
}
library(glmnet)

# Eliminar filas con valores faltantes y convertir todas las columnas a numéricas
datos_sin_na <- na.omit(datos)
datos_numericos <- sapply(datos_sin_na, as.numeric)

# Dividir los datos en conjuntos de entrenamiento y prueba
indices_entrenamiento_r_l <- createDataPartition(datos$Central_de_Riesgo, p = 0.7, list = FALSE)
datos_entrenamiento_r_l <- datos[indices_entrenamiento_r_l, ]
datos_prueba_r_l <- datos[-indices_entrenamiento_r_l, ]

# Preparar los predictores y la variable respuesta
x <- as.matrix(datos_entrenamiento_r_l[, !colnames(datos_entrenamiento_r_l) %in% "Central_de_Riesgo"])
y <- datos_entrenamiento_r_l$Central_de_Riesgo

# Ajustar un modelo de regresión Ridge
modelo_ridge <- cv.glmnet(x, y, alpha = 0)

# Ajustar un modelo de regresión Lasso
modelo_lasso <- cv.glmnet(x, y, alpha = 1)

# Realizar predicciones en los datos de prueba
x_prueba <- as.matrix(datos_prueba_r_l[, !colnames(datos_prueba_r_l) %in% "Central_de_Riesgo"])
predicciones_ridge <- predict(modelo_ridge, s = "lambda.min", newx = x_prueba)
predicciones_lasso <- predict(modelo_lasso, s = "lambda.min", newx = x_prueba)

# Calcular el error cuadrático medio (MSE) en los datos de prueba
mse_ridge <- mean((predicciones_ridge - datos_prueba_r_l$Central_de_Riesgo)^2)
mse_lasso <- mean((predicciones_lasso - datos_prueba_r_l$Central_de_Riesgo)^2)

# Imprimir los resultados
print("Regresión Ridge:")
print(paste("MSE en datos de prueba:", mse_ridge))
print("\nRegresión Lasso:")
print(paste("MSE en datos de prueba:", mse_lasso))

# Ajustar un modelo de regresión logística
set.seed(123)
modelo_logit <- glm(Central_de_Riesgo ~ ., data = datos_entrenamiento, family = "binomial", 
                    control = glm.control(maxit = 1000))


# Imprimir el modelo de regresión logística
print("\nModelo de Regresión Logística:")
print(summary(modelo_logit))

# Obtener los datos del Z-Score
Z_Score <- datos$Z_Score
Z_Score

# Calcular las probabilidades de incumplimiento basadas en el modelo de regresión logística
set.seed(123)
modelo_logit_completo <- glm(Central_de_Riesgo ~ ., 
                    data = datos, family = binomial(link = "logit"),
                    control = glm.control(maxit = 1000))


probabilidades_incumplimiento <- predict(modelo_logit_completo, type = "response")

# Desactivar la notación científica para imprimir números
options(scipen = 999)

# Mostrar las primeras probabilidades de incumplimiento calculadas
print(probabilidades_incumplimiento)

# Definir la función asignar_calificacion() con los umbrales adecuados
asignar_calificacion <- function(probabilidad_incumplimiento) {
  if (probabilidad_incumplimiento >= 0.9999999999999997779554) {
    return("AAA")
  } else if (probabilidad_incumplimiento >= 0.9999999999957617236035) {  
    return("AA")
  } else if (probabilidad_incumplimiento >= 0.9999999999942537076691) {  
    return("A")
  } else if (probabilidad_incumplimiento >= 0.9999999999901710845407) {  
    return("BBB")
  } else if (probabilidad_incumplimiento >= 0.0000000000051490913657) {  
    return("BB")
  } else if (probabilidad_incumplimiento >= 0.0000000000043316507051) {  
    return("B")
  } else if (probabilidad_incumplimiento >= 0.0000000000024820614702) {  
    return("CCC")
  } else if (probabilidad_incumplimiento >= 0.0000000000010797496919) {  
    return("CC")
  } else if (probabilidad_incumplimiento >= 0.0000000000001702553529) {  
    return("C")
  } else {
    return("D")
  }
}

# Aplicar la función asignar_calificacion() a las probabilidades de incumplimiento
calificaciones <- sapply(probabilidades_incumplimiento, asignar_calificacion)

# Mostrar las calificaciones asignadas
print(calificaciones)

print(datos$Z_Score)

# Filtrar datos para eliminar NA
datos_filtrados <- datos[!is.na(datos$Z_Score) & !is.na(calificaciones), ]

# Graficar los datos de puntajes Z y las calificaciones de riesgo
plot(datos_filtrados$Z_Score, datos_filtrados$calificaciones, 
     xlab = "Calificación de Riesgo", ylab = "Puntaje Z", 
     main = "Relación entre Puntaje Z y Calificación de Riesgo")


if (!require(stats)) {
  install.packages("stats")
}

# Cargar la biblioteca necesaria para ANOVA
library(stats)

# ANOVA
resultado_anova <- aov(Z_Score ~ calificaciones, data = datos)

# Mostrar el resumen del ANOVA
summary(resultado_anova)

# Calcular los residuos del modelo
residuos <- resid(modelo_logit, type = "deviance")

# Graficar los residuos
plot(residuos, type = "p", main = "Gráfico de Residuos", xlab = "Índice de Observación", ylab = "Residuos de Deviance")
abline(h = 0, col = "red")

# Asignar las calificaciones de Moody's a los datos
datos$Calificacion_Moodys <- calificaciones

# Mostrar las primeras filas de los datos actualizados
print(datos)

set.seed(123)
datos$Nombres_Empresas <- datos_gordon$Nombres_Empresas

# Definir las categorías de riesgo aprobadas
categorias_aprobadas <- c("AAA","AA","A", "BBB")

# Asignar el crédito aprobado o no aprobado
credito_aprobado <- ifelse(calificaciones %in% categorias_aprobadas, "Crédito Aprobado", "Crédito No Aprobado")

# Instalar y cargar la librería 'truncnorm'
if (!require(truncnorm)) {
  install.packages("truncnorm")
}

library(truncnorm)

# Crear un data frame con los resultados y reemplazar los números de cliente con los nombres de las empresas
resultados <- data.frame(Empresa = datos$Nombres_Empresas,
                         Valor_Scoring = Z_Score,
                         Calificación = calificaciones,
                         Resultado = credito_aprobado)

# Mostrar los resultados en forma de tabla
print(resultados)

# Obtener los nombres de las empresas unicornio
nombres_empresas_unicornio <- empresas_unicornio$Nombres_Empresas

# Marcar las empresas en resultados como unicornio si están en la lista de nombres de empresas unicornio
resultados$Unicornio <- resultados$Empresa %in% nombres_empresas_unicornio

# Mostrar los resultados
print(resultados)

set.seed(123)
# Definir una función para asignar montos de crédito con ajuste
asignar_monto_credito_ajustado <- function(z_score, calificaciones, unicornio) {
  # Definir los límites de crédito máximo y mínimo
  limite_credito_maximo <- 100000  # $100,000
  
  # Factor de ajuste para reflejar mejor la relación entre el puntaje de crédito y el monto de crédito
  factor_ajuste <- 0.75
  
  # Verificar si la calificación es aprobada (AAA, AA o A)
  if (calificaciones %in% c("AAA", "AA", "A", "BBB")) {
    # Calcular el monto de crédito inicial basado en el Z-Score
    monto_credito <- z_score / 3 * limite_credito_maximo
    
    # Ajustar el monto de crédito en función de la calificación
    if (calificaciones %in% c("AAA", "AA")) {
      monto_credito <- monto_credito * 1.5  # Aumentar el monto de crédito para calificaciones más altas
    }
    
    # Aplicar el factor de ajuste
    monto_credito <- monto_credito * factor_ajuste
    
    # Ajustar el monto de crédito si es unicornio
    if (unicornio) {
      monto_credito <- min(monto_credito, limite_credito_maximo)  # Asegurar que no exceda el límite máximo
    }
    
    # Generar un monto de crédito ajustado usando una distribución beta truncada
    if (unicornio) {
      monto_credito_ajustado <- monto_credito
    } else {
      monto_credito_ajustado <- rtruncnorm(1, a = 0, b = limite_credito_maximo, mean = monto_credito, sd = monto_credito * 0.2)
    }
  } else {
    # Si la calificación no está aprobada, asignar un monto de crédito de $0
    monto_credito_ajustado <- 0
  }
  
  return(monto_credito_ajustado)
}

# Aplicar la función a los datos, pasando el parámetro 'unicornio' para las empresas
resultados$Monto_del_Credito <- mapply(asignar_monto_credito_ajustado, resultados$Valor_Scoring, resultados$Calificación, resultados$Unicornio)

# Verificar si hay algún monto de crédito negativo y ajustarlo a $0 si es necesario
resultados$Monto_del_Credito[resultados$Monto_del_Credito < 0] <- 0

# Mostrar los resultados
print(resultados)

# Aplicar la función a los datos, pasando el parámetro 'unicornio' para las empresas
resultados$Monto_del_Credito <- mapply(asignar_monto_credito_ajustado, resultados$Valor_Scoring, resultados$Calificación, resultados$Unicornio)

# Verificar si hay algún monto de crédito negativo y ajustarlo a $0 si es necesario
resultados$Monto_del_Credito[resultados$Monto_del_Credito < 0] <- 0

# Establecer opciones para mostrar números sin notación científica
options(scipen = 999)

# Mostrar los resultados
print(resultados)

# Función para filtrar los resultados por nombre de empresa
filtrar_resultados_por_empresa <- function(nombre_empresa) {
  # Filtrar los resultados para la empresa específica
  resultados_empresa <- resultados[resultados$Empresa == nombre_empresa, ]
  
  # Mostrar los resultados en forma de tabla
  print(resultados_empresa)
  
  # Obtener las variables más predictivas del modelo
  variables_predictivas_importantes <- coef(modelo_logit_completo)
  top_variables <- names(sort(abs(variables_predictivas_importantes), decreasing = TRUE))[1:5]
  print(top_variables)
}

#PRUEBA CON UNO SIN CREDITO
nombre_empresa <- "YYKWZ" # Cambiar por el nombre de la empresa deseada
filtrar_resultados_por_empresa(nombre_empresa)

#PRUEBA CON UNO CON CREDITO
nombre_empresa <- "UWSBX"  # Cambiar por el nombre de la empresa deseada
filtrar_resultados_por_empresa(nombre_empresa)

#PRUEBA CON UN UNICORNIO
nombre_empresa <- "MLYMT"  # Cambiar por el nombre de la empresa deseada
filtrar_resultados_por_empresa(nombre_empresa)

