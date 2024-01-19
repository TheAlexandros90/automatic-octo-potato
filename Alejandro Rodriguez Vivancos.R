
# Obtención de la información ---------------------------------------------

#Obtengo los datos de Yahoo de la empresa Condor Gold Plc 
install.packages("quantmod")
library(quantmod)
install.packages("plotly")
library(plotly)
install.packages("forecast")
library(forecast)
install.packages("lubridate")
library(lubridate)
install.packages("tseries")
library(tseries)
install.packages("stats")
library(stats)
install.packages("GGally")
library(GGally)
install.packages("rugarch")
library(rugarch)
install.packages("sandwich")
library(sandwich)
install.packages("lmtest")
library(lmtest)
install.packages("dplyr")
library(dplyr)
library(xts)

getSymbols("COG.TO", src = "yahoo", from = "2020-01-11", to = "2023-02-11")

#Convierto el objeto en dataframe para que sea mas sencillo de manejar

datos <- data.frame(Fecha = index(COG.TO), Precio = coredata(COG.TO)[,4])
datos$Fecha <- as.Date(datos$Fecha, format = "%Y-%m-%d" )
plot(datos,type = 'l') 


# Descripción de la serie temporal ----------------------------------------

#Creo la serie temporal
 
serie_temporal <- xts(datos$Precio, order.by = datos$Fecha)
plot(serie_temporal)
dim(datos)[1]

#Creo una segunda serie de otra manera que utilizaré en algunos test en los que serie_temporal no funciona

-----------------------------------------------------------------------
serie_temporal_2 <- ts(data = datos$Precio, start = c(year(min(datos$Fecha)), month(min(datos$Fecha))),
                       frequency = 252) # Usamos periodo bursatil
plot(serie_temporal_2)

#Uso las funciones head() y tail() para ver los primeros 6 valores y los últimos 6 valores

head(serie_temporal_2)
tail(serie_temporal_2)

# Creo un gráfico interactivo de la linea temporal
plot_ly(datos, x = ~Fecha, y = ~Precio, type = 'scatter', mode = 'lines') %>%
  layout(title = "Evolución de los precios de Cierre de Condor Gold", xaxis = list(title = "Fecha"), yaxis = list(title = "Precio de cierre ajustado"))

#Aplico la función summary para obtener información de la serie temporal

info_básica <- summary(serie_temporal)

# Descomposición de la serie temporal -------------------------------------

#Para encontrar la tendencia, estudio la regresión lineal

regresion_lineal <- lm(Precio ~ Fecha, data = datos)
summary(regresion_lineal)

#Fluctuación cíclica por medio de la descomposición clásica

#Descompongo la serie temporal àra separar la aleatoriedad, la estacionalidad y la tendencia

serie_temporal_descom <- decompose(serie_temporal_2)
plot(serie_temporal_descom)
serie_temporal_2

#Me centro en la tendencia

tendecia_serie_temporal <- decompose(serie_temporal_2)$trend

#Creo un segundo dataframe cruzando la tendencia y la serie original

df_tendencia <- data.frame(x = index(serie_temporal), y = coredata(serie_temporal), 
                 tendecia_serie_temporal = coredata(tendecia_serie_temporal))

#El cual grafico

ggplot(df_tendencia, aes(x = x)) + 
  geom_line(aes(y = y), color = "blue") + 
  geom_line(aes(y = tendecia_serie_temporal), color = "red") + 
  labs(title = "Relación de la serie temporal - Tendencia", 
       y = "Valor", x = "Tiempo")

#paso a la estacionalidad siguiendo pero si ncruzarla con la línea temporal ya que básicamente se trata de la misma línea

tendecia_estacional <- decompose(serie_temporal_2)$seasonal
ggseasonplot(tendecia_estacional, year.labels = TRUE, year.labels.right = TRUE,
             main = "Descomposición de la serie temporal - Componente estacional")

#Por último analizo el ruido que al igual que con la tendencia cruzaré con la serie temporal:

ruido <- decompose(serie_temporal_2)$random

#El dataframe que creo es la serie temporal - el componente estacional y la tendencia, dejando el ruido como resultado

df_ruido <- df <- data.frame(x = index(serie_temporal), y = coredata(serie_temporal) - coredata(tendecia_serie_temporal) - coredata(tendecia_serie_temporal), 
                             residuo = coredata(ruido))

#Quedando gráficamente representado de la siguiente manera

plot_ly(df, x = ~x, y = ~ruido, type = "scatter", mode = "lines", line = list(color = "purple")) %>%
  layout(title = "Evolución del componente ruido", xaxis = list(title = "Tiempo"), yaxis = list(title = "Valor"))


# Análisis de estacionariedad ---------------------------------------------

# Antes de hacer otro análisis he de determinar si la serie es estacionaria o no

comprobacion_estacionariedad <- adf.test(serie_temporal)

comprobacion_estacionariedad


#Siendo la hipótesis nula que la serie no es estacionaria, dado que el p-value = 0.2826 y es mayor que 
#el nivel de significación elegido (0.05), no se rechaza la hipótesis nula
#Por ello, debo convertirla en estacionaria

serie_estacionaria <- diff(serie_temporal, differences = 1)
plot(serie_estacionaria)
serie_estacionaria_2 <- diff(serie_temporal_2, differences = 1)

#Vuelvo a aplicar el adf.test para comprobar si es estacionaria esta nueva serie temporal

comprobacion_estacionariedad_2 <-adf.test(serie_estacionaria_2)

#Obtenemos el mensaje In adf.test(serie_estacionaria) : p-value smaller than printed p-value
#que indica que el valor de p es tan pequeño que es menor que el valor pro defecto (0,01), indicando que 
#Se trata de una serie temporal es estacionaria con un alto nivel de confianza
#https://medium.com/wwblog/stationarity-testing-using-the-augmented-dickey-fuller-test-8cf7f34f4844

#Aplico la función acf() para encontrar patrones estacionales a través de un análisis de autoacorrelacion
# Y hayo los limites de confinaza (para pode representarlo más claramente)
relacion_acf <- acf(serie_estacionaria_2,lag.max = 50, plot = FALSE)
plot(relacion_acf)
#Creo un dataframe y otro con los límites de confianza (95%)

df_acf <- data.frame(lag = relacion_acf$lag, acf = relacion_acf$acf)
n <- length(serie_estacionaria_2)
conf95 <- qnorm(0.025)/sqrt(n)

df_ci <- data.frame(lag = relacion_acf$lag,
                    upper = conf95,
                    lower = -conf95)

#Creo el gráfico

ggplot(df_acf, aes(x = lag, y = acf)) +
  geom_segment(aes(xend = lag, yend = 0)) +
  geom_hline(yintercept = c(-conf95, conf95), linetype = "dashed") +
  geom_hline(yintercept = 0) +
  ggtitle("Función de autocorrelación (ACF)") +
  xlab("Retraso") +
  ylab("Coeficiente de autocorrelación")

#Complementariamente realizo el test PACF para calcular la función de autocorrelación parcial (PACF) 

relacion_pacf <- pacf(as.numeric(serie_temporal), lag.max = 50)

# Hago los mismos pasos que para el ACF epresentar gráficamente la PACF

df_pacf <- data.frame(lag = relacion_pacf$lag, acf = relacion_pacf$acf)

df_ci_2 <- data.frame(lag = relacion_pacf$lag,
                    upper = conf95,
                    lower = -conf95)

#Creo el gráfico

ggplot(df_pacf, aes(x = lag, y = acf)) +
  geom_segment(aes(xend = lag, yend = 0)) +
  geom_hline(yintercept = c(-conf95, conf95), linetype = "dashed") +
  geom_hline(yintercept = 0) +
  ggtitle("Función de autocorrelación parcial (PACF)") +
  xlab("Retraso") +
  ylab("Coef. de autocorrelación parcial")





# Ajuste de la serie temporal a un modelo ARIMA ---------------------------

# Se que serie_estacionaria es una serie temporal que puedo ajusta a un modelo ARMA de orden (p,q) con p>0 y q=0.

modelo_arma <-arima(serie_estacionaria, order = c(1, 0, 0))
summary(modelo_arma)


#Adicinonalmente creo el modelo ARIMA de orden superior (2,1,1)

modelo_arima <-arima(serie_estacionaria, order = c(2, 1, 1))
summary(modelo_arima)

#Lo creo adicionalemte para la serie_estacioanria_2

modelo_arima_2 <-arima(serie_estacionaria_2, order = c(2, 1, 1))

summary(modelo_arima_2)

#Para acabar ejecuto un modelo GARCH


# Especifico el modelo GARCH (1,1)
modeloGarch <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0)))

# Ajusto el modelo GARCH a la serie temporal
Garch <- ugarchfit(modeloGarch, serie_estacionaria_2)

# Obtengo el resumen del modelo
summary(Garch)

#Estudio el objeto uGARCHfit, obteniendo los coeficientes

coef(Garch)

#Obtengo los errores de los coerficientes:

coeficiente_errores_GARCH <- sqrt(diag(vcov(Garch)))
coeficiente_errores_GARCH


# Análisis de la bondad de ajuste -----------------------------------------

#Del modelo ARMA (1,0): Intervalos de confianza de los coeficientes

confint_arma <- confint(modelo_arma)
confint_arma
#prueba de significancia para el coeficiente ar1
#hipótesis nula: ar1=0
ar1_coef <- -0.1154
t_stat <- ar1_coef / (modelo_arma$sigma2 / sqrt(n))
p_value <- 2 * pt(abs(t_stat), df = n - 2, lower.tail = FALSE)

p_value

#Del modelo ARIMA: Intervalos de confianza de los coeficientes

confint_arima <- confint(modelo_arima)

#Criterios de información

AIC(modelo_arima)
BIC(modelo_arima)

#t.test para evaluar la relevancia del cada c ar1

coeficiente_ar1_arima <- coef(modelo_arima)[1]

coeficiente_ar1_arima
#D.estandar del coeficiente

desv_est_ar1 <- sqrt(vcov(modelo_arima)[1,1])

desv_est_ar1
#Calculo p y t de AR

t <- coeficiente_ar1_arima / desv_est_ar1
p <- 2 * pt(-abs(t), df = modelo_arima$nobs - 2)

#Repetimos los pasos para ar2:

coeficiente_ar2_arima <- coef(modelo_arima)[2]

coeficiente_ar2_arima
#D.estandar del coeficiente

desv_est_ar2 <- sqrt(vcov(modelo_arima)[2,2])

#Calculo p y t de AR2

t2 <- coeficiente_ar2_arima / desv_est_ar2
p2 <- 2 * pt(-abs(t2), df = modelo_arima$nobs - 2)

#Reviso la significancia de  ma1

coeficiente_ma1_arima <- coef(modelo_arima)[3]

#D.estandar del coeficiente

desv_est_ma1 <- sqrt(vcov(modelo_arima)[3,3])

#Calculo p y t de am1

t3 <- coeficiente_ma1_arima / desv_est_ma1
p3 <- 2 * pt(-abs(t3), df = modelo_arima$nobs - 2)

#Me centro en el modelo GARCH

#Criterios de información

modeloGarch <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                          mean.model = list(armaOrder = c(0, 0)))
Garch <- ugarchfit(modeloGarch, serie_estacionaria_2)
infocriteria(Garch)

#Complementariamente hayo las varianzass del modelo ajustado

varianzas <- vcov(Garch)
varianzas

# Análisis de residuos ----------------------------------------------------

#Modelo ARIMA

#Empiezo con test de Shapiro-Wilk para evaluar si los residuos tienen una distribución normal

shapiro.test(modelo_arima$residuals)

#Aplico la función checkresiduals para entre otros obtener un test de Ljung-Box

checkresiduals(modelo_arima)

#Aplico la función tsdiag paralelamente par verificar que obtengo lso mismos resultados que con la función anterior
par(mfrow=c(2,2), mar=c(4, 4, 2, 1), oma=c(0, 0, 2, 0))
tsdiag(modelo_arima)

#Procedemos a grafical la autocorrelación parcial de los residuos

pacf(modelo_arima_2$residuals)

#Por último creamos un gráfico de cuantiles de los residuos contra una distribución normal teórica

qqnorm(modelo_arima_2$residuals)
qqline(modelo_arima_2$residuals)

#Replico estos análisis para el modelo garch

#Creo los residuos del modelo que convierto en vector ya que de primeras obtengo una matriz

residuos <- residuals(Garch)
residuos_vec <- as.numeric(residuos)

#Test de Shapiro_Wilk

resultado_test <- shapiro.test(residuos_vec)


#Test de Ljung-Box

checkresiduals(residuos_vec)


#Autocorrelación parcial de los residuos

pacf(residuos)

#qqnorm y qqline

qqnorm(residuos)
qqline(residuos)


# Movimientos irregulares -------------------------------------------------

# Comparamos las varianzas de la serie temporal y la del ruido del modelo arima:

#Por comodidad voy a crear una variable que englobe los residuos del modelo ARIMA

residuos_arima <- modelo_arima_2$residuals

#Calculo las varianzas
var_serie <- var(datos$Precio)
var_ruido <- var(residuos_arima)

#Analizo de una manera visual cómo los residuos del modelo ARIMA se ajustan a la serie temporal original

#Parece que el objeto residuos_arima tiene una fila menos que el objeto datos, de manera que reduzco el tamaño de éste

datos <- tail(datos, n = length(residuos_arima))

#Creo un dataframe 
df_residuos_arima <- data.frame(Fecha = datos$Fecha, Precio = datos$Precio, Residuos = residuos_arima)

# Grafico la serie temporal original y los residuos
grafico_residuos_arima <- plot_ly(data = df_residuos_arima, x = ~Fecha, y = ~Precio, type = 'scatter', mode = 'lines', name = 'Serie temporal original') %>%
  add_trace(y = ~Residuos, name = 'Residuos del modelo ARIMA', line = list(color = 'red'))

# Doy estilo al gráfico
grafico_residuos_arima <- grafico_residuos_arima %>% layout(title = list(text = "Serie temporal original con residuos del modelo ARIMA"),
                                                            xaxis = list(title = "Fecha"),
                                                            yaxis = list(title = "Precio"))



#Repito estos procesos para el modelo GARCH

var_serie <- var(datos$Precio)

var_GARCH <- var(residuals(Garch))

#Grafico las 2 series

df_residuos_GARCH <- data.frame(Fecha = datos$Fecha, Precio = datos$Precio, Residuos = residuals(Garch))

# Grafico la serie temporal original y los residuos
grafico_residuos_GARCH <- plot_ly(data = df_residuos_GARCH, x = ~Fecha, y = ~Precio, type = 'scatter', mode = 'lines', name = 'Serie temporal original') %>%
  add_trace(y = ~Residuos, name = 'Residuos del modelo GARCH', line = list(color = 'red'))

# Doy estilo al gráfico
grafico_residuos_GARCH <- grafico_residuos_GARCH %>% layout(title = list(text = "Serie temporal original con residuos del modelo GARCH"),
                                                            xaxis = list(title = "Fecha"),
                                                            yaxis = list(title = "Precio"))


# Predicción para un intervalo temporal -----------------------------------

#Obtengo los siguientes 15 valores reales de los precios de las acciones

getSymbols("COG.TO", src = "yahoo", from = "2023-02-12", to = "2023-02-25")
nuevos_datos <- data.frame(Fecha = index(COG.TO), Precio = coredata(COG.TO)[,4])
nuevos_datos$Fecha <- as.Date(nuevos_datos$Fecha, format = "%Y-%m-%d")
plot(nuevos_datos)

#Predigo el precio para los siguientes 15 días siguiendo el modelo ARIMA

pred_arima <- predict(modelo_arima, n.ahead = 9)

#Los meto en un dataframe

pred_arima_df <- data.frame(Fecha = index(COG.TO), Precio = pred_arima)


#Agrego una columna que muestra la relación entre el precio estimado y el precio real

pred_arima_df <- mutate(pred_arima_df, Diferencia = abs(Precio.pred-nuevos_datos$Precio))



# Creo un dataframe con las fechas estiamdas
pred_arima_df <- data.frame(Fecha = index(COG.TO), Precio = pred_arima)
colnames(pred_arima_df)


# Le añado los datos reales y la diferencia
pred_arima_df$nuevos_datos_Precio <- nuevos_datos$Precio
pred_arima_df$Diferencia <- abs(pred_arima_df$Precio.pred - pred_arima_df$nuevos_datos_Precio)

# Lo grafico

plot_ly(pred_arima_df, x = ~Fecha) %>%
  add_trace(y = ~Precio.pred, name = "Precio Predicho", type = "bar") %>%
  add_trace(y = ~nuevos_datos_Precio, name = "Precio Real", type = "bar") %>%
  add_trace(y = ~Diferencia, name = "Diferencia Absoluta", type = "bar") %>%
  layout(yaxis = list(type = "log"))



#Ahora prosigo con el GARCH, al cual entreno


COG.TO <- getSymbols("COG.TO", src = "yahoo", from = "2020-01-11", to = "2023-02-11",adjust=TRUE, auto.assign = FALSE)
COG.TO_2 <- getSymbols("COG.TO", src = "yahoo", from = "2023-02-12", to = "2023-02-25",adjust=TRUE, auto.assign = FALSE)
COG.TO <- Ad(COG.TO) # Solo precio
plot(COG.TO)
require(tseries)
library(PerformanceAnalytics)

COG.TO_ret <- CalculateReturns(COG.TO) %>% na.omit() # Volatilidad

# Utilizo el modelo previamente seleccionado (1,1)

modeloGarch <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                          mean.model = list(armaOrder = c(0, 0)))
# Entrenamos el modelo con la serie descarga al inicio del script

Garch <- ugarchfit(modeloGarch, COG.TO_ret)
pred = ugarchforecast(Garch, n.ahead=9)

# Extraigo los precios predichos del objeto "pred"
precios_predichos <- as.numeric(pred@forecast$seriesFor[1:9, 1])

# Creo un data frame con los precios reales y predichos
pred_garch_df <- data.frame(Fecha = index(COG.TO_2), 
                            Precio_Real = as.numeric(COG.TO_2),
                            Precio_Predicho = precios_predichos)

# Agrego una columna con la diferencia absoluta entre los precios reales y predichos
pred_garch_df$Diferencia <- abs(pred_garch_df$Precio_Real - pred_garch_df$Precio_Predicho)


#Grafico el df


plot_ly(pred_garch_df, x = ~Fecha) %>%
  add_trace(y = ~Precio_Predicho, name = "Precio Predicho", type = "bar") %>%
  add_trace(y = ~Precio_Real, name = "Precio Real", type = "bar") %>%
  add_trace(y = ~Diferencia, name = "Diferencia Absoluta", type = "bar") %>%
  layout(yaxis = list(type = "log"))




