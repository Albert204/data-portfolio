load("C:/Users/Alberto/Documents/Clase/Analisis de muestras/datos_2022/R/EES_2022.RData")
ls()

retri = Microdatos$RETRINOIN
anos = Microdatos$ANOS2
anos = as.numeric(anos)
anti = Microdatos$ANOANTI

datosp = data.frame(retri,anos,anti)
summary(datosp)

# Parámetros del muestreo
N <- length(retri)        # Tamaño de la población
z <- 1.96                 # Nivel de confianza al 95%
n <- 600                  # Tamaño de la muestra

set.seed(123) # Fija la semilla para reproducibilidad
f = n/N

# parametros y error retribucion
totalp = sum(retri)#este es el total de la muestra que tomamos como poblacion pero hay que estimar la poblacion real,Cual es la N real?
mediap = mean(retri)
var(retri)
sd(retri)
# error media retri ajustada con f(inecesario por el tamaño de la muestra<5%)
varp = ((1-f)/n)*var(retri)#varianza de la media
sdp = sqrt(varp)


rango_retri <- range(retri)
quantile_retri <- quantile(retri, probs = c(0.25, 0.5, 0.75))
cv_retri <- sd(retri) / mean(retri)#ya nos dice que va a tener alta dispersion

# parametros y error antiguedad
mean(anti)
var(anti)
rango_anti <- range(anti)
quantile_anti <- quantile(anti, probs = c(0.25, 0.5, 0.75))
cv_anti <- sd(anti) / mean(anti)#ya nos dice que va a tener alta dispersion

# proporciones edad por tramos
total_filas <- length(anos)
prop19 = sum(anos==1)/total_filas #proporcion menores de 20 años :19
prop29 = sum(anos==2)/total_filas #20<29
prop39 = sum(anos==3)/total_filas #30<39
prop49 = sum(anos==4)/total_filas #40<49
prop59 = sum(anos==5)/total_filas #50<59
prop100 = sum(anos==6)/total_filas #60:

prop = list(prop19, prop29, prop39, prop49, prop59, prop100)
prop
(prop_cumulativa <- cumsum(unlist(prop)))

mean(anos)
var(anos)
sd(anos)
# -----------------------M.A.S--------------------------

muestra <- sample(retri, n, replace = FALSE)

summary(muestra)

# PARA LA MEDIA
(var_est_x = ((1-f)/n)*var(muestra))#esta da un numero muy alta ya que al hacer cuadrados de los valores extremos cambia significativamente el total
(sd_est_x = sqrt(var_est_x))#la sd en este caso es razonablemente baja respecto al rango de valores que estamos trabajando

media_muestra = mean(muestra)
media_muestra

#sd es en este caso el EM
er_x = sd_est_x/media_muestra#error relativo
er_x#da un valor menor a 5% lo cual indica que la media muestral es bastante precisa

int95 = c(media_muestra - z*sd_est_x, media_muestra + z*sd_est_x)
int95#para z=1.96 que es 95%

(sesgo_x <- media_muestra - mediap)
(sesgo_var <- var_est_x - varp)
(sesgo_sd <- sd_est_x - sdp)

#grafico intervalo de confianza media estimada
plot(1, type = "n", xlab = "Media", ylab = "Valores", xlim = range(c(int95[1], int95[2])), 
     ylim = c(0.9, 1.1), main = "Intervalo de Confianza (Media)")
segments(int95[1], 1, int95[2], 1, col = "blue", lwd = 2) # IC
points(media_muestra, 1, col = "red", pch = 19, cex = 1.5) # Media muestral
abline(v = mediap, col = "green", lty = 2, lwd = 2) # Media poblacional
legend("topright", legend = c("Media Poblacional", "Media Muestral", "Intervalo de Confianza"), 
       col = c("green", "red", "blue"), lty = c(2, NA, 1), pch = c(NA, 19, NA), lwd = c(2, NA, 2))


# PARA EL TOTAL
(var_est_t = N^2*((1-f)/n)*var(muestra))
(sd_est_t = sqrt(var_est_t))

(total_muestra = sum(muestra))
(total_estimado = N*media_muestra)

er_t = sd_est_t/total_estimado#error relativo
er_t# igual que el de la media prq escalamos por N^2

int95_t = c(total_estimado - z*sd_est_t, total_estimado + z*sd_est_t)
int95_t#para z=1.96 que es 95%

(sesgo_t <- total_estimado - totalp)#500m para el total de 7b entra en el error permitido

# -----------------------graficos M.A.S--------------------------

#grafico intervalo de confianza total estimado, de nuevo se escala por N por eso son proporcionales
plot(1, type = "n", xlab = "Total", ylab = "Valores", xlim = range(c(int95_t[1], int95_t[2])), 
     ylim = c(0.9, 1.1), main = "Intervalo de Confianza (Total Estimado)")
segments(int95_t[1], 1, int95_t[2], 1, col = "blue", lwd = 2) # IC
points(total_estimado, 1, col = "red", pch = 19, cex = 1.5) # Total estimado
abline(v = totalp, col = "green", lty = 2, lwd = 2) # Total poblacional
legend("topright", legend = c("Total Poblacional", "Total Estimado", "Intervalo de Confianza"), 
       col = c("green", "red", "blue"), lty = c(2, NA, 1), pch = c(NA, 19, NA), lwd = c(2, NA, 2))



# -----------------------estratificado proporcional--------------------------

# Se ha preferido usar afijación proporcional  frente a neyman porque garantiza la representatividad de todos los estratos, 
# asegurando que incluso los estratos pequeños (como el Estrato 1) estén incluidos en la muestra mientras que en neyman tendria 0 observaciones.
# y refleja fielmente las proporciones poblacionales, lo cual es esencial en análisis donde todos los grupos tienen relevancia. 

size_p1 = round(n * prop19)#tan solo 2 observaciones de este estrato
size_p2 = round(n * prop29)#61 observaciones
size_p3 = round(n * prop39)#123 observaciones
size_p4 = round(n * prop49)#194 observaciones
size_p5 = round(n * prop59)#165 observaciones
size_p6 = round(n * prop100)#55 observaciones

muestra_p1 = sample(datosp$retri[datosp$anos == 1], size_p1, replace = FALSE)
muestra_p2 = sample(datosp$retri[datosp$anos == 2], size_p2, replace = FALSE)
muestra_p3 = sample(datosp$retri[datosp$anos == 3], size_p3, replace = FALSE)
muestra_p4 = sample(datosp$retri[datosp$anos == 4], size_p4, replace = FALSE)
muestra_p5 = sample(datosp$retri[datosp$anos == 5], size_p5, replace = FALSE)
muestra_p6 = sample(datosp$retri[datosp$anos == 6], size_p6, replace = FALSE)

muestra_ep = c(muestra_p1,muestra_p2,muestra_p3,muestra_p4,muestra_p5, muestra_p6)
muestra_ep


medias_estratos = c(mean(muestra_p1), mean(muestra_p2), mean(muestra_p3), mean(muestra_p4), mean(muestra_p5), mean(muestra_p6))
tamanos_muestra = c(size_p1, size_p2, size_p3, size_p4, size_p5, size_p6)

media_estp = sum(tamanos_muestra * medias_estratos) / n
media_estp # estimación de la media con la fórmula de afijación proporcional

varianzas_estratos = c(var(muestra_p1), var(muestra_p2), var(muestra_p3), var(muestra_p4), var(muestra_p5), var(muestra_p6))
tamanos_poblacionales = c(sum(datosp$anos == 1), sum(datosp$anos == 2), sum(datosp$anos == 3), sum(datosp$anos == 4), sum(datosp$anos == 5), sum(datosp$anos == 6))
pesos_poblacionales = tamanos_poblacionales / N

# estimación de la varianza de la media con la fórmula de afijación proporcional
varianza_media_estp = (1 - f) / n * sum(pesos_poblacionales^2 * varianzas_estratos)

# estimación del total con la fórmula de afijación proporcional
total_estp = sum(tamanos_muestra * medias_estratos) / f

# estimación de la varianza del total con la fórmula de afijación proporcional
varianza_total_estp = (1 - f) / n * sum(tamanos_poblacionales^2 * varianzas_estratos)

# Resultados
cat("Media estimada (estp):", media_estp, "\n")
cat("Varianza de la media (estp):", varianza_media_estp, "\n")
cat("Desviación estándar de la media (estp):", sqrt(varianza_media_estp), "\n")
cat("Total estimado (estp):", total_estp, "\n")
cat("Varianza del total (estp):", varianza_total_estp, "\n")
cat("Desviación estándar del total (estp):", sqrt(varianza_total_estp), "\n")

(ic_estp = c(media_estp - z * sqrt(varianza_media_estp), media_estp + z * sqrt(varianza_media_estp)))
(ic_estp_t = c(total_estp - z * sqrt(varianza_total_estp), total_estp + z * sqrt(varianza_total_estp)))


# -----------------------gráficas estratificado proporcional--------------------------
# Nombres de los estratos por edades
estratos <- c("<20 años", "20-29 años", "30-39 años", "40-49 años", "50-59 años", "60+ años")

# Gráfico de barras para las medias por estrato
par(mar = c(5, 5, 4, 5))  # Ajustar márgenes para dar espacio al eje secundario
barplot(medias_estratos, names.arg = estratos, col = "skyblue", 
        ylim = c(0, max(medias_estratos) * 1.2), 
        ylab = "Media de Retribuciones", 
        main = "Medias y Tamaños Muestrales por Edad (Proporcional)",
        cex.names = 0.8, cex.axis = 0.8)

# Gráfico combinado: Medias y Tamaños Muestrales
par(new = TRUE)
plot(tamanos_muestra, type = "o", pch = 16, col = "red", 
     xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     ylim = c(0, max(tamanos_muestra) * 1.2))
axis(4, at = seq(0, max(tamanos_muestra), length.out = 5), 
     labels = seq(0, max(tamanos_muestra), length.out = 5), las = 1, cex.axis = 0.8)
mtext("Tamaño Muestral", side = 4, line = 3, col = "red", cex = 0.8)

# Leyenda para el gráfico combinado
legend("top", inset = c(0, -0.1), legend = c("Media de Retribuciones", "Tamaño Muestral"), 
       fill = c("skyblue", NA), border = NA, lty = c(0, 1), pch = c(NA, 16), col = c("skyblue", "red"),
       bty = "n", cex = 0.8)

# Gráfico de barras para las varianzas por estrato
barplot(varianzas_estratos, names.arg = estratos, col = "purple", 
        ylim = c(0, max(varianzas_estratos) * 1.2), 
        ylab = "Varianza de Retribuciones", 
        main = "Varianzas de Retribuciones por Edad (Proporcional)",
        cex.names = 0.8, cex.axis = 0.8)

# -----------------------estratificado uniforme--------------------------

# El uso complementario de afijación uniforme se justifica para garantizar un tamaño mínimo de muestra en todos los estratos, 
# asegurando representatividad incluso en los estratos más pequeños o con baja proporción poblacional como el 1. 
# Esto permite obtener información útil de todos los grupos y evita que los estratos pequeños queden excluidos, 
# lo que sería importante para análisis comparativos o específicos de cada estrato.

r = 100
L = 6

muestra_f1 = sample(datosp$retri[datosp$anos == 1], r, FALSE)
muestra_f2 = sample(datosp$retri[datosp$anos == 2], r, FALSE)
muestra_f3 = sample(datosp$retri[datosp$anos == 3], r, FALSE)
muestra_f4 = sample(datosp$retri[datosp$anos == 4], r, FALSE)
muestra_f5 = sample(datosp$retri[datosp$anos == 5], r, FALSE)
muestra_f6 = sample(datosp$retri[datosp$anos == 6], r, FALSE)

muestra_ef = c(muestra_f1,muestra_f2,muestra_f3,muestra_f4,muestra_f5, muestra_f6)
muestra_ef


varianzas_estratosu <- c(var(muestra_f1), var(muestra_f2), var(muestra_f3), var(muestra_f4), var(muestra_f5), var(muestra_f6))

# Varianza del total 
varianza_total_ef <- sum(tamanos_poblacionales^2 * (1 - r / tamanos_poblacionales) * varianzas_estratosu / r)

# Varianza de la media 
varianza_media_ef <- sum(pesos_poblacionales^2 * (1 - r / tamanos_poblacionales) * varianzas_estratosu / r)

cat("Varianza del total estimada (afijación uniforme):", varianza_total_ef, "\n")
cat("Desviación estándar del total estimada (afijación uniforme):", sqrt(varianza_total_ef), "\n")
cat("Varianza de la media estimada (afijación uniforme):", varianza_media_ef, "\n")
cat("Desviación estándar de la media estimada (afijación uniforme):", sqrt(varianza_media_ef), "\n")

medias_estratosu <- c(mean(muestra_f1), mean(muestra_f2), mean(muestra_f3), mean(muestra_f4), mean(muestra_f5), mean(muestra_f6))
medias_estratosu# Comparar las medias

cv_estratosu <- varianzas_estratosu^0.5 / medias_estratosu
cv_estratosu  # Comparar la desigualdad entre estratos

# -----------------------graficas estratificado uniforme--------------------------

# Datos para el gráfico con rangos de edades
estratos <- c("<20 años", "20-29 años", "30-39 años", "40-49 años", "50-59 años", "60+ años")

# Crear el gráfico
par(mar = c(5, 5, 4, 5))  # Ajustar márgenes para dejar espacio al eje secundario

# Gráfico de barras para las medias
barplot(medias_estratosu, names.arg = estratos, col = "skyblue", 
        ylim = c(0, max(medias_estratosu) * 1.2), ylab = "Media de Retribuciones", 
        main = "Comparación de Retribuciones y Desigualdad por Edades",
        cex.names = 0.8, cex.axis = 0.8)  # Reducir tamaño del texto para claridad

# Agregar el eje secundario para el coeficiente de variación
par(new = TRUE)
plot(cv_estratosu * max(medias_estratosu), type = "o", pch = 16, col = "red", 
     xaxt = "n", yaxt = "n", xlab = "", ylab = "", ylim = c(0, max(cv_estratosu) * max(medias_estratosu) * 1.2))
axis(4, at = seq(0, max(cv_estratosu) * max(medias_estratosu), length.out = 5), 
     labels = round(seq(0, max(cv_estratosu), length.out = 5), 2), las = 1, cex.axis = 0.8)
mtext("Coeficiente de Variación", side = 4, line = 3, col = "red", cex = 0.8)

# Leyenda ajustada para evitar solapamientos
legend("top", inset = c(0, -0.1), legend = c("Media de Retribuciones", "Coef. de Variación"), 
       fill = c("skyblue", NA), border = NA, lty = c(0, 1), pch = c(NA, 16), col = c("skyblue", "red"),
       bty = "n", cex = 0.8)

# -----------------------sistematico--------------------------

k= trunc(N/n)#metodo de redondeo
pi_i = n/N


R <- sample(1:k, 1)  # arranque aleatorio
muestra_s <- retri[ seq(R, by = k, length.out = n) ] 
cat("Resumen de la muestra:\n")
print(summary(muestra_s))
cat("\nSuma de la muestra:", sum(muestra_s), "\n")


SCT <- sum( (retri - mediap)^2 )


# CONSTRUIR LA PARTICIÓN Y CALCULAR SCdentro
# partición sin desplazamiento
SCdentro <- 0

for(i in 1:k) {
  # Índices del grupo i: i, i+k, i+2k, ..., hasta N
  idx_grupo_i <- seq(i, N, by=k)
  subpop_i <- retri[idx_grupo_i]
  
  media_i <- mean(subpop_i)
  SCdentro <- SCdentro + sum((subpop_i - media_i)^2)
}

#CALCULAR LA HOMOGENEIDAD
homogeneidad <- 1 - ((N - 1)/(N - k)) * (SCdentro / SCT)

cat("\nValor de homogeneidad =", homogeneidad, "\n")#<0 mas eficiente que mas, >0 menos eficiente, =0 igual
#Los datos no guardan relación con el índice y, por ende, agruparlos “salteado” no aumenta la homogeneidad.


media_muestrals <- mean(muestra_s)
#Varianza muestral (s^2)
var_muestrals <- var(muestra_s)
fpc <- (1 - n / N)

#Varianza del estimador de la media, asumiendo M.A.S.:
var_media_s <- fpc * (var_muestrals / n)

se_media_s <- sqrt(var_media_s)

#Intervalo de confianza al 95% para la media
IC_media_inf <- media_muestrals - z * se_media_s
IC_media_sup <- media_muestrals + z * se_media_s

#Estimador del TOTAL poblacional
total_ests <- N * media_muestrals

#Varianza (y error estándar) del total
var_totals <- N^2 * var_media_s
se_totals <- N * se_media_s

#Intervalo de confianza al 95% para el total
IC_total_inf <- total_ests - z * se_totals
IC_total_sup <- total_ests + z * se_totals

#resultados
cat("Estimación de la media =", media_muestrals, "\n")
cat("Error estándar de la media =", se_media_s, "\n")
cat(sprintf("IC 95%% para la media: [%.2f, %.2f]\n", IC_media_inf, IC_media_sup))

cat("\nEstimación del total =", total_ests, "\n")
cat("Error estándar del total =", se_totals, "\n")
cat(sprintf("IC 95%% para el total: [%.2f, %.2f]\n", IC_total_inf, IC_total_sup))

# -----------------------razon y regresion--------------------------

indices_muestra <- sample(1:length(retri), n, replace = FALSE)
muestra_r_retri <- retri[indices_muestra]
muestra_r_anti <- anti[indices_muestra]


# Cálculo del estimador de razón
R_est = sum(muestra_r_retri) / sum(muestra_r_anti)
# Total estimado usando el estimador de razón
X_total = sum(anti) # Total poblacional de la variable auxiliar (anti)
Y_est = R_est * X_total
Y_mean_est = R_est*mean(anti)

# Medias muestrales
mean_retri = mean(muestra_r_retri)
mean_anti = mean(muestra_r_anti)


# Pendiente estimada para el estimador de regresión
b_est = sum((muestra_r_retri - mean(muestra_r_retri)) * (muestra_r_anti - mean(muestra_r_anti))) /
  sum((muestra_r_anti - mean(muestra_r_anti))^2)
# Total estimado usando el estimador de regresión
X_total = sum(anti) # Total poblacional de la variable auxiliar (X)
mean_retri = mean(muestra_r_retri) # Media muestral de Y
mean_anti = mean(muestra_r_anti)  # Media muestral de X

mu_est_RG <- mean_retri + b_est * (X_total / length(anti) - mean_anti)
Y_est_RG <- N * mu_est_RG

# Mostrar resultados
cat("Estimador de Razón (R_est):", R_est, "\n")
cat("Total estimado usando Razón (Y_est):", Y_est, "\n")
cat("Media estimada usando Razón (Y_mean_est):", Y_mean_est, "\n")
cat("Pendiente estimada (b_est):", b_est, "\n")
cat("Total estimado usando Regresión (Y_est_RG):", Y_est_RG, "\n")


#Varianzas de las medias por razon y regresion-----------------------------------------------
## A) Estimador de RAZÓN  -- usando la forma expandida:
##    Var( meanY ) = (1 - f)/n * (1/(n-1)) * [ R^2 * sum(x_i^2) + sum(y_i^2) - 2*R*sum(x_i*y_i) ]
##
##  donde:
##   - x_i, y_i son los datos muestrales
##   - R = sum(y_i)/sum(x_i)
##

# Sumas de cuadrados y producto cruzado en la muestra
sum_xi2    <- sum(muestra_r_anti^2)         # sum(x_i^2)
sum_yi2    <- sum(muestra_r_retri^2)        # sum(y_i^2)
sum_xi_yi  <- sum(muestra_r_anti * muestra_r_retri)  # sum(x_i * y_i)

var_ratio_meanY <- (1 - f) / n * (1/(n - 1)) * (
  R_est^2 * sum_xi2 + sum_yi2 - 2 * R_est * sum_xi_yi
)

# Error estándar
se_ratio_meanY <- sqrt(var_ratio_meanY)

cat("Var( mediaY, Est.Razón ) =", var_ratio_meanY, "\n")
cat("SE(  mediaY, Est.Razón ) =", se_ratio_meanY, "\n\n")


##
## B) Estimador de REGRESIÓN -- usando la forma (según lo comentado):
##    Var( meanY ) = (1 - f)/n * (n-2)/(n-1) * [ sum(y_i^2) - n * (meanY_mues^2) - b * Sxy_centrado ]
##  donde Sxy_centrado = sum( (x_i - bar{x})(y_i - bar{y}) ).
##

# sum(y_i^2) ya lo tenemos: sum_yi2
# mean_retri = media muestral de Y
# b_est = pendiente
# Para Sxy "centrado", definimos:
Sxy_centrado <- sum((muestra_r_anti - mean_anti) * (muestra_r_retri - mean_retri))
# equivale a: sum_xi_yi - n*mean_anti*mean_retri

var_reg_meanY <- (1 - f) / n * ((n - 2)/(n - 1)) * (
  sum_yi2 - n*(mean_retri^2) - b_est*Sxy_centrado
)

se_reg_meanY <- sqrt(var_reg_meanY)

cat("Var( mediaY, Est.Regresión ) =", var_reg_meanY, "\n")
cat("SE(  mediaY, Est.Regresión ) =", se_reg_meanY, "\n")

# -----------------------graficos razon y regresion--------------------------

est_means <- c(Razon = Y_mean_est, Regresion = mu_est_RG)
est_SE    <- c(Razon = se_ratio_meanY, Regresion = se_reg_meanY)

# Máximo para el eje Y considerando espacio para las barras de error
ylim_max <- max(est_means + 2*est_SE)

# Barplot con error bars
par(mfrow = c(1,1))  # Asegurarnos de un solo panel
bars_x <- barplot(
  est_means,
  beside = TRUE,
  col    = c("steelblue", "tomato"),
  names.arg = c("Est.Razón", "Est.Regresión"),
  ylim   = c(0, ylim_max),
  main   = "Comparación de la Media Estimada (+ Error Bars)",
  ylab   = "Media Estimada de Y"
)

# Agregamos barras de error (IC ~ 95%): 
arrows(
  x0 = bars_x,
  y0 = est_means - 1.96*est_SE,
  x1 = bars_x,
  y1 = est_means + 1.96*est_SE,
  angle = 90, code = 3, length = 0.1, col = "gray20"
)



var_ratio_total <- N^2 * var_ratio_meanY
se_ratio_total  <- sqrt(var_ratio_total)

var_reg_total   <- N^2 * var_reg_meanY
se_reg_total    <- sqrt(var_reg_total)

est_totals  <- c(Razon = Y_est, Regresion = Y_est_RG)
est_tot_se  <- c(Razon = se_ratio_total, Regresion = se_reg_total)

ylim_max2 <- max(est_totals + 2*est_tot_se)

bars_x2 <- barplot(
  est_totals,
  beside    = TRUE,
  col       = c("steelblue", "tomato"),
  names.arg = c("Est.Razón", "Est.Regresión"),
  ylim      = c(0, ylim_max2),
  main      = "Comparación del Total Estimado (+ Error Bars)",
  ylab      = "Total Estimado de Y"
)

arrows(
  x0 = bars_x2,
  y0 = est_totals - 1.96*est_tot_se,
  x1 = bars_x2,
  y1 = est_totals + 1.96*est_tot_se,
  angle = 90, code = 3, length = 0.1, col = "gray20"
)


a_est <- mean_retri - b_est * mean_anti

# Preparamos el plot (ejes, títulos, etc.)
plot(
  x = muestra_r_anti, y = muestra_r_retri,
  main = "Muestra (Xi vs. Yi) con Rectas de Razón y Regresión",
  xlab = "X (muestra antiguedad)", ylab = "Y (muestra retribución)",
  pch = 19, col = "darkgray"
)

# Recta de razón: pasa por el origen con pendiente R_est
abline(a = 0, b = R_est, col = "blue", lwd = 2)
# Recta de regresión: a_est + b_est * x
abline(a = a_est, b = b_est, col = "red", lwd = 2)

# Leyenda
legend(
  "topleft",
  legend = c("Recta Est.Razón (pasa por origen)","Recta Est.Regresión"),
  col    = c("blue", "red"),
  lty    = c(1, 1),
  lwd    = 2,
  bty    = "n"
)





###############################################################################
# BLOQUE DE COMPARACIÓN E INTERPRETACIÓN DE RESULTADOS (SIN REPETIR CÓDIGO)
###############################################################################
# En este bloque se utilizan únicamente las variables ya calculadas en el script.
# Se comparan estimaciones de la media y el total, así como sus errores estándar,
# sesgos y diferencias entre los diversos métodos de muestreo.
###############################################################################

# --------------------------------------------------------------------------------
# COMPARACIÓN DE ESTIMACIONES DE LA MEDIA
# --------------------------------------------------------------------------------
cat("\n================ COMPARACIÓN DE LA MEDIA ESTIMADA =================\n")

cat("\n--- 1) Muestreo Aleatorio Simple (M.A.S.) ---\n")
cat("Media muestral (media_muestra):", media_muestra, "\n")
cat("Sesgo respecto a la media poblacional (mediap):", sesgo_x, "\n")
cat("Error estándar aproximado (sd_est_x):", sd_est_x, "\n")
cat("Intervalo de confianza 95% (int95):",
    paste0("[", round(int95[1],2), ", ", round(int95[2],2), "]"), "\n")

cat("\n--- 2) Estratificado con Afijación Proporcional ---\n")
cat("Media estimada (media_estp):", media_estp, "\n")
cat("Diferencia respecto a la media poblacional (mediap):", media_estp - mediap, "\n")
cat("Desviación estándar (sqrt(varianza_media_estp)):", sqrt(varianza_media_estp), "\n")
cat("Intervalo de confianza 95%:",
    paste0("[", round(ic_estp[1],2), ", ", round(ic_estp[2],2), "]"), "\n")

cat("\n--- 3) Estratificado con Afijación Uniforme ---\n")
cat("Varianza de la media (varianza_media_ef):", varianza_media_ef, "\n")
cat("Desviación estándar de la media (sqrt(varianza_media_ef)):", sqrt(varianza_media_ef), "\n")
cat("Nota: No se calculó aquí la media global final para este método, aunque se puede\n",
    "obtener a partir de 'muestra_ef'. Se muestran la varianza y su raíz cuadrada.\n")

cat("\n--- 4) Muestreo Sistemático ---\n")
cat("Media muestral (media_muestrals):", media_muestrals, "\n")
cat("Diferencia respecto a la media poblacional (mediap):", media_muestrals - mediap, "\n")
cat("Error estándar de la media (se_media_s):", se_media_s, "\n")
cat("Intervalo de confianza 95%:",
    paste0("[", round(IC_media_inf,2), ", ", round(IC_media_sup,2), "]"), "\n")
cat("Índice de homogeneidad calculado (homogeneidad):", homogeneidad, "\n",
    "Interpretación: valores negativos suelen indicar mayor eficiencia que el M.A.S.,\n",
    "positivos menor eficiencia, y cercanos a cero, similar eficiencia.\n")

cat("\n--- 5) Estimador de Razón ---\n")
cat("Media estimada por razón (Y_mean_est):", Y_mean_est, "\n")
cat("Diferencia respecto a la media poblacional (mediap):", Y_mean_est - mediap, "\n")
cat("Error estándar (se_ratio_meanY):", se_ratio_meanY, "\n")

cat("\n--- 6) Estimador de Regresión ---\n")
cat("Media estimada por regresión (mu_est_RG):", mu_est_RG, "\n")
cat("Diferencia respecto a la media poblacional (mediap):", mu_est_RG - mediap, "\n")
cat("Error estándar (se_reg_meanY):", se_reg_meanY, "\n")


# --------------------------------------------------------------------------------
# COMPARACIÓN DE ESTIMACIONES DEL TOTAL
# --------------------------------------------------------------------------------
cat("\n================ COMPARACIÓN DEL TOTAL ESTIMADO =================\n")

cat("\n--- 1) Muestreo Aleatorio Simple (M.A.S.) ---\n")
cat("Total estimado (total_estimado):", total_estimado, "\n")
cat("Sesgo respecto al total poblacional (totalp):", sesgo_t, "\n")
cat("Desviación estándar del total (sd_est_t):", sd_est_t, "\n")
cat("Intervalo de confianza 95% (int95_t):",
    paste0("[", round(int95_t[1],2), ", ", round(int95_t[2],2), "]"), "\n")

cat("\n--- 2) Estratificado con Afijación Proporcional ---\n")
cat("Total estimado (total_estp):", total_estp, "\n")
cat("Varianza del total (varianza_total_estp):", varianza_total_estp, "\n")
cat("Desviación estándar del total:", sqrt(varianza_total_estp), "\n")
cat("IC 95% del total:",
    paste0("[", round(ic_estp_t[1],2), ", ", round(ic_estp_t[2],2), "]"), "\n")

cat("\n--- 3) Estratificado con Afijación Uniforme ---\n")
cat("Varianza del total (varianza_total_ef):", varianza_total_ef, "\n")
cat("Desviación estándar del total (sqrt(varianza_total_ef)):", sqrt(varianza_total_ef), "\n")
cat("Nota: No se muestra el total estimado en este método porque no se\n",
    "definió explícitamente en el script, aunque se puede derivar a partir\n",
    "de las medias y tamaños de cada estrato.\n")

cat("\n--- 4) Muestreo Sistemático ---\n")
cat("Total estimado (total_ests):", total_ests, "\n")
cat("Error estándar del total (se_totals):", se_totals, "\n")
cat("IC 95% para el total:",
    paste0("[", round(IC_total_inf,2), ", ", round(IC_total_sup,2), "]"), "\n")

cat("\n--- 5) Estimador de Razón ---\n")
cat("Total estimado por razón (Y_est):", Y_est, "\n")
cat("Varianza del total (var_ratio_total):", N^2 * var_ratio_meanY, "\n")
cat("Desviación estándar del total (se_ratio_total):", sqrt(N^2 * var_ratio_meanY), "\n")

cat("\n--- 6) Estimador de Regresión ---\n")
cat("Total estimado por regresión (Y_est_RG):", Y_est_RG, "\n")
cat("Varianza del total (var_reg_total):", N^2 * var_reg_meanY, "\n")
cat("Desviación estándar del total (se_reg_total):", sqrt(N^2 * var_reg_meanY), "\n")


###############################################################################
# CONCLUSIONES FINALES (SOLO CON LOS RESULTADOS ACTUALES)
###############################################################################
# A continuación se presentan las conclusiones generales sobre las estimaciones
# de la media y el total de la variable “retribución” (retri), comparando los
# diferentes métodos de muestreo y estimación implementados.
#
# Se observan los valores de la media o total estimado, su sesgo (o diferencia
# respecto al valor poblacional), y los errores estándar (o desviaciones) que
# permiten evaluar la precisión de cada método.

cat("\n================ CONCLUSIONES SOBRE LA MEDIA =================\n")

cat("\n1) M.A.S. (Muestreo Aleatorio Simple)\n",
    "   - Media muestral ~ 30,398.5\n",
    "   - Sesgo respecto a la media poblacional: ~ +2,126\n",
    "   - Error estándar ~ 1,464\n",
    "   - IC 95%: [27,528.94 ; 33,268.05]\n",
    "   => La estimación está algo desplazada hacia arriba en comparación\n",
    "      con la media real, pero su precisión (error ~1,464) es razonable\n",
    "      dado el rango de la variable.\n")

cat("\n2) Estratificado con Afijación Proporcional\n",
    "   - Media estimada ~ 28,500.73\n",
    "   - Diferencia respecto a la media real: ~ +228\n",
    "   - Desviación estándar ~ 453.51\n",
    "   - IC 95%: [27,611.85 ; 29,389.62]\n",
    "   => Logra un sesgo muy bajo (+228) y una desviación estándar pequeña\n",
    "      (~453), indicando buena precisión y menor dispersión que el M.A.S.\n")

cat("\n3) Estratificado con Afijación Uniforme\n",
    "   - Varianza de la media: 1,759,250\n",
    "   - Desviación estándar de la media: ~1,326.37\n",
    "   => Asegura un tamaño mínimo por estrato, pero no se muestra la media\n",
    "      global final. Comparando la dispersión (~1,326) se ve más reducida\n",
    "      que en el M.A.S. (aunque no tanto como en la afijación proporcional).\n")

cat("\n4) Muestreo Sistemático\n",
    "   - Media muestral ~ 28,860.9\n",
    "   - Diferencia respecto a la media real: ~ +588\n",
    "   - Error estándar ~ 1,007\n",
    "   - IC 95%: [26,886.45 ; 30,835.36]\n",
    "   - Índice de homogeneidad ~ 3.52e-05 (prácticamente 0)\n",
    "   => Indica eficiencia similar al M.A.S.; el sesgo (+588) es moderado,\n",
    "      aunque el error estándar es menor que con M.A.S.\n")

cat("\n5) Estimador de Razón\n",
    "   - Media estimada ~ 27,532.86\n",
    "   - Diferencia respecto a la media real: ~ -740\n",
    "   - Error estándar ~ 1,091\n",
    "   => Utilizar la antigüedad como variable auxiliar corrige parcialmente\n",
    "      la estimación, aunque existe un sesgo hacia abajo (-740). El\n",
    "      error estándar es algo menor que en M.A.S.\n")

cat("\n6) Estimador de Regresión\n",
    "   - Media estimada ~ 27,652.55\n",
    "   - Diferencia respecto a la media real: ~ -620\n",
    "   - Error estándar ~ 19,779.58 (notablemente alto)\n",
    "   => Aunque el sesgo es moderado (-620), la desviación estándar aparece\n",
    "      muy elevada (~19,780). Esto puede indicar que la regresión no está\n",
    "      capturando toda la variabilidad con la variable auxiliar y genera\n",
    "      una alta incertidumbre en la media muestral. Revisar la correlación\n",
    "      o ajuste del modelo podría ser necesario.\n")

cat("\n================ CONCLUSIONES SOBRE EL TOTAL =================\n")

cat("\n1) M.A.S.\n",
    "   - Total estimado ~ 7.31e9\n",
    "   - Sesgo respecto al total real: ~ +5.11e8\n",
    "   - Error estándar ~ 3.52e8\n",
    "   - IC 95%: [6.62e9 ; 8.00e9]\n",
    "   => El sesgo es considerable, y el intervalo cubre un rango amplio.\n")

cat("\n2) Estratificado Proporcional\n",
    "   - Total estimado ~ 6.85e9\n",
    "   - Varianza del total ~ 1.19e16 => desviación estándar ~ 1.09e8\n",
    "   - IC 95%: [6.64e9 ; 7.07e9]\n",
    "   => Apreciablemente menor error respecto a M.A.S. (dentro de ~1.09e8).\n",
    "      Mejor precisión global.\n")

cat("\n3) Estratificado Uniforme\n",
    "   - Varianza total ~ 1.02e17 => desviación estándar ~ 3.19e8\n",
    "   => No se mostró el total estimado final, pero la dispersión (~3.19e8)\n",
    "      es algo inferior a la de M.A.S. (3.52e8), aunque no tan ajustada\n",
    "      como en el proporcional.\n")

cat("\n4) Muestreo Sistemático\n",
    "   - Total estimado ~ 6.94e9\n",
    "   - Error estándar ~ 2.42e8\n",
    "   - IC 95%: [6.46e9 ; 7.42e9]\n",
    "   => Menor rango que M.A.S.; se logra una precisión intermedia.\n")

cat("\n5) Estimador de Razón\n",
    "   - Total estimado ~ 6.62e9\n",
    "   - Desviación estándar ~ 2.62e8\n",
    "   => Comparado con M.A.S. (3.52e8), reduce el error.\n")

cat("\n6) Estimador de Regresión\n",
    "   - Total estimado ~ 6.65e9\n",
    "   - Desviación estándar ~ 4.76e9\n",
    "   => El error es muy grande, reflejando la elevada varianza observada\n",
    "      en la estimación de la media por regresión. Podría no ser la\n",
    "      mejor alternativa en este escenario o con la variable auxiliar usada.\n")

cat("\n================ SÍNTESIS =================\n",
    "• El Estratificado Proporcional destaca por su baja desviación estándar\n",
    "  tanto en la media como en el total, lo que indica alta precisión.\n",
    "• El Sistemático y el Ratio también muestran mejoras respecto al M.A.S.,\n",
    "  pero el Estimador de Regresión presenta una varianza demasiado alta,\n",
    "  posiblemente por un ajuste inadecuado o correlación insuficiente.\n",
    "• Finalmente, M.A.S. sirve de referencia base, y la Afijación Uniforme\n",
    "  garantiza muestras mínimas en estratos aunque no sea tan eficiente\n",
    "  como la afijación proporcional en este caso.\n")
###############################################################################



