#crear matriz aleatoria
#datos = rlnorm(400,-1/8,1/2)
#saveRDS(datosm, "datos.rds")
datos = readRDS("datos.rds")
datosm = matrix(datos,10,40)
View(datosm)
apply(datosm,1,mean)
mean (datosm)
media = mean (datosm)
estadistico = exp(mean(log(datosm)))
estadistico
hist(datosm)

#Calcular estadistico mediana geometrica
p1m = apply(datosm, 2, log)
View(p1m)

p2m = matrix(apply(p1m, 2, mean), 1, 40)
View(p2m)

Valores_estadistico = matrix(apply(p2m, 2, exp), 1, 40)
p3m = Valores_estadistico
View(Valores_estadistico)
hist(Valores_estadistico)

#media estadistico
MD = mean(p3m)
MD

#media poblacional
MD_pobl = mean(datosm)
MD_pobl

#desviación estadistico
SD1 = sqrt(sum((p3m - MD)^2) / length(p3m))
SD1
VAR1 = SD1^2
VAR1
#desviación poblacional
SD = sqrt(sum((datosm - mean(datosm))^2) / length(datosm))
SD
VAR = SD^2
VAR
#Estadistico media muestral ej8
Est8 = apply(datosm,2,mean)
Est8
hist(Est8)
MD8 = mean(Est8) #media estadistico (media muestral)
MD8
SD8 = sqrt(sum((Est8 - mean(Est8))^2) / length(Est8))
SD8 #desviación estadistico (media muestral)
VAR8 = SD8^2
VAR8

#sesgo
sesgo_est = MD - 1
sesgo_est


#------------------------------------------------------------------------------


#precision
precision = 1/VAR1
precision

#error cuadratico
Error_cuadratico_medio = VAR1 + sesgo_est^2
Error_cuadratico_medio


#Consistencia--------------------------------------------------------------------


  # Simulación para verificar la consistencia
  set.seed(123)  # Establece una semilla para reproducibilidad

# Tamaño de muestra inicial y máximo
n_inicial <- 10
n_max <- 1000

# Número de simulaciones
num_simulaciones <- 1000

# Vector para almacenar estimaciones y varianzas
estimaciones <- numeric(num_simulaciones)
varianzas <- numeric(num_simulaciones)

for (i in 1:num_simulaciones) {
  # Genera una muestra de tamaño n
  n <- sample(n_inicial:n_max, 1)
  muestra <- rlnorm(400,-1/8,1/2)
  
  # Calcula la estimación y la varianza del estimador
  estimaciones[i] <- exp(mean(log(muestra)))  #estadistico
  varianzas[i] <- var(muestra) / n
}

# Verifica la consistencia
limite_estimaciones <- mean(estimaciones)
limite_varianzas <- mean(varianzas)

print(paste("Limite de las estimaciones:", limite_estimaciones))
print(paste("Limite de las varianzas:", limite_varianzas))


#Invarianza--------------------------------------------------------------------


set.seed(123)  # Establece una semilla para reproducibilidad

# Genera una muestra de una distribución log-normal
muestra <- rlnorm(1000,-1/8, 1/2)

# Calcula la mediana geométrica del conjunto de datos original
mediana_geometrica_original <- exp(mean(log(muestra)))

# Realiza transformaciones en la muestra
a <- 0.5  # Factor de traslación
b <- 2    # Factor de cambio de escala

# Muestra trasladada
muestra_traslada <- muestra + a
mediana_geometrica_traslada <- exp(mean(log(muestra_traslada)))

# Muestra escalada
muestra_escala <- muestra * b
mediana_geometrica_escala <- exp(mean(log(muestra_escala)))

# Verifica la invarianza
if (abs(mediana_geometrica_traslada - (mediana_geometrica_original + a)) < 0.001) {
  print("La mediana geométrica es invariante por traslaciones")
  print(abs(mediana_geometrica_traslada - (mediana_geometrica_original + a)))
}else{
  print("La mediana geométrica no es invariante por traslaciones")
  print(abs(mediana_geometrica_traslada - (mediana_geometrica_original + a)))
}

if (abs(mediana_geometrica_escala - (b * mediana_geometrica_original)) < 0.001) {
  print("La mediana geométrica es invariante por cambios de escala")
  print(abs(mediana_geometrica_escala - (b * mediana_geometrica_original)))
}else{
  print("La mediana geométrica no es invariante por traslaciones")
  print(abs(mediana_geometrica_escala - (b * mediana_geometrica_original)))
}


#Suficiencia--------------------------------------------------------------------


set.seed(123)  # Establece una semilla para reproducibilidad
n=400
# Genera una muestra de una distribución log-normal
muestra <- rlnorm(n,-1/8, 1/2)

# Calcula el estadístico T
Tu <- exp(mean(log(muestra)))

# Realiza una verificación de suficiencia utilizando el teorema de factorización
# La densidad de la muestra log-normal es conocida
# Densidad conjunta: f(x_1, x_2, ..., x_n | \theta) = (producto de densidades individuales)

# Factorización de la densidad conjunta
densidad_conjunta <- prod(dlnorm(muestra,-0.125, 0.5))

# Factorización de la densidad conjunta condicional a T
densidad_condicional_T <- dlnorm(Tu, -0.125, 0.5)^n

# mira si T es suficiente
tolerancia <- 1e-6  # Define una tolerancia para la comparación
if (abs(densidad_conjunta - densidad_condicional_T) < tolerancia) {
  print("El estadístico T es suficiente para efectuar inferencias sobre el parámetro desconocido θ")
  print((abs(densidad_conjunta - densidad_condicional_T)))
} else {
  print("El estadístico T no es suficiente para efectuar inferencias sobre el parámetro desconocido θ")
  print((abs(densidad_conjunta - densidad_condicional_T)))
}
#con una muestra de 100 dice que si es suficiente no se 

#Distribución contaminada y Robustez---------------------------------------------------------- 
  


#datosuni = runif(400,2000,2050)
#saveRDS(datosuni, "datos.unif")
datosuni = readRDS("datos.unif") #llamar los datos de la distribución uniforme
datosunim1 = matrix(datosuni,10,40)
datosunim = matrix(apply(datosunim1, 2, mean), 1, 40)
View(datosunim)
hist(datosunim)

MD_uni = mean(datosunim)
MD_uni

var_uni =(sqrt(sum((datosunim - MD_uni)^2) / length(datosunim)))^2
var_uni

n_muestra = 40 #tamaño de la muestra y proporción de cada distribución
porc_est = 0.95
porc_uni = 0.05

muestra_est = sample(datosm,n_muestra*porc_est) #crear muestras de cada distribución
muestra_uni = sample(datosunim1, n_muestra * porc_uni)
muestra_contaminada400 = c(muestra_est,muestra_uni)#crear la contaminada juntando las muestras
matriz_contaminada400 = matrix(muestra_contaminada400,10,40)
hist(matriz_contaminada400)

matriz_contaminada <- matrix(muestra_contaminada400,1,40) 
View(matriz_contaminada)
hist(matriz_contaminada)

hist(apply(matriz_contaminada400,2,mean))
View(apply(matriz_contaminada400,2,mean))
MD_contaminada = mean(matriz_contaminada)
MD_contaminada


var_contaminada =(sum((matriz_contaminada - MD_contaminada)^2) / length(matriz_contaminada))
var_contaminada

#estadístico_contaminado
c1m = log(matriz_contaminada400)
View(c1m)

c2m = matrix(apply(c1m, 2, mean), 1, 40)
View(c2m)

estadistico_contaminado = matrix(apply(c2m, 2, exp), 1, 40)
MD_estadistico_contaminado = mean(estadistico_contaminado)
MD_estadistico_contaminado #media estadístico contaminado
var_estadistico_contaminado = sum((estadistico_contaminado - MD_estadistico_contaminado)^2) / length(estadistico_contaminado)
var_estadistico_contaminado

View(estadistico_contaminado)
hist(estadistico_contaminado)


#-----------------------------------------------------------------------------

bMM <- log((exp(2 * media + VAR) * (exp(VAR) - 1)) / (exp(media + VAR/2)^2) + 1)
aMM <- log(exp(media + VAR/2)) - bMM/2

#aMM = log(mean(datosm)) - log(VAR)/2
aMM #mu por el metodo de los momentos
#bMM = log(VAR)
bMM #var por el metodo de los momentos

media = mean(datosm)

aMV = sum(log(datosm)) / length(datosm)
aMV #mu por el metodo de maxima verosimilitud
bMV = sum((log(datosm) - media)^2) / length(datosm)
bMV #var por el metodo de maxima verosimilitud

#E = esperanza, V = var. MM = metodo momentos, MV = maxima verosimilitud
Ea = exp(-1/8 + (1/4 / 2))
Ea
Vb = (exp(1/4) - 1) * exp(2 * -1/8 + 1/4)
Vb
EMM = exp(aMM + (bMM / 2))
EMM
VMM =(exp(bMM) - 1) * exp(2 * aMM + bMM)
VMM
EMV = exp(aMV + (bMV / 2))
EMV
VMV =(exp(bMV) - 1) * exp(2 * aMV + bMV)
VMV




conf90 = t.test(datosm, conf.level = 0.9)$conf.int #$conf.int -> mostrar solo el intervalo
conf90 #intervalo confianza 0.9
M90 = t.test(datosm, conf.level = 0.9)$estimate #$estimate -> media intervalo
M90
conf95 = t.test(datosm)$conf.int #$conf.int -> mostrar solo el intervalo
conf95 #intervalo confianza 0.95
M95 = t.test(datosm)$estimate #$estimate -> media intervalo
M95

# Gráfico para el intervalo de confianza del 90%
plot(1, M90, type = "o", ylim = range(c(conf90)), ylab = "Valor", xlab = "", main = "Intervalo de Confianza 90%")
segments(1, conf90[1], 1, conf90[2], lwd = 2)

# Gráfico para el intervalo de confianza del 95%
plot(1, M95, type = "o", ylim = range(c(conf95)), ylab = "Valor", xlab = "", main = "Intervalo de Confianza 95%")
segments(1, conf95[1], 1, conf95[2], lwd = 2)

#---------------------------------------------------------------------------------------


#datosconf <- matrix(rlnorm(3000, -1/8, 1/2), ncol = 100)
#saveRDS(datosconf, "datosconf.rds")
datosconf = readRDS("datosconf.rds")
View(datosconf)
mean(datosconf)

mediasconf = matrix(apply(datosconf,2,mean))
sdconf = matrix(apply(datosconf,2,sd))
n <- nrow(datosconf)

mediaestconf = apply(datosconf,2,function(x) exp(mean(log(x))))
mediaestconf
mean(mediaestconf)
sdestconf <- apply(datosconf, 2, function(x) sqrt(sum((x - rep(mediaestconf, length(x)))^2) / length(x)))
sdestconf
mean(exp(mean(log(datosconf))))


intervalo_media_muestral = function(confianza){
confianza <- confianza

intervalos_confianza <- matrix(NA, ncol = 4, nrow = ncol(datosconf))
colnames(intervalos_confianza) <- c("Media", "SD", "Inferior", "Superior")

# Calcular los intervalos de confianza para cada columna
for (i in 1:ncol(datosconf)){
  mdind <- mediasconf[i]
  sdind <- sdconf[i]
  Z <- qt((1 - confianza) / 2 + confianza, df = n - 1)
  
  # Calcular los límites del intervalo de confianza para la media actual
  inferior <- mdind - Z * (sdind / sqrt(n))
  superior <- mdind + Z * (sdind / sqrt(n))
  
  # Almacenar los resultados en la matriz de intervalos_confianza
  intervalos_confianza[i, ] <- c(mdind, sdind, inferior, superior)
}
View(intervalos_confianza)
}

intervalo_media_muestral(0.95)#6 por abajo
intervalo_media_muestral(0.9)#7 por abajo 3 por arriba


intervalo_estadistico = function(confianza){
  confianza <- confianza

  intervalos_confianzaest <- matrix(NA, ncol = 4, nrow = ncol(datosconf))
  colnames(intervalos_confianzaest) <- c("Media", "SD", "Inferior", "Superior")
  
  # Calcular los intervalos de confianza para cada columna
  for (i in 1:ncol(datosconf)){
    mdind <- mediaestconf[i]
    sdind <- sdestconf[i]
    Z <- qt((1 - confianza) / 2 + confianza, df = n - 1)
    
    # Calcular los límites del intervalo de confianza para la media actual
    inferior <- mdind - Z * (sdind / sqrt(n))
    superior <- mdind + Z * (sdind / sqrt(n))
    
    # Almacenar los resultados en la matriz de intervalos_confianza
    intervalos_confianzaest[i, ] <- c(mdind, sdind, inferior, superior)
  }
  View(intervalos_confianzaest)
}

intervalo_estadistico(0.95)#3 por abajo
intervalo_estadistico(0.90)#6 por abajo 1 por arriba

getwd()
