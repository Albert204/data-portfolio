
calcular_intervalo_confianza <- function(media, desviacion_estandar, n) {
  z <- 1.96  # Valor de Z para un intervalo de confianza del 95%
  
  error_estandar <- desviacion_estandar / sqrt(n)
  margen_de_error <- z * error_estandar
  
  limite_inferior <- media - margen_de_error
  limite_superior <- media + margen_de_error
  
  return(c(limite_inferior, limite_superior))
}

INTTOV1988 <- calcular_intervalo_confianza(15.1950495, 6.02848644, 100)
print(INTTOV1988)

INTTOV1989 <- calcular_intervalo_confianza(16.00792079, 6.090906762, 100)
print(INTTOV1989)

INTTOV1990 <- calcular_intervalo_confianza(16.30792079, 8.096835749, 100)
print(INTTOV1990)

INTTOV2015 <- calcular_intervalo_confianza(13.82673267, 6.95874535, 100)
print(INTTOV2015)

INTTOV2016 <- calcular_intervalo_confianza(13.14158416, 6.058661541, 100)
print(INTTOV2016)

INTTOV2017 <- calcular_intervalo_confianza(12.09207921, 5.576500964, 100)
print(INTTOV2017)

# -----------------------------------------------------------------------------

INTUSG1988 <- calcular_intervalo_confianza(18.5029703, 4.50283641, 100)
print(INTUSG1988)

INTUSG1989 <- calcular_intervalo_confianza(19.45544554, 5.038298378, 100)
print(INTUSG1989)

INTUSG1990 <- calcular_intervalo_confianza(19.73069307, 5.206807094, 100)
print(INTUSG1990)

INTUSG2015 <- calcular_intervalo_confianza(18.94950495, 5.026595087, 100)
print(INTUSG2015)

INTUSG2016 <- calcular_intervalo_confianza(18.98415842, 4.508995029, 100)
print(INTUSG2016)

INTUSG2017 <- calcular_intervalo_confianza(18.07722772, 5.312080035, 100)
print(INTUSG2017)

# -----------------------------------------------------------------------------

INTTRB1988 <- calcular_intervalo_confianza(213.0693069, 217.9828432, 100)
print(INTTRB1988)

INTTRB1989 <- calcular_intervalo_confianza(213.9306931, 202.0213819, 100)
print(INTTRB1989)

INTTRB1990 <- calcular_intervalo_confianza(197.6930693, 190.3494308, 100)
print(INTTRB1990)

INTTRB2015 <- calcular_intervalo_confianza(199.7425743, 195.7704077, 100)
print(INTTRB2015)

INTTRB2016 <- calcular_intervalo_confianza(179.2376238, 158.2347015, 100)
print(INTTRB2016)

INTTRB2017 <- calcular_intervalo_confianza(179.3267327, 215.995467, 100)
print(INTTRB2017)