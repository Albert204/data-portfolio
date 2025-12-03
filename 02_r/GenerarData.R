# Cargar librerías
setwd("E:/ProgramData/MySQL/MySQL Server 9.0/Uploads")
library(dplyr)
library(stringr)
library(lubridate)

# 1. Crear la tabla 'Usuario'
usuarios <- data.frame(
  idU = 1:100,
  Nombre = str_c("EcoUsuario_", sample(LETTERS, 100, replace = TRUE), 1:100),
  Email = str_c("eco.usuario", sample(letters, 100, replace = TRUE), "@sostenible.com"),
  Fecha_nac = sample(c(NA, sample(seq(as.Date('1960-01-01'), as.Date('2005-12-31'), by = "day"), 70)), 100, replace = TRUE),
  Género = sample(c('H', 'M', 'O', 'N'), 100, replace = TRUE, prob = c(0.5, 0.4, 0.07, 0.03)),
  Fecha_Reg = sample(seq(as.Date('2020-01-01'), as.Date('2024-12-31'), by = "day"), 100, replace = TRUE),
  CP = sample(c(NA, sprintf("%05d", sample(10000:99999, 60, replace = TRUE))), 100, replace = TRUE),
  TipoU = sample(c('personal', 'profesional'), 100, replace = TRUE, prob = c(0.65, 0.35))
)

# Asegurar que las fechas están en formato Date
usuarios$Fecha_nac <- as.Date(usuarios$Fecha_nac)
usuarios$Fecha_Reg <- as.Date(usuarios$Fecha_Reg)

# 2. Crear la tabla 'Categoria'
categorias <- data.frame(
  ID_cat = sprintf("%02d", 1:30),
  Nombre = c(
    "Hogar Sostenible", "Moda Ecológica", "Tecnología Verde", "Alimentación Orgánica",
    "Energías Renovables", "Movilidad Sostenible", "Cosmética Natural", "Jardinería Urbana",
    "Ropa Orgánica", "Accesorios Ecológicos", "Electrodomésticos Eficientes", "Dispositivos Reciclados",
    "Paneles Solares", "Aerogeneradores", "Bicicletas Eléctricas", "Vehículos Híbridos",
    "Productos Veganos", "Superalimentos", "Huertos Urbanos", "Compostaje",
    "Calzado Sostenible", "Joyas Ecológicas", "Productos Reacondicionados", "Software Sostenible",
    "Energía Solar", "Energía Eólica", "Patinetes Eléctricos", "Carsharing",
    "Maquillaje Natural", "Cuidado Personal Ecológico"
  ),
  Cat_Padre = c(NA, NA, NA, NA,
                "01", "01", "02", "02",
                "02", "02", "03", "03",
                "05", "05", "06", "06",
                "04", "04", "08", "08",
                "09", "09", "12", "12",
                "05", "05", "06", "06",
                "07", "07")
)

# 3. Crear la tabla 'Producto'
set.seed(123)
productos <- data.frame(
  IdP = sprintf("P%03d", 1:500),
  Título = str_c(sample(c("EcoProducto", "BioArtículo", "SostenibleItem", "VerdeObjeto", "OrgánicoBien"), 500, replace = TRUE), "_", 1:500),
  Precio_salida = round(runif(500, 15, 1500), 2),
  Estado = sample(c('disponible', 'vendido', 'reservado'), 500, replace = TRUE, prob = c(0.4, 0.5, 0.1)),
  Categoria = sample(categorias$ID_cat[!categorias$ID_cat %in% categorias$Cat_Padre], 500, replace = TRUE),
  Vendedor = sample(usuarios$idU, 500, replace = TRUE)
)

# Asegurar que cada usuario tenga al menos 2 productos en venta ('disponible')
productos_disponibles <- productos %>% filter(Estado == 'disponible')
productos_por_usuario_disponible <- productos_disponibles %>%
  group_by(Vendedor) %>%
  tally()

usuarios_con_pocos_productos <- usuarios$idU[!(usuarios$idU %in% productos_por_usuario_disponible$Vendedor[productos_por_usuario_disponible$n >= 2])]

productos_a_agregar <- data.frame()

# Inicializar next_id para generar IdP únicos
current_max_id <- max(as.numeric(substr(productos$IdP, 2, 5)))
next_id <- current_max_id + 1

for (usuario in usuarios_con_pocos_productos) {
  num_productos_disponibles <- productos_por_usuario_disponible$n[productos_por_usuario_disponible$Vendedor == usuario]
  if (length(num_productos_disponibles) == 0) {
    num_productos_disponibles <- 0
  }
  num_productos_faltantes <- 2 - num_productos_disponibles
  if (num_productos_faltantes > 0) {
    nuevos_ids <- next_id:(next_id + num_productos_faltantes - 1)
    next_id <- next_id + num_productos_faltantes
    nuevos_productos <- data.frame(
      IdP = sprintf("P%03d", nuevos_ids),
      Título = str_c(sample(c("EcoProducto", "BioArtículo", "SostenibleItem", "VerdeObjeto", "OrgánicoBien"), num_productos_faltantes, replace = TRUE), "_Extra_", usuario),
      Precio_salida = round(runif(num_productos_faltantes, 15, 1500), 2),
      Estado = 'disponible',
      Categoria = sample(categorias$ID_cat[!categorias$ID_cat %in% categorias$Cat_Padre], num_productos_faltantes, replace = TRUE),
      Vendedor = usuario
    )
    productos_a_agregar <- rbind(productos_a_agregar, nuevos_productos)
  }
}

# Combinar los productos existentes con los nuevos
productos <- rbind(productos, productos_a_agregar)

# Verificar si hay IdP duplicados
if (any(duplicated(productos$IdP))) {
  cat("Advertencia: Se encontraron IdP duplicados:\n")
  print(productos$IdP[duplicated(productos$IdP)])
} else {
  cat("No se encontraron IdP duplicados.\n")
}

# Recalcular las proporciones de estados
total_productos <- nrow(productos)
cantidad_disponibles_deseada <- 200
cantidad_reservados_deseada <- 50
cantidad_vendidos_deseada <- 250

cantidad_total_deseada <- cantidad_disponibles_deseada + cantidad_reservados_deseada + cantidad_vendidos_deseada

if (cantidad_total_deseada < total_productos) {
  diferencia <- total_productos - cantidad_total_deseada
  cantidad_disponibles_deseada <- cantidad_disponibles_deseada + diferencia
}

productos$Estado <- sample(
  c(
    rep('disponible', cantidad_disponibles_deseada),
    rep('reservado', cantidad_reservados_deseada),
    rep('vendido', cantidad_vendidos_deseada)
  ),
  total_productos,
  replace = FALSE
)

# 4. Actualizar tablas derivadas
disponible <- productos %>%
  filter(Estado == 'disponible') %>%
  select(IdP)

vendido <- productos %>%
  filter(Estado == 'vendido') %>%
  mutate(
    Fecha_venta = sample(seq(as.Date('2020-01-01'), Sys.Date(), by = "day"), n(), replace = TRUE),
    Precio_Venta = round(Precio_salida * runif(n(), 0.9, 1.1), 2),
    Tipo_V = sample(c('efectivo', 'no efectivo'), n(), replace = TRUE, prob = c(0.7, 0.3)),
    Comprado_por = sample(usuarios$idU, n(), replace = TRUE)
  ) %>%
  select(IdP, Fecha_venta, Precio_Venta, Tipo_V, Comprado_por)

# Asegurar al menos 50 compradores distintos
compradores_distintos <- length(unique(vendido$Comprado_por))
if (compradores_distintos < 50) {
  compradores_faltantes <- setdiff(sample(usuarios$idU, 50), unique(vendido$Comprado_por))
  vendido$Comprado_por[1:length(compradores_faltantes)] <- compradores_faltantes
}

reservado <- productos %>%
  filter(Estado == 'reservado') %>%
  mutate(
    Fecha_reserva = sample(seq(as.Date('2023-01-01'), Sys.Date(), by = "day"), n(), replace = TRUE),
    Reservado_por = sample(usuarios$idU, n(), replace = TRUE)
  ) %>%
  select(IdP, Fecha_reserva, Reservado_por)

# Asegurar al menos 30 usuarios reservantes distintos
reservantes_distintos <- length(unique(reservado$Reservado_por))
if (reservantes_distintos < 30) {
  reservantes_faltantes <- setdiff(sample(usuarios$idU, 30), unique(reservado$Reservado_por))
  reservado$Reservado_por[1:length(reservantes_faltantes)] <- reservantes_faltantes
}

# Verificar que cada usuario tenga al menos 2 productos disponibles
productos_disponibles <- productos %>% filter(Estado == 'disponible')
productos_por_usuario_disponible <- productos_disponibles %>%
  group_by(Vendedor) %>%
  tally()

usuarios_con_pocos_productos <- productos_por_usuario_disponible$Vendedor[productos_por_usuario_disponible$n < 2]

if (length(usuarios_con_pocos_productos) == 0) {
  cat("Todos los usuarios tienen al menos 2 productos en venta.\n")
} else {
  cat("Aún hay usuarios que no cumplen la condición:\n")
  print(usuarios_con_pocos_productos)
}

# 5. Actualizar la tabla 'Foto'
fotos <- data.frame(
  IdF = sprintf("F%05d", 1:(2 * nrow(productos))),
  URL = str_c("https://sostenible.com/imagenes/", sample(letters, 2 * nrow(productos), replace = TRUE), "_", 1:(2 * nrow(productos)), ".jpg"),
  Producto = rep(productos$IdP, each = 2)
)

# 6. Actualizar las tablas de opiniones
opinion_comprador <- vendido %>%
  sample_frac(0.75) %>%
  mutate(
    EstrellasC = sample(1:5, n(), replace = TRUE),
    OpinionC = sample(c(
      "Excelente calidad y compromiso ecológico", "Muy satisfecho con mi compra sostenible",
      "El producto es bueno, pero esperaba más", "No cumplió con los estándares ecológicos que buscaba",
      "Decepcionado, no era tan ecológico como parecía"
    ), n(), replace = TRUE)
  ) %>%
  select(Producto = IdP, EstrellasC, OpinionC)

opinion_vendedor <- vendido %>%
  sample_frac(0.75) %>%
  mutate(
    EstrellasV = sample(1:5, n(), replace = TRUE),
    OpinionV = sample(c(
      "Comprador comprometido con el medio ambiente", "Transacción fluida y sin problemas",
      "Comunicación un poco lenta", "Dificultades en la coordinación de entrega",
      "No se concretó la entrega, mala experiencia"
    ), n(), replace = TRUE)
  ) %>%
  select(Producto = IdP, EstrellasV, OpinionV)

# 7. Actualizar la tabla 'Favorito'
num_disponibles <- nrow(disponible)
num_favoritos <- min(100, num_disponibles)

favorito <- data.frame(
  Producto = sample(disponible$IdP, num_favoritos, replace = FALSE),
  Usuario = sample(usuarios$idU, num_favoritos, replace = TRUE)
)

usuarios_favoritos <- length(unique(favorito$Usuario))
if (usuarios_favoritos < 50) {
  usuarios_faltantes <- setdiff(sample(usuarios$idU, 50), unique(favorito$Usuario))
  favorito$Usuario[1:length(usuarios_faltantes)] <- usuarios_faltantes
}

# Exportar las tablas actualizadas
write.csv(usuarios, "usuarios.csv", row.names = FALSE, na = "", fileEncoding = "UTF-8", quote = TRUE)
write.csv(categorias, "categorias.csv", row.names = FALSE, na = "", fileEncoding = "UTF-8", quote = TRUE)
write.csv(productos, "productos.csv", row.names = FALSE, na = "", fileEncoding = "UTF-8", quote = TRUE)
write.csv(vendido, "vendido.csv", row.names = FALSE, na = "", fileEncoding = "UTF-8", quote = TRUE)
write.csv(reservado, "reservado.csv", row.names = FALSE, na = "", fileEncoding = "UTF-8", quote = TRUE)
write.csv(disponible, "disponible.csv", row.names = FALSE, na = "", fileEncoding = "UTF-8", quote = TRUE)
write.csv(fotos, "fotos.csv", row.names = FALSE, na = "", fileEncoding = "UTF-8", quote = TRUE)
write.csv(opinion_comprador, "opinion_comprador.csv", row.names = FALSE, na = "", fileEncoding = "UTF-8", quote = TRUE)
write.csv(opinion_vendedor, "opinion_vendedor.csv", row.names = FALSE, na = "", fileEncoding = "UTF-8", quote = TRUE)
write.csv(favorito, "favorito.csv", row.names = FALSE, na = "", fileEncoding = "UTF-8", quote = TRUE)

