# ==============================================================================
# PROPÓSITO: Representación cartográfica de resultados espaciales y tendencias
# PROYECTO: Respuesta de las aves acuáticas a la dinámica de inundación (Proyecto Cerceta)
# ARCHIVOS DE ENTRADA:
#   - outputs/01_parcelas_filtradas.shp (Vectores de las parcelas de estudio)
#   - outputs/03_test_tendencias_agua.csv (Resultados estadísticos de significancia)
#   - outputs/GEE/2_MNDWI_Medio_Donana.tif (Ráster de inundación media histórica)
#   - outputs/GEE/5_Tendencia_Tau_Donana_TIF.tif (Ráster de tendencia de Sen)
#   - outputs/GEE/MNDWI_anuales/ (Carpeta con los 20 rásters anuales)
# ARCHIVOS DE SALIDA:
#   - outputs/05_mapa_global_significancia.png (Mapa de situación y resultados)
#   - outputs/05_mapa_tendencia_tau.png (Mapa de degradación/recuperación)
#   - outputs/05_mapa_mosaico_anual.png (Mosaico temporal de 20 años)
# ==============================================================================

# 1. Cargar librerías ----------------------------------------------------------
library(tidyverse)
library(sf)
library(terra)
library(ggspatial) # Para flecha norte y escala
library(here)

# 2. Configuración de Proyección y Carga de Datos ------------------------------
# Definimos el CRS estándar para Doñana (UTM 30N - ETRS89)
crs_proyecto <- "EPSG:25830"

# Carga de vectores
parcelas_estudio <- st_read(here("outputs/01_parcelas_filtradas.shp")) %>% 
  st_transform(crs_proyecto)

# Carga de resultados estadísticos del Script 03 para marcar parcelas significativas
resultados_agua <- read_csv(here("outputs/03_test_tendencias_agua.csv"))

# Unimos la estadística con la geometría
mapa_datos <- parcelas_estudio %>%
  left_join(resultados_agua, by = "parcela")

# Carga de Rásters
mndwi_medio <- rast(here("outputs/GEE/MNDWI_Medio_Donana_2005_2025.tif")) %>% 
  project(crs_proyecto)

raster_tau <- rast(here("outputs/GEE/Tendencia_Tau.tif")) %>% 
  project(crs_proyecto)


# 3. Mapa A: Inundación Media y Significancia ---------------------------------
# Visualiza el promedio histórico resaltando en rojo las lagunas que se secan.

mapa_global <- ggplot() +
  # Fondo: Ráster de MNDWI medio
  layer_spatial(data = mndwi_medio, aes(fill = after_stat(band1))) +
  scale_fill_gradientn(colors = c("white", "#ebf3fb", "#084594"), 
                       name = "MNDWI Medio", na.value = "transparent") +
  
  # Capa: Todas las parcelas de estudio (Contorno amarillo)
  geom_sf(data = mapa_datos, fill = NA, color = "yellow", linewidth = 0.4) +
  
  # CORRECCIÓN AQUÍ: Cambiamos 'p_valor' por 'p_val' según la Fuente [1]
  geom_sf(data = filter(mapa_datos, p_val < 0.05), 
          fill = NA, color = "red", linewidth = 0.8) +
  
  # Elementos cartográficos
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr", style = north_arrow_minimal()) +
  labs(title = "Inundación Media y Áreas Críticas",
       subtitle = "Rojo: Tendencias significativas (p < 0.05) | Amarillo: Parcelas",
       caption = "Datos: Landsat / GEE") +
  theme_minimal()

# Visualizar
print(mapa_global)














# 3. Mapa A: Estado Global e Inundación Media ---------------------------------
# Este mapa muestra el promedio histórico de agua y resalta las parcelas 
# donde la pérdida de agua es estadísticamente significativa.

mapa_global <- ggplot() +
  # Fondo: Ráster de MNDWI medio (Azul = Agua, Blanco = Suelo)
  layer_spatial(mndwi_medio, aes(fill = after_stat(band1))) +
  scale_fill_gradientn(colors = c("white", "#ebf3fb", "#084594"), 
                       name = "MNDWI Medio", na.value = "transparent") +
  # Capa: Todas las parcelas de estudio (Contorno amarillo)
  geom_sf(data = mapa_datos, fill = NA, color = "yellow", linewidth = 0.4) +
  # Capa: Resaltado en rojo de parcelas con tendencia significativa
  geom_sf(data = filter(mapa_datos, p_valor < 0.05), 
          fill = NA, color = "red", linewidth = 0.8) +
  # Elementos cartográficos
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         style = north_arrow_fancy_orienteering()) +
  labs(title = "Distribución de la Inundación y Áreas Críticas",
       subtitle = "Rojo: Tendencias de desecación significativas | Amarillo: Parcelas analizadas",
       caption = "Datos: Landsat 7-8 / GEE") +
  theme_minimal()

print(mapa_global)

# 4. Mapa B: Mapa de Tendencia Tau (Degradación vs Recuperación) ---------------
# Visualiza la pendiente de Sen: Rojo indica desecación, Verde recuperación.

mapa_tau <- ggplot() +
  # Ráster Tau de fondo
  layer_spatial(raster_tau, aes(fill = after_stat(band1))) +
  scale_fill_gradient2(low = "#e31a1c", mid = "white", high = "#33a02c", 
                       midpoint = 0, name = "Valor Tau") +
  # Contornos de parcelas significativas para dar contexto
  geom_sf(data = filter(mapa_datos, p_valor < 0.05), 
          fill = NA, color = "black", linetype = "dashed", linewidth = 0.3) +
  annotation_scale(location = "bl") +
  labs(title = "Mapa de Tendencia Hidrológica (Índice Tau)",
       subtitle = "Valores negativos (rojo) indican pérdida de agua persistente 2005-2025") +
  theme_minimal()

print(mapa_tau)



# 6. Exportación de Productos Cartográficos ------------------------------------

ggsave(here("outputs/05_mapa_global_significancia.png"), mapa_global, 
       width = 10, height = 8, dpi = 300)

ggsave(here("outputs/05_mapa_tendencia_tau.png"), mapa_tau, 
       width = 10, height = 8, dpi = 300)

ggsave(here("outputs/05_mapa_mosaico_anual.png"), mapa_mosaico, 
       width = 14, height = 10, dpi = 300)

message(">>> SCRIPT 05 COMPLETADO: Mapas generados y exportados a la carpeta outputs.")


# 5. Mapa C: Mosaico Temporal (20 paneles anuales) -----------------------------
# Genera un stack de todos los inviernos para ver la evolución visual año a año.

# Listar y cargar todos los rásters anuales
archivos_anuales <- list.files(here("outputs/GEE/MNDWI_anuales/"), 
                               pattern = ".tif$", full.names = TRUE)
stack_anual <- rast(archivos_anuales) %>% project(crs_proyecto)

mapa_mosaico <- ggplot() +
  layer_spatial(stack_anual) +
  # facet_wrap crea un panel por cada año (cada "banda" del stack)
  facet_wrap(~band) + 
  scale_fill_gradient(low = "white", high = "#084594", name = "Inundación") +
  geom_sf(data = parcelas_estudio, fill = NA, color = "yellow", linewidth = 0.1, alpha = 0.5) +
  labs(title = "Evolución Anual de la Lámina de Agua en Doñana",
       subtitle = "Serie temporal histórica (inviernos 2005-2025)") +
  theme_void() + # Limpiamos ejes para que no saturen los 20 paneles
  theme(strip.text = element_text(size = 7, face = "bold"),
        legend.position = "bottom")

print(mapa_mosaico)