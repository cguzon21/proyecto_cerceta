#===============================================================================
#                             GRUPOS FUNCIONALES
#===============================================================================

library(tidyverse)

# Importar csv con datos de año, especie y localidad
species_yearly <- read.csv("outputs/species_abundance_yearly.csv")

# 1. FILTRAR ESPECIES OBJETIVO Y AÑADIR GRUPOS FUNCIONALES

# Importar clasificación en grupos funcionales
functional_groups <- read.csv("data/functional_groups_unbalanced.csv")
´
# Filtrar el dataset para esas especies
species_yearly <- species_yearly |>
  inner_join(functional_groups |> select(-notes), 
             by = c("Especie" = "species")) |>
  relocate(fun_group, .after = Especie)

# Comprobar que están todas las especies (39)
# unique(species_yearly$Especie)

# 2. GRÁFICOS DE ABUNDANCIA POR GRUPO FUNCIONAL

# Gráficos exploratorios usando máximos invernales, para hacerse idea de la tendencia.

# Calcular máximos invernales por especie:
max_winter_abund <- species_yearly |>
  group_by(hydro_year, fun_group, Especie) |>
  summarize(winter_max = max(max_abundance, na.rm = TRUE),.groups = "drop")

# Calcular máximos de cada grupo funcional sumando máximos de especies:
max_abund_groups <- max_winter_abund |>
  group_by(hydro_year, fun_group) |> 
  summarize(group_max = sum(winter_max), .groups = "drop")
 
# Gráfico ejemplo con los grupos funcionales de anátidas:
max_abund_groups |> 
  filter(fun_group %in% c("duck_dabb", "duck_dive", "grazer")) |> 
  ggplot(aes(x = hydro_year, y = group_max, color = fun_group)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3) +
  labs(
    x = "Año hidrológico",
    y = "Abundancia máxima"
  ) +
  theme_minimal()
    