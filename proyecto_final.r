
############################################################
### 00. LIBRERÍAS ###
############################################################

# Instalar solo si hace falta:
# install.packages(c("tidyverse", "corrplot", "randomForest", "shiny", "shinydashboard"))

library(tidyverse)
library(corrplot)
library(randomForest)
library(shiny)
library(shinydashboard)


############################################################
### 01. CONFIGURACIÓN GENERAL ###
############################################################

ruta <- "/Users/JuanEstebanDaza/Desktop/GitHub/Proyecto-Final/csv/"


archivos_necesarios <- c(
  "Match.csv",
  "Team_Attributes.csv",
  "Player_Attributes.csv",
  "League.csv",
  "Country.csv",
  "Team.csv",
  "Player.csv"
)

archivos_faltantes <- archivos_necesarios[
  !file.exists(file.path(ruta, archivos_necesarios))
]

if (length(archivos_faltantes) > 0) {
  stop(
    paste(
      "Faltan estos archivos en la carpeta configurada:",
      paste(archivos_faltantes, collapse = ", ")
    )
  )
}

# Colores consistentes para todos los gráficos.
colores_resultado <- c(
  "Home Win" = "#2E7D32",  # Verde
  "Draw" = "#9E9E9E",      # Gris
  "Away Win" = "#C62828"   # Rojo
)

# Tema visual ejecutivo.
tema_ejecutivo <- theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 13),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "bottom"
  )


############################################################
### 02. IMPORTACIÓN DE BASES ###
############################################################

match_raw <- read_csv(file.path(ruta, "Match.csv"), show_col_types = FALSE)
team_attr <- read_csv(file.path(ruta, "Team_Attributes.csv"), show_col_types = FALSE)
player_attr <- read_csv(file.path(ruta, "Player_Attributes.csv"), show_col_types = FALSE)
league <- read_csv(file.path(ruta, "League.csv"), show_col_types = FALSE)
country <- read_csv(file.path(ruta, "Country.csv"), show_col_types = FALSE)
team <- read_csv(file.path(ruta, "Team.csv"), show_col_types = FALSE)
player <- read_csv(file.path(ruta, "Player.csv"), show_col_types = FALSE)


############################################################
### 03. EXPLORACIÓN INICIAL ###
############################################################

print(dim(match_raw))
print(dim(team_attr))
print(dim(player_attr))
print(dim(league))
print(dim(country))
print(dim(team))
print(dim(player))

print(names(match_raw))
glimpse(match_raw)


############################################################
### 04. CREACIÓN DE VARIABLES BASE DEL PARTIDO ###
############################################################

match_data <- match_raw %>%
  mutate(
    result = case_when(
      home_team_goal > away_team_goal ~ "Home Win",
      home_team_goal < away_team_goal ~ "Away Win",
      TRUE ~ "Draw"
    ),
    result = factor(result, levels = c("Home Win", "Draw", "Away Win")),
    goal_difference = home_team_goal - away_team_goal,
    home_win_binary = ifelse(result == "Home Win", 1, 0),
    home_win_factor = factor(
      ifelse(result == "Home Win", "Win", "No Win"),
      levels = c("No Win", "Win")
    )
  )

print(table(match_data$result))
print(summary(match_data$goal_difference))
print(table(match_data$home_win_factor))


############################################################
### 05. UNIÓN CON PAÍSES Y LIGAS ###
############################################################

country_clean <- country %>%
  select(
    country_id = id,
    country_name = name
  )

league_clean <- league %>%
  select(
    league_id = id,
    league_country_id = country_id,
    league_name = name
  )

match_data <- match_data %>%
  left_join(country_clean, by = "country_id") %>%
  left_join(league_clean, by = "league_id")

print(head(match_data %>% select(country_name, league_name)))


############################################################
### 06. UNIÓN CON NOMBRES DE EQUIPOS ###
############################################################

team_clean <- team %>%
  select(
    team_api_id,
    team_long_name,
    team_short_name
  )

home_team_names <- team_clean %>%
  rename(
    home_team_api_id = team_api_id,
    home_team_name = team_long_name,
    home_team_short = team_short_name
  )


away_team_names <- team_clean %>%
  rename(
    away_team_api_id = team_api_id,
    away_team_name = team_long_name,
    away_team_short = team_short_name
  )

match_data <- match_data %>%
  left_join(home_team_names, by = "home_team_api_id") %>%
  left_join(away_team_names, by = "away_team_api_id")

print(head(match_data %>% select(home_team_name, away_team_name)))


############################################################
### 07. ATRIBUTOS MÁS RECIENTES DE EQUIPOS ###
############################################################

team_latest <- team_attr %>%
  mutate(date = as.Date(date)) %>%
  arrange(team_api_id, desc(date)) %>%
  distinct(team_api_id, .keep_all = TRUE)

team_home <- team_latest %>%
  select(
    home_team_api_id = team_api_id,
    home_buildUpPlaySpeed = buildUpPlaySpeed,
    home_buildUpPlayPassing = buildUpPlayPassing,
    home_chanceCreationPassing = chanceCreationPassing,
    home_chanceCreationShooting = chanceCreationShooting,
    home_defencePressure = defencePressure,
    home_defenceAggression = defenceAggression,
    home_defenceTeamWidth = defenceTeamWidth
  )

team_away <- team_latest %>%
  select(
    away_team_api_id = team_api_id,
    away_buildUpPlaySpeed = buildUpPlaySpeed,
    away_buildUpPlayPassing = buildUpPlayPassing,
    away_chanceCreationPassing = chanceCreationPassing,
    away_chanceCreationShooting = chanceCreationShooting,
    away_defencePressure = defencePressure,
    away_defenceAggression = defenceAggression,
    away_defenceTeamWidth = defenceTeamWidth
  )

match_data <- match_data %>%
  left_join(team_home, by = "home_team_api_id") %>%
  left_join(team_away, by = "away_team_api_id")

match_data <- match_data %>%
  mutate(
    diff_speed = home_buildUpPlaySpeed - away_buildUpPlaySpeed,
    diff_build_passing = home_buildUpPlayPassing - away_buildUpPlayPassing,
    diff_chance_passing = home_chanceCreationPassing - away_chanceCreationPassing,
    diff_chance_shooting = home_chanceCreationShooting - away_chanceCreationShooting,
    diff_pressure = home_defencePressure - away_defencePressure,
    diff_aggression = home_defenceAggression - away_defenceAggression,
    diff_width = home_defenceTeamWidth - away_defenceTeamWidth
  )

print(summary(match_data %>% select(
  diff_speed,
  diff_build_passing,
  diff_chance_passing,
  diff_chance_shooting,
  diff_pressure,
  diff_aggression,
  diff_width
)))


############################################################
### 08. VISUALIZACIONES EJECUTIVAS INICIALES ###
############################################################

# Distribución general de resultados.
graf_resultados <- match_data %>%
  ggplot(aes(x = result, fill = result)) +
  geom_bar(width = 0.65) +
  scale_fill_manual(values = colores_resultado, drop = FALSE) +
  labs(
    title = "Distribución de resultados",
    subtitle = "Cantidad de partidos por resultado final",
    x = "Resultado",
    y = "Número de partidos",
    fill = "Resultado"
  ) +
  tema_ejecutivo

print(graf_resultados)

# Boxplot de diferencia de velocidad por resultado.
graf_boxplot_speed <- match_data %>%
  filter(!is.na(diff_speed)) %>%
  ggplot(aes(x = result, y = diff_speed, fill = result)) +
  geom_boxplot(width = 0.6, alpha = 0.85, outlier.alpha = 0.35) +
  scale_fill_manual(values = colores_resultado, drop = FALSE) +
  labs(
    title = "Diferencia de velocidad y resultado",
    subtitle = "Valores positivos indican mayor velocidad del equipo local",
    x = "Resultado",
    y = "Diferencia de velocidad",
    fill = "Resultado"
  ) +
  tema_ejecutivo

print(graf_boxplot_speed)

# Barras de presión defensiva promedio por resultado.
graf_bar_pressure <- match_data %>%
  group_by(result) %>%
  summarise(
    avg_pressure = mean(diff_pressure, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(avg_pressure)) %>%
  ggplot(aes(x = result, y = avg_pressure, fill = result)) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = round(avg_pressure, 2)),
    vjust = -0.35,
    size = 5
  ) +
  scale_fill_manual(values = colores_resultado, drop = FALSE) +
  labs(
    title = "Presión defensiva promedio por resultado",
    subtitle = "Comparación entre equipo local y visitante",
    x = "Resultado",
    y = "Diferencia promedio de presión",
    fill = "Resultado"
  ) +
  tema_ejecutivo

print(graf_bar_pressure)

############################################################
### 09. ANOVA DE VARIABLES TÁCTICAS DEL EQUIPO ###
############################################################

anova_pressure <- aov(diff_pressure ~ result, data = match_data)
anova_speed <- aov(diff_speed ~ result, data = match_data)

print(summary(anova_pressure))
print(summary(anova_speed))


############################################################
### 10. CORRELACIONES INICIALES ###
############################################################

corr_data <- match_data %>%
  select(
    goal_difference,
    home_win_binary,
    diff_speed,
    diff_pressure,
    diff_aggression,
    diff_chance_passing,
    home_team_goal,
    away_team_goal
  ) %>%
  na.omit()

cor_matrix <- cor(corr_data, use = "complete.obs")
print(round(cor_matrix, 2))

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  tl.col = "black"
)


############################################################
### 11. MODELO DE REGRESIÓN LINEAL BASE ###
############################################################

# Para conectar mejor con la pregunta de investigación, usamos home_win_binary.
# Esta variable vale 1 cuando gana el equipo local y 0 cuando no gana.
# El modelo se puede interpretar como una aproximación simple a probabilidad de victoria.

model1 <- lm(
  home_win_binary ~ diff_pressure + diff_aggression + diff_speed,
  data = match_data
)

print(summary(model1))


############################################################
### 12. RANDOM FOREST INICIAL PARA CLASIFICAR VICTORIA LOCAL ###
############################################################

rf_data <- match_data %>%
  select(
    home_win_factor,
    diff_pressure,
    diff_aggression,
    diff_speed
  ) %>%
  na.omit()

set.seed(123)

rf_model <- randomForest(
  home_win_factor ~ .,
  data = rf_data,
  importance = TRUE,
  ntree = 300
)

print(rf_model)
print(importance(rf_model))

rf_importance_inicial <- importance(rf_model) %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  mutate(importancia = MeanDecreaseGini) %>%
  arrange(importancia)

graf_rf_inicial <- rf_importance_inicial %>%
  ggplot(aes(x = reorder(variable, importancia), y = importancia)) +
  geom_col(width = 0.65, fill = "#455A64") +
  coord_flip() +
  labs(
    title = "Importancia de variables - Random Forest inicial",
    subtitle = "Modelo predictivo de victoria local",
    x = "Variable",
    y = "Importancia: Mean Decrease Gini"
  ) +
  tema_ejecutivo

print(graf_rf_inicial)


############################################################
### 13. INFORMACIÓN DE JUGADORES: RATING, ALTURA Y PESO ###
############################################################

# Player_Attributes tiene el rating de los jugadores.
# Player tiene datos físicos: altura y peso.
# Unimos las dos bases por player_api_id.

player_latest <- player_attr %>%
  mutate(date = as.Date(date)) %>%
  arrange(player_api_id, desc(date)) %>%
  distinct(player_api_id, .keep_all = TRUE)

player_bio <- player %>%
  select(
    player_api_id,
    player_name,
    height,
    weight
  )

player_info <- player_latest %>%
  select(
    player_api_id,
    overall_rating
  ) %>%
  left_join(player_bio, by = "player_api_id")

print(head(player_info))


############################################################
### 14. RATINGS, ALTURA Y PESO DEL EQUIPO LOCAL ###
############################################################

match_data <- match_data %>%
  left_join(player_info %>% rename(home_player_1 = player_api_id, home_rating_1 = overall_rating, home_height_1 = height, home_weight_1 = weight), by = "home_player_1") %>%
  left_join(player_info %>% rename(home_player_2 = player_api_id, home_rating_2 = overall_rating, home_height_2 = height, home_weight_2 = weight), by = "home_player_2") %>%
  left_join(player_info %>% rename(home_player_3 = player_api_id, home_rating_3 = overall_rating, home_height_3 = height, home_weight_3 = weight), by = "home_player_3") %>%
  left_join(player_info %>% rename(home_player_4 = player_api_id, home_rating_4 = overall_rating, home_height_4 = height, home_weight_4 = weight), by = "home_player_4") %>%
  left_join(player_info %>% rename(home_player_5 = player_api_id, home_rating_5 = overall_rating, home_height_5 = height, home_weight_5 = weight), by = "home_player_5") %>%
  left_join(player_info %>% rename(home_player_6 = player_api_id, home_rating_6 = overall_rating, home_height_6 = height, home_weight_6 = weight), by = "home_player_6") %>%
  left_join(player_info %>% rename(home_player_7 = player_api_id, home_rating_7 = overall_rating, home_height_7 = height, home_weight_7 = weight), by = "home_player_7") %>%
  left_join(player_info %>% rename(home_player_8 = player_api_id, home_rating_8 = overall_rating, home_height_8 = height, home_weight_8 = weight), by = "home_player_8") %>%
  left_join(player_info %>% rename(home_player_9 = player_api_id, home_rating_9 = overall_rating, home_height_9 = height, home_weight_9 = weight), by = "home_player_9") %>%
  left_join(player_info %>% rename(home_player_10 = player_api_id, home_rating_10 = overall_rating, home_height_10 = height, home_weight_10 = weight), by = "home_player_10") %>%
  left_join(player_info %>% rename(home_player_11 = player_api_id, home_rating_11 = overall_rating, home_height_11 = height, home_weight_11 = weight), by = "home_player_11")

match_data <- match_data %>%
  mutate(
    home_team_rating = rowMeans(select(., starts_with("home_rating_")), na.rm = TRUE),
    home_team_height = rowMeans(select(., starts_with("home_height_")), na.rm = TRUE),
    home_team_weight = rowMeans(select(., starts_with("home_weight_")), na.rm = TRUE),
    home_team_rating = ifelse(is.nan(home_team_rating), NA_real_, home_team_rating),
    home_team_height = ifelse(is.nan(home_team_height), NA_real_, home_team_height),
    home_team_weight = ifelse(is.nan(home_team_weight), NA_real_, home_team_weight)
  )

print(summary(match_data$home_team_rating))
print(summary(match_data$home_team_height))
print(summary(match_data$home_team_weight))


############################################################
### 15. RATINGS, ALTURA Y PESO DEL EQUIPO VISITANTE ###
############################################################

match_data <- match_data %>%
  left_join(player_info %>% rename(away_player_1 = player_api_id, away_rating_1 = overall_rating, away_height_1 = height, away_weight_1 = weight), by = "away_player_1") %>%
  left_join(player_info %>% rename(away_player_2 = player_api_id, away_rating_2 = overall_rating, away_height_2 = height, away_weight_2 = weight), by = "away_player_2") %>%
  left_join(player_info %>% rename(away_player_3 = player_api_id, away_rating_3 = overall_rating, away_height_3 = height, away_weight_3 = weight), by = "away_player_3") %>%
  left_join(player_info %>% rename(away_player_4 = player_api_id, away_rating_4 = overall_rating, away_height_4 = height, away_weight_4 = weight), by = "away_player_4") %>%
  left_join(player_info %>% rename(away_player_5 = player_api_id, away_rating_5 = overall_rating, away_height_5 = height, away_weight_5 = weight), by = "away_player_5") %>%
  left_join(player_info %>% rename(away_player_6 = player_api_id, away_rating_6 = overall_rating, away_height_6 = height, away_weight_6 = weight), by = "away_player_6") %>%
  left_join(player_info %>% rename(away_player_7 = player_api_id, away_rating_7 = overall_rating, away_height_7 = height, away_weight_7 = weight), by = "away_player_7") %>%
  left_join(player_info %>% rename(away_player_8 = player_api_id, away_rating_8 = overall_rating, away_height_8 = height, away_weight_8 = weight), by = "away_player_8") %>%
  left_join(player_info %>% rename(away_player_9 = player_api_id, away_rating_9 = overall_rating, away_height_9 = height, away_weight_9 = weight), by = "away_player_9") %>%
  left_join(player_info %>% rename(away_player_10 = player_api_id, away_rating_10 = overall_rating, away_height_10 = height, away_weight_10 = weight), by = "away_player_10") %>%
  left_join(player_info %>% rename(away_player_11 = player_api_id, away_rating_11 = overall_rating, away_height_11 = height, away_weight_11 = weight), by = "away_player_11")

match_data <- match_data %>%
  mutate(
    away_team_rating = rowMeans(select(., starts_with("away_rating_")), na.rm = TRUE),
    away_team_height = rowMeans(select(., starts_with("away_height_")), na.rm = TRUE),
    away_team_weight = rowMeans(select(., starts_with("away_weight_")), na.rm = TRUE),
    away_team_rating = ifelse(is.nan(away_team_rating), NA_real_, away_team_rating),
    away_team_height = ifelse(is.nan(away_team_height), NA_real_, away_team_height),
    away_team_weight = ifelse(is.nan(away_team_weight), NA_real_, away_team_weight)
  )

print(summary(match_data$away_team_rating))
print(summary(match_data$away_team_height))
print(summary(match_data$away_team_weight))


############################################################
### 16. VENTAJAS DEL EQUIPO LOCAL FRENTE AL VISITANTE ###
############################################################

# Estas variables conectan directamente con la pregunta de investigación.
# Valores positivos indican ventaja del equipo local frente al visitante.

match_data <- match_data %>%
  mutate(
    rating_advantage = home_team_rating - away_team_rating,
    height_advantage = home_team_height - away_team_height,
    weight_advantage = home_team_weight - away_team_weight
  )

print(summary(match_data$rating_advantage))
print(summary(match_data$height_advantage))
print(summary(match_data$weight_advantage))

ventajas_por_resultado <- match_data %>%
  group_by(result) %>%
  summarise(
    avg_rating_advantage = mean(rating_advantage, na.rm = TRUE),
    avg_height_advantage = mean(height_advantage, na.rm = TRUE),
    avg_weight_advantage = mean(weight_advantage, na.rm = TRUE),
    avg_pressure = mean(diff_pressure, na.rm = TRUE),
    avg_speed = mean(diff_speed, na.rm = TRUE),
    .groups = "drop"
  )

print(ventajas_por_resultado)

############################################################
### 17. ANOVA DE CALIDAD Y CARACTERÍSTICAS FÍSICAS ###
############################################################

anova_rating <- aov(rating_advantage ~ result, data = match_data)
anova_height <- aov(height_advantage ~ result, data = match_data)
anova_weight <- aov(weight_advantage ~ result, data = match_data)

print(summary(anova_rating))
print(summary(anova_height))
print(summary(anova_weight))


############################################################
### 18. MODELO DE REGRESIÓN COMPLETO ###
############################################################

model2 <- lm(
  home_win_binary ~ rating_advantage + height_advantage + weight_advantage +
    diff_pressure + diff_aggression + diff_speed + diff_chance_passing,
  data = match_data
)

print(summary(model2))


############################################################
### 19. CORRELACIONES DEL MODELO COMPLETO ###
############################################################

corr2 <- match_data %>%
  select(
    home_win_binary,
    goal_difference,
    rating_advantage,
    height_advantage,
    weight_advantage,
    diff_pressure,
    diff_aggression,
    diff_speed,
    diff_chance_passing
  ) %>%
  na.omit()

cor2_matrix <- cor(corr2, use = "complete.obs")
print(round(cor2_matrix, 2))

corrplot(
  cor2_matrix,
  method = "color",
  type = "upper",
  tl.col = "black"
)


############################################################
### 20. DATASET FINAL PARA MODELOS Y DASHBOARD ###
############################################################


dashboard_data <- match_data %>%
  select(
    country_name,
    league_name,
    season,
    home_team_name,
    away_team_name,
    result,
    home_win_binary,
    home_win_factor,
    home_team_goal,
    away_team_goal,
    goal_difference,
    home_team_rating,
    away_team_rating,
    rating_advantage,
    home_team_height,
    away_team_height,
    height_advantage,
    home_team_weight,
    away_team_weight,
    weight_advantage,
    diff_pressure,
    diff_aggression,
    diff_speed,
    diff_build_passing,
    diff_chance_passing,
    diff_chance_shooting,
    diff_width
  ) %>%
  filter(!is.na(country_name)) %>%
  filter(!is.na(league_name)) %>%
  filter(!is.na(home_team_name)) %>%
  filter(!is.na(away_team_name)) %>%
  filter(!is.na(home_win_binary)) %>%
  mutate(
    result = factor(result, levels = c("Home Win", "Draw", "Away Win")),
    home_win_factor = factor(home_win_factor, levels = c("No Win", "Win"))
  )

final_data <- dashboard_data %>%
  select(
    home_win_factor,
    home_win_binary,
    rating_advantage,
    height_advantage,
    weight_advantage,
    diff_pressure,
    diff_aggression,
    diff_speed,
    diff_chance_passing
  ) %>%
  na.omit()

print(dim(dashboard_data))
print(dim(final_data))
print(summary(dashboard_data))


############################################################
### 21. RANDOM FOREST FINAL PARA CLASIFICAR VICTORIA LOCAL ###
############################################################

set.seed(123)

rf_final <- randomForest(
  home_win_factor ~ rating_advantage + height_advantage + weight_advantage +
    diff_pressure + diff_aggression + diff_speed + diff_chance_passing,
  data = final_data,
  importance = TRUE,
  ntree = 300
)

print(rf_final)
print(importance(rf_final))

rf_importance_final <- importance(rf_final) %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  mutate(importancia = MeanDecreaseGini) %>%
  arrange(importancia)

graf_rf_final <- rf_importance_final %>%
  ggplot(aes(x = reorder(variable, importancia), y = importancia)) +
  geom_col(width = 0.65, fill = "#455A64") +
  coord_flip() +
  labs(
    title = "Importancia de variables - Random Forest final",
    subtitle = "Modelo de clasificación de victoria local",
    x = "Variable",
    y = "Importancia: Mean Decrease Gini"
  ) +
  tema_ejecutivo

print(graf_rf_final)


############################################################
### 22. VISUALIZACIONES FINALES ###
############################################################

# Boxplot final: Rating Advantage por resultado.
graf_boxplot_rating <- dashboard_data %>%
  filter(!is.na(rating_advantage)) %>%
  ggplot(aes(x = result, y = rating_advantage, fill = result)) +
  geom_boxplot(width = 0.6, alpha = 0.85, outlier.alpha = 0.35) +
  scale_fill_manual(values = colores_resultado, drop = FALSE) +
  labs(
    title = "Rating Advantage por resultado",
    subtitle = "Valores positivos favorecen al equipo local",
    x = "Resultado",
    y = "Ventaja de rating",
    fill = "Resultado"
  ) +
  tema_ejecutivo

print(graf_boxplot_rating)

# Barras finales: altura promedio por resultado.
graf_bar_height <- dashboard_data %>%
  filter(!is.na(height_advantage)) %>%
  group_by(result) %>%
  summarise(
    mean_height = mean(height_advantage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = result, y = mean_height, fill = result)) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = round(mean_height, 2)),
    vjust = -0.35,
    size = 5
  ) +
  scale_fill_manual(values = colores_resultado, drop = FALSE) +
  labs(
    title = "Ventaja promedio de altura por resultado",
    subtitle = "Diferencia promedio de altura entre local y visitante",
    x = "Resultado",
    y = "Ventaja de altura promedio",
    fill = "Resultado"
  ) +
  tema_ejecutivo

print(graf_bar_height)

# Barras finales: rating promedio por resultado.
graf_bar_rating <- dashboard_data %>%
  filter(!is.na(rating_advantage)) %>%
  group_by(result) %>%
  summarise(
    mean_rating = mean(rating_advantage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = result, y = mean_rating, fill = result)) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = round(mean_rating, 2)),
    vjust = -0.35,
    size = 5
  ) +
  scale_fill_manual(values = colores_resultado, drop = FALSE) +
  labs(
    title = "Promedio de Rating Advantage por resultado",
    subtitle = "Comparación del nivel promedio entre local y visitante",
    x = "Resultado",
    y = "Rating Advantage promedio",
    fill = "Resultado"
  ) +
  tema_ejecutivo

print(graf_bar_rating)


############################################################
### 23. DASHBOARD SHINY EJECUTIVO ###
############################################################

# Variables disponibles para construir el modelo dinámico.
# Todas están expresadas como ventaja del equipo local frente al visitante.

variables_modelo <- c(
  "Ventaja de rating de jugadores" = "rating_advantage",
  "Ventaja de altura promedio" = "height_advantage",
  "Ventaja de peso promedio" = "weight_advantage",
  "Diferencia de presión defensiva" = "diff_pressure",
  "Diferencia de agresividad defensiva" = "diff_aggression",
  "Diferencia de velocidad de juego" = "diff_speed",
  "Diferencia de creación de pase" = "diff_chance_passing",
  "Diferencia de creación de tiro" = "diff_chance_shooting",
  "Diferencia de amplitud defensiva" = "diff_width"
)

paises_disponibles <- dashboard_data %>%
  distinct(country_name) %>%
  arrange(country_name) %>%
  pull(country_name)

ligas_disponibles <- dashboard_data %>%
  distinct(league_name) %>%
  arrange(league_name) %>%
  pull(league_name)

equipos_disponibles <- c(
  dashboard_data$home_team_name,
  dashboard_data$away_team_name
)

equipos_disponibles <- equipos_disponibles[!is.na(equipos_disponibles)]
equipos_disponibles <- sort(unique(equipos_disponibles))

ui <- dashboardPage(
  skin = "green",
  
  dashboardHeader(
    title = "Football Analytics"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Panel ejecutivo", tabName = "panel", icon = icon("dashboard"))
    ),
    
    selectInput(
      inputId = "country",
      label = "País:",
      choices = c("Todos", paises_disponibles),
      selected = "Todos"
    ),
    
    selectInput(
      inputId = "league",
      label = "Liga:",
      choices = c("Todas", ligas_disponibles),
      selected = "Todas"
    ),
    
    selectInput(
      inputId = "home_team",
      label = "Equipo local:",
      choices = c("Todos", equipos_disponibles),
      selected = "Todos"
    ),
    
    selectInput(
      inputId = "away_team",
      label = "Equipo visitante:",
      choices = c("Todos", equipos_disponibles),
      selected = "Todos"
    ),
    
    selectInput(
      inputId = "result",
      label = "Resultado:",
      choices = c("Todos", "Home Win", "Draw", "Away Win"),
      selected = "Todos"
    ),
    
    checkboxGroupInput(
      inputId = "variables",
      label = "Variables independientes del modelo:",
      choices = variables_modelo,
      selected = c(
        "rating_advantage",
        "height_advantage",
        "weight_advantage",
        "diff_pressure",
        "diff_speed"
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .small-box { border-radius: 10px; }
        .box { border-radius: 10px; }
        .main-header .logo { font-weight: bold; }
        .formula-box {
          font-size: 19px;
          font-weight: 600;
          padding: 15px;
          background-color: #F7F7F7;
          border-radius: 10px;
          border-left: 5px solid #2E7D32;
        }
        .note-box {
          font-size: 14px;
          padding: 10px;
          background-color: #FFFFFF;
          border-radius: 8px;
          border: 1px solid #DDDDDD;
        }
      "))
    ),
    
    tabItems(
      tabItem(
        tabName = "panel",
        
        fluidRow(
          valueBoxOutput("kpi_partidos", width = 4),
          valueBoxOutput("kpi_r2", width = 4),
          valueBoxOutput("kpi_goal_diff", width = 4)
        ),
        
        fluidRow(
          box(
            title = "Pregunta de investigación",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            div(
              class = "note-box",
              "¿Cuáles son los factores más importantes que influyen en la probabilidad de victoria de un equipo de fútbol profesional, considerando la calidad de los jugadores y los atributos tácticos del equipo?"
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Modelo de regresión dinámico",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            uiOutput("formula_modelo")
          )
        ),
        
        fluidRow(
          box(
            title = "Resumen estadístico del modelo",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            tableOutput("tabla_modelo")
          )
        ),
        
        fluidRow(
          box(
            title = "Distribución de resultados",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("plot_resultados", height = 330)
          ),
          
          box(
            title = "Rating Advantage por resultado",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("plot_boxplot_rating", height = 330)
          )
        ),
        
        fluidRow(
          box(
            title = "Ventaja promedio de altura",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("plot_altura", height = 330)
          ),
          
          box(
            title = "Ventaja promedio de peso",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("plot_peso", height = 330)
          )
        ),
        
        fluidRow(
          box(
            title = "Relación entre rating y probabilidad de victoria",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("plot_rating_prob", height = 330)
          ),
          
          box(
            title = "Importancia de variables - Random Forest",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("plot_importancia_rf", height = 330)
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  
  ############################################################
  ### 23.1 DATOS FILTRADOS ###
  ############################################################
  
  filtered_data <- reactive({
    df <- dashboard_data
    
    if (input$country != "Todos") {
      df <- df %>%
        filter(country_name == input$country)
    }
    
    if (input$league != "Todas") {
      df <- df %>%
        filter(league_name == input$league)
    }
    
    if (input$home_team != "Todos") {
      df <- df %>%
        filter(home_team_name == input$home_team)
    }
    
    if (input$away_team != "Todos") {
      df <- df %>%
        filter(away_team_name == input$away_team)
    }
    
    if (input$result != "Todos") {
      df <- df %>%
        filter(result == input$result)
    }
    
    df
  })
  
  
  ############################################################
  ### 23.2 DATOS PARA EL MODELO DINÁMICO ###
  ############################################################
  
  datos_modelo <- reactive({
    variables_elegidas <- input$variables
    
    if (is.null(variables_elegidas)) {
      variables_elegidas <- character(0)
    }
    
    if (length(variables_elegidas) == 0) {
      df_modelo <- filtered_data() %>%
        select(home_win_binary) %>%
        na.omit()
    } else {
      df_modelo <- filtered_data() %>%
        select(home_win_binary, all_of(variables_elegidas)) %>%
        na.omit()
    }
    
    df_modelo
  })
  
  datos_random_forest <- reactive({
    variables_elegidas <- input$variables
    
    if (is.null(variables_elegidas)) {
      variables_elegidas <- character(0)
    }
    
    if (length(variables_elegidas) == 0) {
      df_rf <- filtered_data() %>%
        select(home_win_factor) %>%
        na.omit()
    } else {
      df_rf <- filtered_data() %>%
        select(home_win_factor, all_of(variables_elegidas)) %>%
        na.omit()
    }
    
    df_rf
  })
  
  
  ############################################################
  ### 23.3 MODELO DE REGRESIÓN DINÁMICO ###
  ############################################################
  
  modelo_dinamico <- reactive({
    df_modelo <- datos_modelo()
    variables_elegidas <- input$variables
    
    if (is.null(variables_elegidas)) {
      variables_elegidas <- character(0)
    }
    
    if (nrow(df_modelo) < 10) {
      return(NULL)
    }
    
    if (length(variables_elegidas) == 0) {
      formula_lm <- as.formula("home_win_binary ~ 1")
    } else {
      formula_lm <- as.formula(
        paste("home_win_binary ~", paste(variables_elegidas, collapse = " + "))
      )
    }
    
    lm(formula_lm, data = df_modelo)
  })
  
  
  ############################################################
  ### 23.4 KPIS SUPERIORES ###
  ############################################################
  
  output$kpi_partidos <- renderValueBox({
    valueBox(
      value = nrow(filtered_data()),
      subtitle = "Partidos analizados",
      icon = icon("table"),
      color = "green"
    )
  })
  
  output$kpi_r2 <- renderValueBox({
    modelo <- modelo_dinamico()
    
    if (is.null(modelo)) {
      valueBox(
        value = "N/A",
        subtitle = "R² del modelo dinámico",
        icon = icon("line-chart"),
        color = "yellow"
      )
    } else {
      r2 <- summary(modelo)$r.squared
      
      if (!is.finite(r2)) {
        valor_r2 <- "N/A"
      } else {
        valor_r2 <- paste0(round(r2 * 100, 1), "%")
      }
      
      valueBox(
        value = valor_r2,
        subtitle = "R² del modelo dinámico",
        icon = icon("line-chart"),
        color = "green"
      )
    }
  })
  
  output$kpi_goal_diff <- renderValueBox({
    promedio <- filtered_data() %>%
      summarise(valor = mean(goal_difference, na.rm = TRUE)) %>%
      pull(valor)
    
    if (is.nan(promedio)) {
      promedio <- NA_real_
    }
    
    valueBox(
      value = ifelse(is.na(promedio), "N/A", round(promedio, 2)),
      subtitle = "Promedio diferencia de goles",
      icon = icon("calculator"),
      color = "red"
    )
  })
  
  
  ############################################################
  ### 23.5 FÓRMULA DEL MODELO DINÁMICO ###
  ############################################################
  
  output$formula_modelo <- renderUI({
    modelo <- modelo_dinamico()
    variables_elegidas <- input$variables
    
    if (is.null(variables_elegidas)) {
      variables_elegidas <- character(0)
    }
    
    if (is.null(modelo)) {
      return(
        div(
          class = "formula-box",
          "No hay suficientes datos para estimar el modelo con los filtros actuales."
        )
      )
    }
    
    coefs <- coef(modelo)
    coeficientes <- data.frame(
      variable = names(coefs),
      beta = as.numeric(coefs)
    )
    
    intercepto <- coeficientes %>%
      filter(variable == "(Intercept)") %>%
      pull(beta)
    
    terminos <- coeficientes %>%
      filter(variable != "(Intercept)") %>%
      mutate(
        signo = ifelse(is.na(beta) | beta >= 0, " + ", " - "),
        beta_texto = ifelse(is.na(beta), "NA", as.character(abs(round(beta, 3)))),
        texto = paste0(signo, beta_texto, " · ", variable, "<sub>i</sub>")
      ) %>%
      pull(texto)
    
    if (!is.finite(intercepto)) {
      intercepto_texto <- "NA"
    } else {
      intercepto_texto <- as.character(round(intercepto, 3))
    }
    
    if (length(terminos) == 0) {
      ecuacion <- paste0(
        "Y<sub>i</sub> = ",
        intercepto_texto,
        " + ε<sub>i</sub>"
      )
    } else {
      ecuacion <- paste0(
        "Y<sub>i</sub> = ",
        intercepto_texto,
        paste(terminos, collapse = ""),
        " + ε<sub>i</sub>"
      )
    }
    
    div(
      class = "formula-box",
      HTML(paste0(
        ecuacion,
        "<br><span style='font-size:14px; font-weight:400;'>",
        "Interpretación: Y<sub>i</sub> representa la probabilidad aproximada de victoria del equipo local. ",
        "El modelo usa calidad de jugadores, altura, peso y atributos tácticos.",
        "</span>"
      ))
    )
  })
  
  
  ############################################################
  ### 23.6 TABLA DEL MODELO ###
  ############################################################
  
  output$tabla_modelo <- renderTable({
    modelo <- modelo_dinamico()
    
    validate(
      need(!is.null(modelo), "No hay suficientes datos para mostrar el resumen del modelo.")
    )
    
    tabla <- summary(modelo)$coefficients %>%
      as.data.frame() %>%
      rownames_to_column("Variable") %>%
      rename(
        Estimacion = Estimate,
        Error_Estandar = `Std. Error`,
        Valor_t = `t value`,
        P_valor = `Pr(>|t|)`
      ) %>%
      mutate(
        across(where(is.numeric), ~ round(.x, 4))
      )
    
    tabla
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")
  
  
  ############################################################
  ### 23.7 GRÁFICOS DINÁMICOS ###
  ############################################################
  
  output$plot_resultados <- renderPlot({
    df_plot <- filtered_data()
    
    validate(
      need(nrow(df_plot) > 0, "No hay datos suficientes para este gráfico con los filtros actuales.")
    )
    
    df_plot %>%
      ggplot(aes(x = result, fill = result)) +
      geom_bar(width = 0.65) +
      scale_fill_manual(values = colores_resultado, drop = FALSE) +
      labs(
        title = "Distribución de resultados",
        x = "Resultado",
        y = "Número de partidos",
        fill = "Resultado"
      ) +
      tema_ejecutivo
  })
  
  output$plot_boxplot_rating <- renderPlot({
    df_plot <- filtered_data() %>%
      filter(!is.na(rating_advantage))
    
    validate(
      need(nrow(df_plot) > 0, "No hay datos suficientes para este gráfico con los filtros actuales.")
    )
    
    df_plot %>%
      ggplot(aes(x = result, y = rating_advantage, fill = result)) +
      geom_boxplot(width = 0.6, alpha = 0.85, outlier.alpha = 0.35) +
      scale_fill_manual(values = colores_resultado, drop = FALSE) +
      labs(
        title = "Rating Advantage por resultado",
        subtitle = "Valores positivos favorecen al equipo local",
        x = "Resultado",
        y = "Ventaja de rating",
        fill = "Resultado"
      ) +
      tema_ejecutivo
  })
  
  output$plot_altura <- renderPlot({
    df_plot <- filtered_data() %>%
      filter(!is.na(height_advantage)) %>%
      group_by(result) %>%
      summarise(
        avg_height = mean(height_advantage, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(avg_height))
    
    validate(
      need(nrow(df_plot) > 0, "No hay datos suficientes para este gráfico con los filtros actuales.")
    )
    
    df_plot %>%
      ggplot(aes(x = result, y = avg_height, fill = result)) +
      geom_col(width = 0.65) +
      geom_text(
        aes(label = round(avg_height, 2)),
        vjust = -0.35,
        size = 5
      ) +
      scale_fill_manual(values = colores_resultado, drop = FALSE) +
      labs(
        title = "Ventaja promedio de altura",
        subtitle = "Altura local promedio - altura visitante promedio",
        x = "Resultado",
        y = "Ventaja de altura",
        fill = "Resultado"
      ) +
      tema_ejecutivo
  })
  
  output$plot_peso <- renderPlot({
    df_plot <- filtered_data() %>%
      filter(!is.na(weight_advantage)) %>%
      group_by(result) %>%
      summarise(
        avg_weight = mean(weight_advantage, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(avg_weight))
    
    validate(
      need(nrow(df_plot) > 0, "No hay datos suficientes para este gráfico con los filtros actuales.")
    )
    
    df_plot %>%
      ggplot(aes(x = result, y = avg_weight, fill = result)) +
      geom_col(width = 0.65) +
      geom_text(
        aes(label = round(avg_weight, 2)),
        vjust = -0.35,
        size = 5
      ) +
      scale_fill_manual(values = colores_resultado, drop = FALSE) +
      labs(
        title = "Ventaja promedio de peso",
        subtitle = "Peso local promedio - peso visitante promedio",
        x = "Resultado",
        y = "Ventaja de peso",
        fill = "Resultado"
      ) +
      tema_ejecutivo
  })
  
  output$plot_rating_prob <- renderPlot({
    df_plot <- filtered_data() %>%
      filter(!is.na(rating_advantage)) %>%
      filter(!is.na(home_win_binary))
    
    validate(
      need(nrow(df_plot) > 10, "No hay datos suficientes para este gráfico con los filtros actuales.")
    )
    
    df_plot %>%
      ggplot(aes(x = rating_advantage, y = home_win_binary)) +
      geom_point(alpha = 0.25, color = "#455A64") +
      geom_smooth(method = "lm", se = FALSE, color = "#2E7D32", linewidth = 1.1) +
      labs(
        title = "Rating y victoria local",
        subtitle = "Relación entre ventaja de rating y probabilidad aproximada de ganar",
        x = "Ventaja de rating",
        y = "Victoria local: 1 = gana, 0 = no gana"
      ) +
      tema_ejecutivo
  })
  
  output$plot_importancia_rf <- renderPlot({
    df_rf <- datos_random_forest()
    variables_elegidas <- input$variables
    
    validate(
      need(!is.null(variables_elegidas), "Selecciona al menos una variable independiente."),
      need(length(variables_elegidas) > 0, "Selecciona al menos una variable independiente."),
      need(nrow(df_rf) >= 20, "No hay suficientes datos para calcular Random Forest."),
      need(n_distinct(df_rf$home_win_factor) >= 2, "Con los filtros actuales solo hay una clase de resultado. Ajusta los filtros para calcular Random Forest.")
    )
    
    set.seed(123)
    rf_dinamico <- randomForest(
      home_win_factor ~ .,
      data = df_rf,
      importance = TRUE,
      ntree = 300
    )
    
    importancia <- importance(rf_dinamico) %>%
      as.data.frame() %>%
      rownames_to_column("Variable")
    
    if ("MeanDecreaseGini" %in% names(importancia)) {
      importancia <- importancia %>%
        mutate(Importancia = MeanDecreaseGini)
    } else {
      importancia <- importancia %>%
        mutate(Importancia = MeanDecreaseAccuracy)
    }
    
    importancia <- importancia %>%
      arrange(Importancia)
    
    importancia %>%
      ggplot(aes(x = reorder(Variable, Importancia), y = Importancia)) +
      geom_col(width = 0.65, fill = "#455A64") +
      coord_flip() +
      labs(
        title = "Importancia de variables",
        subtitle = "Random Forest ajustado con los filtros actuales",
        x = "Variable",
        y = "Importancia"
      ) +
      tema_ejecutivo
  })
}

shinyApp(ui, server)
