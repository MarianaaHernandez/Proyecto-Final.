
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

