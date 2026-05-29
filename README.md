# Análisis Predictivo de la Diferencia de Goles en el Fútbol Europeo

Proyecto de Analítica de Negocios desarrollado en R para analizar cómo las diferencias en el estilo de juego de los equipos influyen en el resultado de los partidos de fútbol europeo, medido mediante la diferencia de goles.

---

# Pregunta Problema

¿Cómo influyen las diferencias en el estilo de juego de los equipos en el resultado de los partidos (diferencia de goles) en el fútbol europeo?

---

# Objetivo General

Analizar cómo las diferencias en el estilo de juego de los equipos influyen en la diferencia de goles en los partidos de fútbol europeo mediante técnicas de analítica de datos y modelos de regresión en R.

---

# Objetivos Específicos

1. Realizar la limpieza, transformación e integración de las bases de datos relacionadas con partidos y atributos de equipos europeos.

2. Identificar las variables de estilo de juego que presentan mayor relación con la diferencia de goles mediante análisis exploratorio y regresiones simples.

3. Construir un modelo de regresión múltiple y un dashboard interactivo que permitan explicar y visualizar el impacto de los estilos de juego sobre los resultados de los partidos.

---

# Dataset Utilizado

Se utilizó información del dataset European Soccer Database obtenido desde:

- :contentReference[oaicite:0]{index=0}

## Bases de datos utilizadas

| Archivo | Descripción |
|---|---|
| Match.csv | Información histórica de partidos |
| Team_Attributes.csv | Estilo de juego y atributos tácticos |
| Team.csv | Información general de equipos |
| Player_Attributes.csv | Estadísticas de jugadores |

---

# Hipótesis

Los equipos con estilos de juego más ofensivos y mayor presión defensiva tienden a obtener diferencias de goles positivas en los partidos del fútbol europeo.

---

# Herramientas Utilizadas

| Herramienta | Uso |
|---|---|
| R | Limpieza y análisis de datos |
| RStudio | Desarrollo del proyecto |
| tidyverse | Manipulación de datos |
| ggplot2 | Visualizaciones |
| dplyr | Transformación de datos |
| corrplot | Correlaciones |
| caret | Modelos predictivos |
| shiny | Dashboard interactivo |
| GitHub | Control de versiones |

---

# 📂 Estructura del Proyecto

```text
football-analytics-project/
│
├── data/
│   ├── raw/
│   ├── processed/
│
├── scripts/
│   ├── limpieza.R
│   ├── analisis_exploratorio.R
│   ├── regresiones.R
│   ├── dashboard.R
│
├── outputs/
│   ├── graficos/
│   ├── tablas/
│
├── dashboard/
│   ├── app.R
│
├── README.md
├── informe.pdf
└── .gitignore
