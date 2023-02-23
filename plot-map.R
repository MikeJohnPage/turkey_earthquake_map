# ---- Load libs ----
library(tidyverse)
library(sf)
library(httr2)
library(viridis)

# ---- Funs ----
download_file <- function(url, file_extension) {
  stopifnot(
    !missing(url),
    !missing(file_extension),
    is.character(url),
    is.character(file_extension)
  )

  temp_path <- tempfile(fileext = file_extension)

  request(url) |>
    req_perform(path = temp_path)

  return(temp_path)
}

# ---- Create map theme ----
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "FiraCode-Retina", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA),
      legend.background = element_rect(fill = "#ffffff", color = NA),
      panel.border = element_blank(),
      # Add labs elements
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9, hjust = 0),
      plot.title = element_text(size = 15, hjust = 0.5),
      plot.subtitle = element_text(
        size = 10, hjust = 0.5,
        margin = margin(
          b = 0.2,
          t = 0.2,
          l = 2,
          unit = "cm"
        ),
        debug = F
      ),
      # captions
      plot.caption = element_text(
        size = 7,
        hjust = .5,
        margin = margin(
          t = 0.2,
          b = 0,
          unit = "cm"
        ),
        color = "#939184"
      ),
      ...
    )
}

# ---- Load data ----
# Source: https://data.humdata.org/dataset/cod-ab-tur?
url <- "https://data.humdata.org/dataset/ac768a5c-db29-4872-9792-bd3179e29c45/resource/6cc83f12-885f-475b-98f7-9bad99d682b9/download/turkey_administrativelevels0_1_2.zip"

download_file(
  url, ".zip"
) |>
  unzip(exdir = tempdir())

raw_shapefile <- file.path(tempdir(), "tur_polbnda_adm1.shp")

turkey_boundaries <- read_sf(raw_shapefile)

# Append Housing and Tent numbers
turkey_housing_tents <- turkey_boundaries |>
  mutate(
    number_of_housing = case_when(
      adm1_en == "KAHRAMANMARAS" ~ "45067",
      adm1_en == "MALATYA" ~ "44770",
      adm1_en == "HATAY" ~ "40426",
      adm1_en == "ADIYAMAN" ~ "25882",
      adm1_en == "GAZIANTEP" ~ "18544",
      adm1_en == "OSMANIYE" ~ "9550",
      adm1_en == "DIYARBAKIR" ~ "6000",
      adm1_en == "ELAZIG" ~ "3750",
      adm1_en == "ADANA" ~ "2500",
      adm1_en == "SANLIURFA" ~ "3000",
      adm1_en == "KILIS" ~ "250",
      TRUE ~ NA_character_
    )
  ) |>
  mutate(
    number_of_tents = case_when(
      adm1_en == "KAHRAMANMARAS" ~ "66685",
      adm1_en == "MALATYA" ~ "25380",
      adm1_en == "HATAY" ~ "69766",
      adm1_en == "ADIYAMAN" ~ "45852",
      adm1_en == "GAZIANTEP" ~ "49670",
      adm1_en == "OSMANIYE" ~ "7170",
      adm1_en == "DIYARBAKIR" ~ "6328",
      adm1_en == "ADANA" ~ "17515",
      adm1_en == "SANLIURFA" ~ "8838",
      adm1_en == "KILIS" ~ "3605",
      TRUE ~ NA_character_
    )
  ) |> 
  mutate(
    number_of_housing = as.numeric(number_of_housing),
    number_of_tents = as.numeric(number_of_tents)
  )

# ---- Plot ----
turkey_housing_tents |> 
  drop_na() |> 
  ggplot() +
  geom_sf(
    mapping = aes(fill = number_of_housing),
    color = "black",
    size = 0.05
  ) +
  scale_fill_viridis(
    breaks = seq(0, 45000, by = 5000),
    labels = seq(0, 45000, by = 5000),
    na.value = "transparent",
    option = "magma",
    name = expression(paste("Number of \nhouses")),
    alpha = 0.8,
    begin = 0.1,
    end = 0.9,
    discrete = F,
    direction = -1,
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = "top",
      reverse = T)) +
  theme_map()

turkey_housing_tents |> 
  drop_na() |> 
  ggplot() +
  geom_sf(
    mapping = aes(fill = number_of_tents),
    color = "black",
    size = 0.05
  ) +
  scale_fill_viridis(
    breaks = seq(0, 70000, by = 10000),
    labels = seq(0, 70000, by = 10000),
    na.value = "transparent",
    option = "magma",
    name = expression(paste("Number of \ntents")),
    alpha = 0.8,
    begin = 0.1,
    end = 0.9,
    discrete = F,
    direction = -1,
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = "top",
      reverse = T)) +
  theme_map()
