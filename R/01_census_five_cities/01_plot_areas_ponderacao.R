
# Introdução --------------------------------------------------------------

#' Este script prepara mapas das áreas de ponderação das cinco cidades 
#' médias paulistas, para inserção no artigo.



# Setup -------------------------------------------------------------------
library("tidyverse")
library("sf")
library("ggspatial")
library("patchwork")
library("geobr")

source(here::here("R/01_census_five_cities", "00_utils.R"))

# Carregar dados ----------------------------------------------------------

### Na primeira vez, fazer download dos dados usando o pacote geobr
# areas_ponderacao_sf <- geobr::read_weighting_area(simplified = FALSE)
# st_write(areas_ponderacao_sf, here::here("data", "areas_ponderacao_br.gpkg"))

### Nas próximas vezes, carregar os dados salvos anteriormente
areas_ponderacao_sf <- st_read(here::here("data", "areas_ponderacao_br.gpkg"))


# Helper Functions --------------------------------------------------------
plot_city_map <- function(city, filename, w_areas, width, height) {
  setores_sf <- st_read(here::here("data/five_cities_original_2000_2010/2010",
                                   sprintf("%s2010.shp", filename)))

  b_box <- sf::st_bbox(setores_sf)
  
  b_box_center_x <- (b_box[["xmax"]] + b_box[["xmin"]]) / 2
  b_box["xmax"] <- b_box_center_x + width / 2
  b_box["xmin"] <- b_box_center_x - width / 2
  
  b_box_center_y <- (b_box[["ymax"]] + b_box[["ymin"]]) / 2
  b_box["ymax"] <- b_box_center_y + height / 2
  b_box["ymin"] <- b_box_center_y - height / 2

  setores_sf %>%
    ggplot() +
    geom_sf(size = 0.3) +
    geom_sf(data = areas_ponderacao_sf %>% filter(name_muni == city, abbrev_state == "SP"),
            colour = "red", fill = NA) +
    coord_sf(xlim = c(b_box["xmin"], b_box["xmax"]),
             ylim = c(b_box["ymin"], b_box["ymax"])) +
    theme_light() +
    labs(subtitle = city) +
    annotation_scale(location = "br", width_hint = 0.5) 
    # annotation_north_arrow(location = "bl", which_north = "true", 
    #                        pad_x = unit(0.2, "in"), pad_y = unit(0.5, "in"),
    #                        style = north_arrow_fancy_orienteering) 
  
  
}


# Plot map of five cities ----------------------------------------------
max_bbox <- find_max_bbox_size(c("Aracatuba", "Bauru", "Marilia", "Presidente_Prudente", "Sao_Jose"))

p_aracatuba <- plot_city_map("Araçatuba", "Aracatuba", areas_ponderacao_sf, max_bbox$width, max_bbox$height)
p_bauru <- plot_city_map("Bauru", "Bauru", areas_ponderacao_sf, max_bbox$width, max_bbox$height)
p_marilia <- plot_city_map("Marília", "Marilia", areas_ponderacao_sf, max_bbox$width, max_bbox$height)
p_prudente <- plot_city_map("Presidente Prudente", "Presidente_Prudente", areas_ponderacao_sf, max_bbox$width, max_bbox$height)
p_sao_jose <- plot_city_map("São José Do Rio Preto", "Sao_Jose", areas_ponderacao_sf, max_bbox$width, max_bbox$height)

p_aracatuba + p_bauru + p_marilia + p_prudente + p_sao_jose +
  # plot_annotation(title = "Five medium-sized cities from the State of São Paulo, Brazil*",
  #                 caption = "*According to Melazzo (2006) and Castello Branco (2007)") +
  plot_layout(
  design = "ABC
            DEF")


ggsave(here::here("plots", "five_cities_w_areas.png"),
       width = 23, height = 17, units = "cm", dpi = 300, scale = 1.2)







