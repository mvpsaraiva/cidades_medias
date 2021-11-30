# Introdução --------------------------------------------------------------

#' Este script compatibiliza as classes de renda entre os censos de 2000 e
#' 2010, considerando a inflação e os reajustes do salário mínimo do período.



# Setup -------------------------------------------------------------------
library("tidyverse")
library("sf")
library("scales")
library("patchwork")
library("Hmisc")
library("viridis")

source(here::here("R/01_census_five_cities", "00_utils.R"))

# city <- "Bauru"
# filename <- "Bauru"


# Plot Map Function --------------------------------------------------------
plot_census_changes_map <- function(city, filename, width, height) {
  
  census_df <- read_rds(here::here("data/five_cities_original_2000_2010", 
                                   sprintf("%s.rds", filename)))
  
  census_df <- census_df %>% 
    mutate(income_class = factor(income_class,
                                 levels = c("from 0 to 0.5 mw",
                                            "from 0.5 to 1 mw",
                                            "from 1 to 2 mw",
                                            "from 2 to 3 mw",
                                            "from 3 to 5 mw",
                                            "from 5 to 10 mw",
                                            "from 10 to 15 mw",
                                            "from 10 to 20 mw",
                                            "above 20 mw"),
                                 labels = c("I", "H", "G", "F", "E", "D", "C", "B", "A")))
  
  setores_2000_sf <- st_read(here::here("data/five_cities_original_2000_2010", 
                                        sprintf("%s_2000.gpkg", filename)))
  setores_2010_sf <- st_read(here::here("data/five_cities_original_2000_2010", 
                                        sprintf("%s_2010.gpkg", filename)))

  b_box <- sf::st_bbox(setores_2010_sf)
  
  b_box_center_x <- (b_box[["xmax"]] + b_box[["xmin"]]) / 2
  b_box["xmax"] <- b_box_center_x + width / 2
  b_box["xmin"] <- b_box_center_x - width / 2
  
  b_box_center_y <- (b_box[["ymax"]] + b_box[["ymin"]]) / 2
  b_box["ymax"] <- b_box_center_y + height / 2
  b_box["ymin"] <- b_box_center_y - height / 2

  census_df %>%
    group_by(census_tract, year, scenario) %>%
    arrange(desc(resp)) %>%
    slice(1) %>%
    left_join(rbind(setores_2000_sf, setores_2010_sf),
              by = c("census_tract", "year")) %>%
    mutate(category = paste(year, scenario)) %>%
    mutate(category = factor(category, 
                             levels = c("2000 original", "2010 original", "2010 adjusted_ipca"),
                             labels = c("2000", "2010", "2010 adjusted"))) %>%
    mutate(city = city) %>%
    ggplot() +
    geom_sf(aes(fill=income_class, geometry=geom), size = 0.2) +
    coord_sf(xlim = c(b_box["xmin"], b_box["xmax"]),
             ylim = c(b_box["ymin"], b_box["ymax"])) +
    scale_fill_brewer(palette = "RdBu", direction = -1, 
                      guide = guide_legend(byrow = TRUE, ncol = 1, reverse = TRUE), 
                      drop = FALSE) +
    # scale_fill_viridis_d() +
    facet_grid(city~category, switch = "y") +
    theme_minimal() +
    theme(legend.position = "right",
          legend.title = element_blank(),
          panel.border = element_rect(colour = "grey40", fill = NA),
          axis.text = element_blank()) 
    # labs(subtitle = city)

}

## Plot map of five cities ----------------------------------------------
max_bbox <- find_max_bbox_size(c("Aracatuba", "Bauru", "Marilia", "Presidente_Prudente", "Sao_Jose"))

p_aracatuba <- plot_census_changes_map("Araçatuba", "Aracatuba", max_bbox$width, max_bbox$height)
p_bauru <- plot_census_changes_map("Bauru", "Bauru", max_bbox$width, max_bbox$height)
p_marilia <- plot_census_changes_map("Marília", "Marilia", max_bbox$width, max_bbox$height)
p_prudente <- plot_census_changes_map("Presidente Prudente", "Presidente_Prudente", max_bbox$width, max_bbox$height)
p_sao_jose <- plot_census_changes_map("São José Do Rio Preto", "Sao_Jose", max_bbox$width, max_bbox$height)


(p_aracatuba / p_bauru / p_marilia / p_prudente / p_sao_jose) +
  # plot_annotation(title = "Changes in predominant income class betweem 2000 and 2010",
  #                 subtitle = "by census tract",
  #                 caption = "Five medium-sized cities from the State of São Paulo, Brazil,\naccording to Melazzo (2006) and Castello Branco (2007)") +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

ggsave(here::here("plots/5 cities", "five_cities_census_changes_ipca.png"),
       width = 14, height = 27, units = "cm", dpi = 300, scale = 1.5)


# city = "Araçatuba"
# filename = "Aracatuba"
# Plot Barchart Function --------------------------------------------------
plot_census_changes_chart <- function(city, filename) {
  census_df <- read_rds(here::here("data/five_cities_original_2000_2010", 
                                   sprintf("%s.rds", filename)))
  
  census_df <- census_df %>% 
    mutate(income_class = factor(income_class,
                                 levels = c("from 0 to 0.5 mw",
                                            "from 0.5 to 1 mw",
                                            "from 1 to 2 mw",
                                            "from 2 to 3 mw",
                                            "from 3 to 5 mw",
                                            "from 5 to 10 mw",
                                            "from 10 to 15 mw",
                                            "from 10 to 20 mw",
                                            "above 20 mw"),
                                 labels = c("I", "H", "G", "F", "E", "D", "C", "B", "A"))) %>%
    mutate(income_class = fct_rev(income_class))
  
  p <- census_df  %>%
    group_by(year, scenario, income_class) %>%
    summarise(households = sum(resp, na.rm = TRUE)) %>%
    mutate(category = paste(year, scenario)) %>%
    mutate(category = factor(category, 
                             levels = c("2000 original", "2010 original", "2010 adjusted_ipca"),
                             labels = c("2000", "2010", "2010 adj."))) %>%
    drop_na() %>%
    ggplot() +
    geom_col(aes(x=income_class, y = households, fill=category), position = "dodge") +
    scale_fill_brewer(palette = "Set1") +
    scale_y_continuous(labels = comma_format()) +
    theme_light() +
    theme(legend.position = "right") +
    labs(title = city,
         y = "number of households",
         x = "income class",
         fill = "year")
  
  ggsave(plot = p, here::here("plots", sprintf("barchart_%s.png", city)),
         width = 16, height = 7, units = "cm", dpi = 300, scale = 1)
  
  p <- census_df  %>%
    group_by(year, scenario, income_class) %>%
    summarise(households = sum(resp, na.rm = TRUE)) %>%
    mutate(p_households = households / sum(households),
           category = paste(year, scenario),
           category = factor(category, 
                             levels = c("2000 original", "2010 original", "2010 adjusted_ipca"),
                             labels = c("2000", "2010", "2010 adj."))) %>%
    drop_na() %>%
    ggplot() +
    geom_col(aes(x=income_class, y = p_households, fill=category), position = "dodge") +
    scale_fill_brewer(palette = "Set1") +
    scale_y_continuous(labels = percent_format(), limits = c(0, 0.4)) +
    theme_light() +
    theme(legend.position = "right",
          axis.title = element_blank()) +
    labs(title = city,
         fill = "year")

  ggsave(plot = p, here::here("plots", sprintf("perc_barchart_%s.png", city)),
         width = 16, height = 7, units = "cm", dpi = 300, scale = 1)
  
  return(p)
}



## Plot Barchart of Five Cities --------------------------------------------

p_ara <- plot_census_changes_chart("Araçatuba", "Aracatuba")
p_bau <- plot_census_changes_chart("Bauru", "Bauru")
p_mar <- plot_census_changes_chart("Marília", "Marilia")
p_pre <- plot_census_changes_chart("Presidente Prudente", "Presidente_Prudente")
p_sjp <- plot_census_changes_chart("São José Do Rio Preto", "Sao_Jose")

layout <- "
AAABBB
CCCDDD
EEEF##
"

p_ara + p_bau + p_mar + p_pre + p_sjp + guide_area() +
  plot_layout(design = layout, guides = 'collect')

ggsave(plot = last_plot(), 
       here::here("plots", "perc_barchart_ipca.png"),
       width = 16, height = 13, units = "cm", dpi = 300, scale = 1)


# Plot Map Changes Function --------------------------------------------------------
plot_census_changes_single_map_h <- function(city, filename, width, height) {
  
  census_df <- read_rds(here::here("data/five_cities_original_2000_2010", 
                                   sprintf("%s.rds", filename)))
  
  census_df <- census_df %>% 
    mutate(income_class = factor(income_class,
                                 levels = c("from 0 to 0.5 mw",
                                            "from 0.5 to 1 mw",
                                            "from 1 to 2 mw",
                                            "from 2 to 3 mw",
                                            "from 3 to 5 mw",
                                            "from 5 to 10 mw",
                                            "from 10 to 15 mw",
                                            "from 10 to 20 mw",
                                            "above 20 mw"),
                                 labels = c("I", "H", "G", "F", "E", "D", "C", "B", "A")))
  
  setores_2000_sf <- st_read(here::here("data/five_cities_original_2000_2010", 
                                        sprintf("%s_2000.gpkg", filename)))
  setores_2010_sf <- st_read(here::here("data/five_cities_original_2000_2010", 
                                        sprintf("%s_2010.gpkg", filename)))
  
  b_box <- sf::st_bbox(setores_2010_sf)
  
  b_box_center_x <- (b_box[["xmax"]] + b_box[["xmin"]]) / 2
  b_box["xmax"] <- b_box_center_x + width / 2
  b_box["xmin"] <- b_box_center_x - width / 2
  
  b_box_center_y <- (b_box[["ymax"]] + b_box[["ymin"]]) / 2
  b_box["ymax"] <- b_box_center_y + height / 2
  b_box["ymin"] <- b_box_center_y - height / 2
  
  p <- census_df %>%
    left_join(rbind(setores_2000_sf, setores_2010_sf),
              by = c("census_tract", "year")) %>%
    mutate(category = paste(year, scenario)) %>%
    mutate(category = factor(category, 
                             levels = c("2000 original", "2010 original", "2010 adjusted_ipca"),
                             labels = c("2000", "2010", "2010 adjusted"))) %>%
    mutate(city = city) %>%
    drop_na() %>%
    ggplot() +
    # geom_sf(aes(geometry=geom), fill = "grey80", color = NA) +
    geom_sf(aes(fill=resp, geometry=geom), color = NA) +
    coord_sf(xlim = c(b_box["xmin"], b_box["xmax"]),
             ylim = c(b_box["ymin"], b_box["ymax"])) +
    scale_fill_distiller(palette = "Reds", direction = 1) +
    # scale_fill_brewer(palette = "RdBu", direction = -1, 
    #                   guide = guide_legend(byrow = TRUE, ncol = 1, reverse = TRUE), 
    #                   drop = FALSE) +
    # scale_fill_viridis_d() +
    facet_grid(income_class~category, switch = "y") +
    theme_minimal() +
    theme(legend.position = "right",
          panel.border = element_rect(colour = "grey40", fill = NA),
          axis.text = element_blank()) +
    labs(fill = "Population")
  
  ggsave(plot = p, 
         here::here("plots", paste0("detailed_map_", filename, ".png")),
         width = 14, height = 25, units = "cm", dpi = 300, scale = 1)
  
  return(p)
  
}

p_aracatuba <- plot_census_changes_single_map_h("Araçatuba", "Aracatuba", max_bbox$width, max_bbox$height)
p_bauru <- plot_census_changes_single_map_h("Bauru", "Bauru", max_bbox$width, max_bbox$height)
p_marilia <- plot_census_changes_single_map_h("Marília", "Marilia", max_bbox$width, max_bbox$height)
p_prudente <- plot_census_changes_single_map_h("Presidente Prudente", "Presidente_Prudente", max_bbox$width, max_bbox$height)
p_sao_jose <- plot_census_changes_single_map_h("São José Do Rio Preto", "Sao_Jose", max_bbox$width, max_bbox$height)


