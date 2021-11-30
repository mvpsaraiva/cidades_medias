# Introdução --------------------------------------------------------------

#' Este script compatibiliza as classes de renda entre os censos de 2000 e
#' 2010, considerando a inflação (IPCA) e os reajustes do salário mínimo do 
#' período.



# Setup -------------------------------------------------------------------
library("tidyverse")
library("sf")
library("scales")
library("patchwork")
library("Hmisc")


# city <- "Araçatuba"
# filename <- "Aracatuba"

adjust_census <- function(city, filename) {
  ### Minimum wage values
  MW_2000 <- 151.00
  MW_2000_adj <- 291.62 # IPCA
  # MW_2000_igp <- 352.36 # IGP-M 
  MW_2010 <- 510
  
  inflation <- 1.93125610 # IGP-M 2.33349840
  # inflation_igp <- 2.33349840 # IGP-M
  
  ## original classification 
  mw_classes_2000 <- c(0, 0.5, 1, 2, 3, 5, 10, 15, 20, 500) * MW_2000
  mw_classes_2010 <- c(0, 0.5, 1, 2, 3, 5, 10, 15, 20, 500) * MW_2010
  ## adjusted classification 
  mw_classes_comp <- c(0, 0.5, 1, 2, 3, 5, 10, 15, 20, 500) * MW_2000_adj
  # mw_classes_igp <- c(0, 0.5, 1, 2, 3, 5, 10, 15, 20, 500) * MW_2000_igp
  
  mw_labels <- c("from 0 to 0.5 mw", 
                 "from 0.5 to 1 mw", 
                 "from 1 to 2 mw", 
                 "from 2 to 3 mw", 
                 "from 3 to 5 mw", 
                 "from 5 to 10 mw", 
                 "from 10 to 15 mw", 
                 "from 10 to 20 mw", 
                 "above 20 mw")
  
  
  ### load and process 2000 data
  setores_2000_sf <- st_read(here::here("data/five_cities_original_2000_2010/2000",
                                        sprintf("%s2000.shp", filename))) 
  
  census_2000 <- setores_2000_sf %>%
    select(census_tract = ID_, respmeio:rendacima2) %>%
    st_set_geometry(NULL) %>%
    rename(respmeiosm = respmeio, respaci20s = respacima2, rendaci20s = rendacima2) %>%
    mutate(year = 2000, scenario = "original") %>%
    pivot_longer(cols = respmeiosm:rendaci20s) %>%
    mutate(variable = str_sub(name, 1, 4),
           group = str_sub(name, 5)) %>%
    select(-name) %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    filter(resp > 0) %>%
    mutate(income_per_capita = rend / resp) %>%
    mutate(income_class = cut(income_per_capita, 
                              breaks = mw_classes_2000, 
                              labels = mw_labels, 
                              include.lowest = TRUE))
  
  setores_2000_sf <- setores_2000_sf %>%
    mutate(year = 2000) %>%
    select(census_tract = ID_, year) 
  
  ### load and process 2010 data
  setores_2010_sf <- st_read(here::here("data/five_cities_original_2000_2010/2010",
                                        sprintf("%s2010.shp", filename))) 
  
  census_2010_original <- setores_2010_sf %>%
    select(census_tract = CD_GEOCODI, respmeiosm:rendaci20s) %>%
    st_set_geometry(NULL) %>%
    rename(respaci20s = respacima2) %>%
    mutate(year = 2010, scenario = "original") %>%
    pivot_longer(cols = respmeiosm:rendaci20s) %>%
    mutate(variable = str_sub(name, 1, 4),
           group = str_sub(name, 5)) %>%
    select(-name) %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    filter(resp > 0) %>%
    mutate(income_per_capita = rend / resp) %>%
    mutate(income_class = cut(income_per_capita,
                              breaks = mw_classes_2010, 
                              labels = mw_labels, 
                              include.lowest = TRUE))
  
  census_2010_adjusted <- setores_2010_sf %>%
    select(census_tract = CD_GEOCODI, respmeiosm:rendaci20s) %>%
    st_set_geometry(NULL) %>%
    rename(respaci20s = respacima2) %>%
    mutate(year = 2010, scenario = "adjusted_ipca") %>%
    pivot_longer(cols = respmeiosm:rendaci20s) %>%
    mutate(variable = str_sub(name, 1, 4),
           group = str_sub(name, 5)) %>%
    select(-name) %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    filter(resp > 0) %>%
    mutate(income_per_capita = rend / resp) %>%
    mutate(income_class = cut(income_per_capita,
                              breaks = mw_classes_comp, 
                              labels = mw_labels, 
                              include.lowest = TRUE))
  
  setores_2010_sf <- setores_2010_sf %>%
    mutate(year = 2010) %>%
    select(census_tract = CD_GEOCODI, year)  
  
  ### save results
  rbind(census_2000, census_2010_original, census_2010_adjusted) %>% 
    write_rds(here::here("data/five_cities_original_2000_2010", sprintf("%s.rds", filename)))
  
  rbind(census_2000, census_2010_original, census_2010_adjusted) %>% 
    write_csv(here::here("data/five_cities_original_2000_2010", sprintf("%s.csv", filename)))

  setores_2000_sf %>%
    st_write(here::here("data/five_cities_original_2000_2010", sprintf("%s_2000.gpkg", filename)))
  
  setores_2010_sf %>%
    st_write(here::here("data/five_cities_original_2000_2010", sprintf("%s_2010.gpkg", filename)))
  
}

adjust_census("Araçatuba", "Aracatuba")
adjust_census("Bauru", "Bauru")
adjust_census("Marília", "Marilia")
adjust_census("Presidente Prudente", "Presidente_Prudente")
adjust_census("São José Do Rio Preto", "Sao_Jose")












  
