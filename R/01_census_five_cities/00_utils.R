find_max_bbox_size <- function(cities) {
  setores_sf <- map(cities, function(city) {
    st_read(here::here("data/five_cities_original_2000_2010/2010",
                       sprintf("%s2010.shp", city)))
  })
  
  b_boxes <- map_df(setores_sf, sf::st_bbox) %>%
    mutate(width = xmax - xmin, height = ymax - ymin) %>%
    summarise(width = max(width), height = max(height))
  
  return(b_boxes)
}
