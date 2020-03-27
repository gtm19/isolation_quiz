library(tidyverse)
library(rnaturalearth)
library(sf)

worldmap <- 
  rnaturalearth::ne_countries(scale = "large", returnclass = "sf") %>% 
  filter(!is.na(iso_a3)) %>% 
  filter(as.numeric(pop_est) > 1e7)

plot_country <- function(iso3 = "CAN") {
  
  countries <- c(iso3,
                 sample(worldmap$iso_a3[!worldmap$iso_a3 == iso3], 3, replace = FALSE))
  
  sf_df <- filter(worldmap, iso_a3 %in% countries)
  
  for(i in seq_along(countries)) {
    
  dat <- filter(sf_df, iso_a3 == countries[i])
  
  coords <- as.vector(st_coordinates(st_centroid(dat)))
  
  proj <- glue::glue("+proj=merc +lat_0={coords[2]} +lon_0={coords[1]}")
    
  filename <- paste0("country_images/", iso3, "_", letters[i], ".png")
    
    plot <- 
      dat %>% 
      st_transform(crs = proj) %>% 
      ggplot() +
      geom_sf(fill = "#b3cbff", colour = "#4d5a75") +
      # geom_sf_label(data = city, aes(label = name)) +
      theme_void()
    
    ggsave(filename, plot = plot)
      
  }
  
  invisible()
  
}

countries <-
  c(
    "PAK",
    "GRC",
    "SWE",
    "MYS",
    "COL",
    "MEX"
    )

set.seed(13)
lapply(countries, plot_country)

