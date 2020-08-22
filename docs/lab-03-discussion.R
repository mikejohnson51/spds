##################################################
## Project: Lab 03
## Script purpose:  Week 3
## Date: August 19-20, 2020
##################################################

library(tidyverse)
library(sf)
library(units)
library(ggrepel) # install.packages("ggrepel")
library(gghighlight) # install.packages("gghighlight")

# Region Classifier (data.frame)
region = data.frame(region = state.region, state_name = state.name)

# Create a sf object of the southern states
south =  USAboundaries::us_states()
  right_join(region, by = "state_name") %>%
  filter(region == "South")

# Read in the cities data from lab 03
# Convert to sf object treating lng, lat as XY in the WGS84 CRS (EPSG:4326)
# Spatially filter the cities using the southern states and the predicate st_intersects

cities = readr::read_csv("data/uscities.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_filter(south, .predicate = st_intersects)

# We want 1:1 distances between state borders and each city
# So, cast the existing 17 feature `south` object into a single MULTIPOLYGON object
# Since we want distance to boarder, we need to cast our geometries in a MULTILINESTRING form

south_c = st_combine(south) %>%
  st_cast("MULTILINESTRING")

# Now, to take a measure (distance) we want our data in a projected system
# Due to the size of cities (~10,000), if we would have left our EPSG:4326 CRS
# Great Circle distance would be generated making our machine slow...
# Here we will use the CONUS Equal Area Albers (EPSG:5070)
# In your lab you will use a distance preserving projection which is a better choice!

south_c = st_transform(south_c, 5070)
cities  = st_transform(cities,  5070)

# Now that the CRS is good, lets "mutate" a new column onto the existing cities set
# The default unit of meter [m] is a bit small for regional measures so lets convert to km
# We also need to drop our units so that we can use the dist_to_state as a numeric vector elsewhere

cities = cities %>%
  mutate(dist_to_state = st_distance(cities, south_c),
         dist_to_state = units::set_units(dist_to_state,"km"),
         dist_to_state = units::drop_units(dist_to_state))

#The above could be written more compactly as:

# cities = cities %>%
#   mutate(dist_to_state = drop_units(set_units(st_distance(cities, south_c), "km")))


# We are also interested in what the most populous city in each state is.
# We will label these on our final plots, to do this we use or dplyr verbs

big_cities = cities %>%
  group_by(state_name) %>%
  slice_max(population, n = 2)


# Time to plot! -----------------------------------------------------------

# Here we learn a few new things:
  # geom_sf lets us as sf object to a ggplot
  # scale_color_gradient changes th gradient of the color aes()
    # Here we set it to a gradient that scales from gray for low values to red for large values
  # We also use ggrepel::geom_label_repel to intelligently label the biggest cities in each state
    # data: the data we want to label
    # aes(): what variable should be drawn (city) and where (geometry)
    # stat: as the standard labeling method in in cartisian space we need to apply a statistical transformations to the labels
    # This transformations is to treat the coordinates as "sf_coordinates"
    # Last, we can change the size

ggplot() +
  geom_sf(data = south_c) +
  geom_sf(data = cities, aes(col = dist_to_state), size = .1) +
  geom_sf(data = big_cities, col = "navy") +
  scale_color_gradient(low = "gray", high = "red") +
  ggthemes::theme_map() +
  ggrepel::geom_label_repel(
    data = big_cities,
    aes(label = city, geometry = geometry),
    stat = "sf_coordinates",
    size = 4
  ) +
  labs(title = "Labeling Example",
       col = "Distance (km)")

# Example for saving the image
ggsave(filename = "img/labeled-map.png", width = 10)


# Here we make a new map:
  # Everything is the same as above except we add gghighlight
  # gghighlight is effectively a filter saying which points should be colored according to the color scale
  # In this case we say only those cities with a population > 10,000

ggplot() +
  geom_sf(data = south_c) +
  geom_sf(data = cities, aes(col = dist_to_state), size = .1) +
  gghighlight::gghighlight(population > 10000) +
  geom_sf(data = big_cities, col = "navy") +
  scale_color_gradient(low = "gray", high = "red") +
  ggthemes::theme_map() +
  labs(title = "Highlighting Example",
       col = "Distance (km)")


