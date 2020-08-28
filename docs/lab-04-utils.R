#' Nearest States
#' @param name state name not an abbr
#' @return leaflet map

map_nearest_state = function(name, rows = 70){

  states = USAboundaries::us_states() %>%
    st_transform(5070)

  state.of.interest = name

  soi = filter(states, state_name == state.of.interest)

  adjoining = st_filter(states, soi, .predicate = st_touches)

  closest = st_make_grid(soi, n = rows, square = FALSE) %>%
    st_centroid() %>%
    st_sf() %>%
    st_join(adjoining, join = st_nearest_feature)

  vor = closest %>%
    st_union() %>%
    st_voronoi() %>%
    st_cast() %>%
    st_sf() %>%
    st_join(closest) %>%
    group_by(state_name) %>%
    summarise() %>%
    st_intersection(soi)

  leaflet() %>%
    addProviderTiles(providers$CartoDB) %>%
    addPolygons(data = st_transform(vor, 4326),
                fillColor = ~colorFactor("viridis", state_name)(state_name), color = NA) %>%
    addPolygons(data = st_transform(soi, 4326),
                fillColor = "transparent",
                color = "black",
                group = "SOI") %>%
    addPolygons(data = st_transform(adjoining, 4326), fillColor = ~colorFactor("viridis", state_name)(state_name), col = NA) %>%
    addLayersControl(overlayGroups = c("SOI"))
}

