


# End goal - Map of Europe with railway lines plotted and filtered by operator (respondents to SYMBIOSIS survey) #

library(sf)
library(ggplot2)
library(osmdata)
library(rnaturalearth)
library(osmdata)
library(purrr)
library(dplyr)


# Simple basemap
europe <- ne_countries(
  continent = "Europe",
  returnclass = "sf"
)


# Plot Europe

#disable s2 to avoid geometry errors
sf_use_s2(FALSE)

#Loading Europe countries as an sf object
europe <- ne_countries(
  continent = "Europe",
  returnclass = "sf"
)

#define bounding box for Europe
europe_bbox <- st_bbox(
  c(
    xmin = -25,  # west
    ymin = 34,   # south
    xmax = 45,   # east
    ymax = 72    # north
  ),
  crs = st_crs(europe)
)

#crop Europe to bounding box
europe_crop <- st_crop(europe, europe_bbox)

#plot cropped map
ggplot() +
  geom_sf(data = europe_crop, fill = "grey95", colour = "grey70") +
  theme_minimal() +
  labs(title = "Europe basemap (cropped)")





# Railway lines - test with Austria

austria <- europe_crop[europe_crop$name == "Austria", ]

#define bounding box - just around vienna (Austria too big to work)
vienna_bbox <- c(
  xmin = 16.2,
  ymin = 48.0,
  xmax = 16.7,
  ymax = 48.4
)

#build OSM query
rail_query <- opq(bbox = vienna_bbox, timeout = 25) %>%
  add_osm_feature(key = "railway", value = "rail")

#download data
rail_data <- osmdata_sf(rail_query)

#extract railway line geometries
rail_lines <- rail_data$osm_lines

#plot: Austria context + Vienna railways
ggplot() +
  geom_sf(data = austria, fill = "grey95", colour = "grey70") +
  geom_sf(data = rail_lines, colour = "black", linewidth = 0.3) +
  coord_sf(
    xlim = c(16.2, 16.7),
    ylim = c(48.0, 48.4)
  ) +
  theme_minimal() +
  labs(title = "Railway lines around Vienna")

# Filter by specific railway companies 

#inspect attribute table and search specific operator
names(rail_lines)
unique(rail_lines$operator)

#create an NA-safe logical filter
is_obb_infra <- !is.na(rail_lines$operator) &
  rail_lines$operator == "ÖBB-Infrastruktur AG"

#subset sf object 
obb_infra_lines <- rail_lines[is_obb_infra, ]

#check and plot
nrow(obb_infra_lines)
st_geometry_type(obb_infra_lines)

ggplot() +
  geom_sf(data = rail_lines, colour = "grey85", linewidth = 0.25) +
  geom_sf(data = obb_infra_lines, colour = "red", linewidth = 0.6) +
  coord_sf(
    xlim = c(16.2, 16.7),
    ylim = c(48.0, 48.4)
  ) +
  theme_minimal() +
  labs(
    title = "ÖBB-Infrastruktur AG railway lines (Vienna region)",
    subtitle = "Infrastructure operator only"
  )

# Scaling up and looking at the railway lines over all Austria (tiled)

#ensure s2 is still disabled
sf_use_s2(FALSE)

#get Austria bounding box
austria_bbox <- st_bbox(austria)

#create a grid over Austria (controls tile size)
austria_grid <- st_make_grid(
  austria,
  n = c(4, 4),          
  what = "polygons"
)

#convert grid to sf
austria_tiles <- st_sf(geometry = austria_grid)

#crop tiles to Austria boundary
austria_tiles <- st_intersection(austria_tiles, austria)

#function to download railways for ONE tile
get_railways_tile <- function(tile_geom) {
    tile_bbox <- st_bbox(tile_geom)
    query <- opq(
    bbox = tile_bbox,
    timeout = 25
  ) %>%
    add_osm_feature(key = "railway", value = "rail")
    result <- tryCatch(
    osmdata_sf(query)$osm_lines,
    error = function(e) NULL
  )
    return(result)
}

#download railways tile by tile
railway_tiles <- map(austria_tiles$geometry, get_railways_tile)

#remove failed tiles
railway_tiles <- railway_tiles[!map_lgl(railway_tiles, is.null)]

#combine all tiles into one sf object
rail_lines_austria <- bind_rows(railway_tiles)

#remove duplicate geometries (tile overlaps)
rail_lines_austria <- rail_lines_austria[!duplicated(st_as_text(st_geometry(rail_lines_austria))), ]

#filter to ÖBB-Infrastruktur AG (NA-safe)
is_obb_infra <- !is.na(rail_lines_austria$operator) &
  rail_lines_austria$operator == "ÖBB-Infrastruktur AG"

obb_infra_austria <- rail_lines_austria[is_obb_infra, ]

#check and plot across all Austria
nrow(rail_lines_austria)
nrow(obb_infra_austria)
unique(rail_lines_austria$operator)

ggplot() +
  geom_sf(data = austria, fill = "grey95", colour = "grey70") +
  geom_sf(data = rail_lines_austria, colour = "grey85", linewidth = 0.2) +
  geom_sf(data = obb_infra_austria, colour = "red", linewidth = 0.4) +
  theme_minimal() +
  labs(
    title = "ÖBB-Infrastruktur AG railway network (Austria)",
    subtitle = "Red = ÖBB infrastructure, Grey = all railways"
  )




