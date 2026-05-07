## ---------------------------------------------------------
## LIBRARIES
## ---------------------------------------------------------
library(RProtoBuf)
library(ggplot2)
library(stringr)
library(dplyr)
library(purrr)
library(tidyr)
library(sf)
library(ggspatial)
library(ggrepel)
library(magrittr)

## ---------------------------------------------------------
## ROUTE TYPE CLASSIFICATION (MUST COME FIRST)
## ---------------------------------------------------------
route_types <- data.frame(
  route_short_name = c(
    "002","009","015","022","023","025","028","033","049","050","144","151","157",
    "310","312","316","321","326","350","391",
    "004","005","006","010","014","016","017","019","041",
    "084","143","145","209","210","211","240","246","250","257","351","354","501",
    "099","R4","R5",
    "N9","N35",
    "068","361","362","363"
  ),
  type = c(
    rep("Local", 20),
    rep("Trolley", 9),
    rep("Suburban/Express", 13),
    rep("RapidBus/B-Line", 3),
    rep("NightBus", 2),
    rep("Community Shuttle", 4)
  )
)

type_colors <- c(
  "RapidBus/B-Line"   = "#00A651",
  "Trolley"           = "#1f78b4",
  "Local"             = "#e31a1c",
  "Suburban/Express"  = "#ff7f00",
  "NightBus"          = "#6a3d9a",
  "Community Shuttle" = "#b15928"
)

## ---------------------------------------------------------
## LOAD GTFS-RT ALERTS
## ---------------------------------------------------------
readProtoFiles("gtfs-realtime.proto")
alerts <- read(transit_realtime.FeedMessage, "translink_gtfsalerts.pb")

detours <- lapply(alerts$entity, function(e) {
  alert <- e$alert
  if (!is.null(alert$effect) && alert$effect == 4) {
    data.frame(
      id          = e$id,
      cause       = alert$cause,
      effect      = alert$effect,
      header      = alert$header_text$translation[[1]]$text,
      description = alert$description_text$translation[[1]]$text,
      stringsAsFactors = FALSE
    )
  } else NULL
})

detours_df <- do.call(rbind, detours)

## ---------------------------------------------------------
## STRUCTURED DETOUR ENTITIES (NO %||%)
## ---------------------------------------------------------
detours_entities <- lapply(alerts$entity, function(e) {
  alert <- e$alert
  if (!is.null(alert$effect) && alert$effect == 4) {
    ents <- alert$informed_entity
    if (length(ents) == 0) return(NULL)
    
    do.call(rbind, lapply(ents, function(ent) {
      data.frame(
        id       = e$id,
        route_id = if (!is.null(ent$route_id)) ent$route_id else NA,
        trip_id  = if (!is.null(ent$trip$trip_id)) ent$trip$trip_id else NA,
        stop_id  = if (!is.null(ent$stop_id)) ent$stop_id else NA,
        stringsAsFactors = FALSE
      )
    }))
  } else NULL
})

detours_entities_df <- do.call(rbind, detours_entities) %>%
  mutate(
    route_id = as.character(route_id),
    trip_id  = as.character(trip_id),
    stop_id  = as.character(stop_id)
  )

## ---------------------------------------------------------
## LOAD STATIC GTFS
## ---------------------------------------------------------
routes <- read.csv("routes.txt")
trips  <- read.csv("trips.txt")
stops  <- read.csv("stops.txt")

routes$route_id <- as.character(routes$route_id)
trips$trip_id   <- as.character(trips$trip_id)
stops$stop_id   <- as.character(stops$stop_id)

## ---------------------------------------------------------
## FULL DETOURS TABLE
## ---------------------------------------------------------
detours_full <- detours_df %>%
  left_join(detours_entities_df, by = "id") %>%
  left_join(routes, by = "route_id") %>%
  left_join(stops,  by = "stop_id") %>%
  left_join(trips,  by = "trip_id")

## ---------------------------------------------------------
## LOAD SHAPES + BUILD LINES
## ---------------------------------------------------------
shapes <- read.csv("shapes.txt")

shapes_sf <- st_as_sf(
  shapes,
  coords = c("shape_pt_lon", "shape_pt_lat"),
  crs = 4326,
  remove = FALSE
)

shapes_lines <- shapes_sf %>%
  arrange(shape_id, shape_pt_sequence) %>%
  group_by(shape_id) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

## ---------------------------------------------------------
## ASSIGN ROUTE TYPES TO SHAPES
## ---------------------------------------------------------
shapes_lines_typed <- shapes_lines %>%
  left_join(trips  %>% select(route_id, shape_id),         by = "shape_id") %>%
  left_join(routes %>% select(route_id, route_short_name), by = "route_id") %>%
  left_join(route_types,                                   by = "route_short_name")

## (Optional) drop shapes with no type to remove NA from legend
shapes_lines_typed <- shapes_lines_typed %>%
  filter(!is.na(type))

## ---------------------------------------------------------
## DETOUR POINTS AS SF + JOIN TYPES
## ---------------------------------------------------------
detours_all_counts <- detours_full %>%
  filter(!is.na(stop_lat), !is.na(stop_lon)) %>%
  count(route_short_name, stop_id, stop_name, stop_lat, stop_lon)

detours_all_sf <- st_as_sf(
  detours_all_counts,
  coords = c("stop_lon", "stop_lat"),
  crs = 4326,
  remove = FALSE
) %>%
  left_join(route_types, by = "route_short_name")

## ---------------------------------------------------------
## CITY LABELS (WITH ADJUSTED SF GEOMETRY)
## ---------------------------------------------------------
city_labels <- data.frame(
  city = c("Vancouver", "Richmond", "Burnaby", "Surrey", "Delta",
           "North Vancouver", "West Vancouver", "New Westminster"),
  lon  = c(-123.10, -123.13, -122.97, -122.85, -123.00,
           -123.07, -123.17, -122.91),
  lat  = c(49.25, 49.17, 49.25, 49.12, 49.09,
           49.32, 49.33, 49.21)
)

city_labels_sf <- city_labels %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  mutate(
    lon_adj = lon + c(
      -0.020,  # Vancouver
      -0.012,  # Richmond
      0.020,   # Burnaby
      0.025,   # Surrey
      -0.012,  # Delta
      0.020,   # North Vancouver
      -0.025,  # West Vancouver
      0.015    # New Westminster
    ),
    lat_adj = lat + c(
      0.012,   # Vancouver
      -0.006,  # Richmond
      0.020,   # Burnaby
      -0.020,  # Surrey
      -0.012,  # Delta
      0.020,   # North Vancouver
      0.012,   # West Vancouver
      -0.012   # New Westminster
    ),
    geometry_adj = st_sfc(
      mapply(function(x, y) st_point(c(x, y)), lon_adj, lat_adj, SIMPLIFY = FALSE),
      crs = 4326
    )
  )

## ---------------------------------------------------------
## FINAL MAP
## ---------------------------------------------------------
p2 <- ggplot() +
  annotation_map_tile(type = "cartolight") +  # faster/more reliable than osm
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  geom_sf(
    data = shapes_lines_typed,
    aes(color = type),
    size = 0.3,
    alpha = 0.35
  ) +
  
  geom_sf(
    data = detours_all_sf,
    aes(color = type, size = n),
    alpha = 0.9
  ) +
  
  geom_label_repel(
    data = city_labels_sf,
    aes(label = city, geometry = geometry_adj),
    stat = "sf_coordinates",
    size = 4,
    fontface = "bold",
    color = "black",
    fill = "white",
    label.size = 0.3,
    box.padding = 0.8,
    point.padding = 0.8,
    force = 2,
    max.overlaps = Inf
  ) +
  
  scale_color_manual(values = type_colors) +
  coord_sf() +
  theme_minimal() +
  labs(
    title = "Metro Vancouver Bus Routes and Detour Hotspots",
    subtitle = "Routes grouped by service type; RapidBus shown in green",
    color = "Route Type",
    size  = "Detours"
  )

print(p2)

ggsave(
  filename = "complete_colour_coded_metro_van_detour_hotspots_and_route_map.png",
  plot = p2,
  width = 10,
  height = 6,
  dpi = 600,
  units = "in"
)
