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
library(cancensus)
library(rmapshaper)

## ---------------------------------------------------------
## ROUTE TYPE CLASSIFICATION
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
## STRUCTURED DETOUR ENTITIES
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
  left_join(route_types,                                   by = "route_short_name") %>%
  filter(!is.na(type))

## ---------------------------------------------------------
## DETOUR POINTS AS SF
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
## CITY LABELS (FIXED GEOMETRY)
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
  mutate(
    lon_adj = lon + c(-0.020, -0.012, 0.020, 0.025, -0.012, 0.020, -0.025, 0.015),
    lat_adj = lat + c(0.012, -0.006, 0.020, -0.020, -0.012, 0.020, 0.012, -0.012)
  ) %>%
  st_as_sf(coords = c("lon_adj", "lat_adj"), crs = 4326, remove = FALSE)

## ---------------------------------------------------------
## LOAD METRO VANCOUVER BOUNDARY (STATCAN VIA API)
## ---------------------------------------------------------
mv_boundary <- get_census(
  dataset = "CA21",
  regions = list(CMA = "59933"),   # Metro Vancouver CMA
  level = "CSD",
  geo_format = "sf"
)

mv_boundary <- rmapshaper::ms_simplify(mv_boundary, keep = 0.1)

## ---------------------------------------------------------
## PROJECT ALL SPATIAL LAYERS TO A PROJECTED CRS
## ---------------------------------------------------------

target_crs <- 26910  # UTM Zone 10N

mv_boundary_proj        <- st_transform(mv_boundary, target_crs)
shapes_lines_typed_proj <- st_transform(shapes_lines_typed, target_crs)
detours_all_sf_proj     <- st_transform(detours_all_sf, target_crs)
city_labels_sf_proj     <- st_transform(city_labels_sf, target_crs)

# Extract numeric coordinates for labels
city_coords <- st_coordinates(city_labels_sf_proj)
city_labels_sf_proj$x <- city_coords[,1]
city_labels_sf_proj$y <- city_coords[,2]

## ---------------------------------------------------------
## FINAL MAP (PROJECTED CRS, NO WARNINGS, NO VIEWPORT ERRORS)
## ---------------------------------------------------------
p2 <- ggplot() +
  geom_sf(
    data = mv_boundary_proj,
    fill = "grey95",
    color = "white",
    size = 0.3
  ) +
  geom_sf(
    data = shapes_lines_typed_proj,
    aes(color = type),
    size = 0.3,
    alpha = 0.35
  ) +
  geom_sf(
    data = detours_all_sf_proj,
    aes(color = type, size = n),
    alpha = 0.9
  ) +
  geom_label_repel(
    data = city_labels_sf_proj,
    aes(x = x, y = y, label = city),
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
  
  # scale bar + north arrow
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    text_cex = 0.8
  ) +
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    style = north_arrow_fancy_orienteering,
    height = unit(1.2, "cm"),
    width = unit(1.2, "cm")
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

## ---------------------------------------------------------
## DETOURS PER ROUTE (BAR CHART)
## ---------------------------------------------------------
detours_by_route <- detours_full %>%
  filter(!is.na(route_short_name)) %>%
  count(route_short_name, sort = TRUE)

p_routes <- ggplot(detours_by_route,
                   aes(x = reorder(route_short_name, n), y = n)) +
  geom_col(fill = "#1f78b4") +
  coord_flip() +
  labs(
    title = "Detours per Route",
    x = "Route",
    y = "Number of Detours"
  ) +
  theme_minimal(base_size = 13)

print(p_routes)

ggsave(
  filename = "detours_per_route.png",
  plot = p_routes,
  width = 8,
  height = 6,
  dpi = 300
)
