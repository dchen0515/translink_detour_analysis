library(RProtoBuf)
library(ggplot2)
library(stringr)
library(dplyr)
library(purrr)
library(tidyr)
library(sf)
library(ggspatial)

# Load the GTFS-RT proto definition
readProtoFiles("gtfs-realtime.proto")

# Read the alerts feed you downloaded
alerts <- read(transit_realtime.FeedMessage, "translink_gtfsalerts.pb")

detours <- lapply(alerts$entity, function(e) {
  alert <- e$alert
  
  if (!is.null(alert$effect) && alert$effect == 4) {
    data.frame(
      id = e$id,
      cause = alert$cause,
      effect = alert$effect,
      header = alert$header_text$translation[[1]]$text,
      description = alert$description_text$translation[[1]]$text,
      stringsAsFactors = FALSE
    )
  } else {
    NULL
  }
})

detours_df <- do.call(rbind, detours)

nrow(detours_df)

str(detours_df)

View(detours_df)

# structured detour table with real GTFS identifiers
library(purrr)
library(tidyr)

detours_entities <- lapply(alerts$entity, function(e) {
  alert <- e$alert
  
  if (!is.null(alert$effect) && alert$effect == 4) {
    ents <- alert$informed_entity
    
    if (length(ents) == 0) return(NULL)
    
    do.call(rbind, lapply(ents, function(ent) {
      data.frame(
        id = e$id,
        route_id = if (!is.null(ent$route_id)) ent$route_id else NA,
        trip_id  = if (!is.null(ent$trip$trip_id)) ent$trip$trip_id else NA,
        stop_id  = if (!is.null(ent$stop_id)) ent$stop_id else NA,
        stringsAsFactors = FALSE
      )
    }))
  } else {
    NULL
  }
})

detours_entities_df <- do.call(rbind, detours_entities)
detours_entities_df$route_id <- as.character(detours_entities_df$route_id)
detours_entities_df$trip_id  <- as.character(detours_entities_df$trip_id)
detours_entities_df$stop_id  <- as.character(detours_entities_df$stop_id)

View(detours_entities_df)


# Sorted table of detours
detours_df$route <- sub(" .*", "", detours_df$header)
sort(table(detours_df$route), decreasing = TRUE)

# Fix route extraction
detours_df$route <- str_extract(detours_df$header, "(R\\d+|N\\d+|\\d+)")

# reorder routes by frequency
detours_df$route <- factor(detours_df$route,
                           levels = names(sort(table(detours_df$route))))

## PRELIMINARY VISUALIZATIONS

# bar chart showing detours by bus route
print(
  ggplot(detours_df, aes(x = route)) +
    geom_bar(width = 0.7) +
    scale_x_discrete(expand = expansion(mult = c(0.05, 0.1))) +
    labs(
      title = "Detours by TransLink Bus Route",
      x = "Route",
      y = "Number of Detours"
    ) +
    theme(
      axis.text.x = element_text(
        size = 7,          # smaller labels
        angle = 45,        # tilt for readability
        hjust = 1,
        vjust = 1,
        margin = margin(t = 6)  # extra breathing room
      ),
      axis.text.y = element_text(size = 9),
      plot.margin = margin(10, 20, 10, 10)  # optional: more space around plot
    )
)

route_counts <- detours_df %>%
  count(route, name = "detour_count")

# histogram visualizing distribution of detour counts across routes
print(ggplot(route_counts, aes(x = detour_count)) +
        geom_histogram(
          binwidth = 1,
          boundary = 0,
          color = "black",
          fill = "steelblue"
        ) +
        labs(
          title = "Distribution of Detour Counts Across Routes",
          x = "Number of Detours",
          y = "Number of Routes"
        ) +
        theme_minimal()
      
)

# standardize cause labels
detours_df$cause <- as.character(detours_df$cause)

# table of detour causes
table(detours_df$cause_label)

# bar plot visualizing causes of detours
print(ggplot(detours_df, aes(x = cause)) +
        geom_bar(fill = "steelblue") +
        labs(
          title = "Detours by Cause",
          x = "Cause",
          y = "Number of Detours"
        ) +
        theme_minimal()
)

## LINKING WITH STATIC GFTS FILES AND ENSURE THEY ARE TYPE CHAR
routes <- read.csv("routes.txt")
trips <- read.csv("trips.txt")
stops <- read.csv("stops.txt")

routes$route_id <- as.character(routes$route_id)
trips$trip_id   <- as.character(trips$trip_id)
stops$stop_id   <- as.character(stops$stop_id)

## FULL DETOURS TABLE
detours_full <- detours_df %>%
  left_join(detours_entities_df, by = "id") %>%
  left_join(routes, by = "route_id") %>%
  left_join(stops, by = "stop_id") %>%
  left_join(trips, by = "trip_id")

View(detours_full)

### Load and convert GTFS route shapes
# Load shapes.txt
shapes <- read.csv("shapes.txt")

# Convert shape points to sf
shapes_sf <- st_as_sf(
  shapes,
  coords = c("shape_pt_lon", "shape_pt_lat"),
  crs = 4326,
  remove = FALSE
)

# Convert points → ordered lines for each shape_id
shapes_lines <- shapes_sf %>%
  arrange(shape_id, shape_pt_sequence) %>%
  group_by(shape_id) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

### FURTHER ANALYSIS BELOW HERE
## Top Detoured Routes
top_detours <- detours_full %>%
  count(route_short_name, sort = TRUE)
View(top_detours)

## Analysis of most detoured bus route (49 Metrotown Station/UBC)
detours_49 <- detours_full %>%
  filter(route_short_name == "049") %>%
  count(stop_name, sort = TRUE)
View(detours_49)

## Map of detouring clusters
detours_49_map <- detours_full %>%
  filter(route_short_name == "049") %>%
  filter(!is.na(stop_lat), !is.na(stop_lon))

detours_49_sf <- st_as_sf(
  detours_49_map,
  coords = c("stop_lon", "stop_lat"),
  crs = 4326,
  remove = FALSE
)

## Plot showing detour hotspots for route 49
print(
  ggplot(detours_49_sf) +
    geom_sf(color = "red", size = 3) +
    coord_sf() +
    theme_minimal() +
    labs(
      title = "Detour Hotspots for Route 49",
      subtitle = "Based on GTFS-Realtime detour alerts"
    )
)

### MAPPING ALL DETOURED ROUTES IN METRO VANCOUVER
detours_all_counts <- detours_full %>%
  filter(!is.na(stop_lat), !is.na(stop_lon)) %>%
  count(route_short_name, stop_id, stop_name, stop_lat, stop_lon)

detours_all_sf <- st_as_sf(
  detours_all_counts,
  coords = c("stop_lon", "stop_lat"),
  crs = 4326,
  remove = FALSE
)

p <- ggplot(detours_all_sf) +
  geom_sf(aes(color = route_short_name, size = n), alpha = 0.8) +
  coord_sf() +
  theme_minimal() +
  labs(
    title = "Metro Vancouver Detour Hotspots by Route",
    subtitle = "Color = route, size = number of detours",
    color = "Route",
    size = "Detours"
  )

print(p)

### COMPLETE MAPPING OF ALL DETOURED ROUTES IN METRO VANCOUVER
library(ggspatial)

p2 <- ggplot() +
  annotation_map_tile(type = "osm") +   # basemap
  geom_sf(data = shapes_lines, color = "grey40", size = 0.3, alpha = 0.5) +  # bus routes
  geom_sf(
    data = detours_all_sf,
    aes(color = route_short_name, size = n),
    alpha = 0.9
  ) +
  coord_sf() +
  theme_minimal() +
  labs(
    title = "Metro Vancouver Bus Routes and Detour Hotspots",
    subtitle = "Basemap + GTFS route shapes + detour points",
    color = "Route",
    size = "Detours"
  )

print(p2)


ggsave(
  filename = "complete_metro_van_detour_hotspots_map.png",
  plot = last_plot(),
  width = 10,        # inches
  height = 6,        # inches
  dpi = 600,         # high resolution
  units = "in"
)
