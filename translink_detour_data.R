library(RProtoBuf)
library(ggplot2)
library(stringr)
library(dplyr)
library(purrr)
library(tidyr)

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
View(detours_entities_df)


# Sorted table of detours
detours_df$route <- sub(" .*", "", detours_df$header)
sort(table(detours_df$route), decreasing = TRUE)

# Fix route extraction
detours_df$route <- str_extract(detours_df$header, "(R\\d+|N\\d+|\\d+)")

library(ggplot2)

# reorder routes by frequency
detours_df$route <- factor(detours_df$route,
                           levels = names(sort(table(detours_df$route))))

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


# ggsave(
#   filename = "detour_count_distribution.png",
#   plot = last_plot(),
#   width = 10,        # inches
#   height = 6,        # inches
#   dpi = 600,         # high resolution
#   units = "in"
# )
