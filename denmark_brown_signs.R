library(readxl)
library(dotenv)
library(openrouteservice)
library(sf)
library(leaflet)
library(purrr)
library(tmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(jsonlite)
library(osmdata)
library(dplyr)
library(av)

# Load environment variables from .env file
dotenv::load_dot_env()

# Read the ORS API key from the environment variable
ors_api_key(Sys.getenv("ORS_API_KEY"))

# Get Denmark as sf polygon
denmark <- ne_countries(scale = 10, country = "Denmark", returnclass = "sf")

# Split into individual polygons and exclude Bornholm based on longitude
denmark_mainland <- denmark %>%
  st_cast("POLYGON") %>%
  mutate(centroid = st_centroid(geometry)) %>%
  filter(st_coordinates(centroid)[,1] < 13) %>%
  select(-centroid)

# Define Denmark bounding box
den_bbox <- getbb("Denmark")

# Download highway
denmark_roads_raw <- opq(bbox = den_bbox) %>%
  add_osm_feature(key = "highway", value = "motorway") %>%
  osmdata_sf() %>%
  .$osm_lines

# Reproject Denmark polygon to match roads CRS
denmark_mainland <- st_transform(denmark_mainland, st_crs(denmark_roads_raw))

# Clip roads to the Denmark polygon
denmark_roads <- st_intersection(denmark_roads_raw, denmark_mainland)

denmark_roads <- st_transform(denmark_roads, crs = 4326)

# Brown tourist signs found (coordinates added by me) in https://www.vejdirektoratet.dk/side/turistoplysningstavler-paa-danske-motorveje
brown_signs <- read_excel("brown_signs.xlsx")

brown_signs <- brown_signs %>%
  arrange(desc(Actual_Latitude))

brown_signs$distance_km <- NA_real_
brown_signs$duration_min <- NA_real_

# Loop through all rows
for (i in seq_len(nrow(brown_signs))) {
  message(paste("Processing:", brown_signs$Attraction[i]))
  
  attraction_count <- i
  visited_count <- sum(brown_signs$Visited[1:i])
  
  actual_coords <- c(brown_signs$Actual_Longitude[i], brown_signs$Actual_Latitude[i])
  sign_coords   <- c(brown_signs$Sign_Longitude[i], brown_signs$Sign_Latitude[i])
  
  route_raw <- tryCatch({
    ors_directions(
      coordinates = list(actual_coords, sign_coords),
      profile = "driving-car",
      format = "geojson"
    )
  }, error = function(e) {
    message(paste("Error for", brown_signs$Attraction[i], ":", e$message))
    return(NULL)
  })
  
  if (is.null(route_raw)) next
  
  # Convert to sf
  route_sf <- tryCatch({
    st_read(toJSON(route_raw, auto_unbox = TRUE), quiet = TRUE)
  }, error = function(e) {
    message("Failed to parse route as sf.")
    return(NULL)
  })
  
  if (is.null(route_sf)) next
  
  # Extract distance + duration
  distance_m <- route_raw$features[[1]]$properties$summary$distance
  duration_s <- route_raw$features[[1]]$properties$summary$duration
  
  brown_signs$distance_km[i] <- round(distance_m / 1000, 2)
  brown_signs$duration_min[i] <- round(duration_s / 60, 1)
  
  # Create label text
  label_text_left <- paste0(
    brown_signs$Attraction[i], "\n",
    "from brown road sign\n",
    "Distance: ", round(distance_m / 1000, 2), " km\n",
    "Duration: ", round(duration_s / 60, 1), " min"
  )
  
  label_text_right <- paste0(
    "Visited / Attraction\n",
    visited_count, " / ", attraction_count
  )
  
  # Left text (top-left)
  text_point_left <- st_sf(
    geometry = st_sfc(st_point(c(bbox["xmin"] + 1, bbox["ymax"] - 0.2))),
    crs = st_crs(denmark_mainland),
    label = label_text_left
  )
  
  # Right text (top-right)
  text_point_right <- st_sf(
    geometry = st_sfc(st_point(c(bbox["xmax"] - 0.5, bbox["ymax"] - 0.2))),
    crs = st_crs(denmark_mainland),
    label = label_text_right
  )
  
  # Create route endpoints
  points_sf <- st_as_sf(data.frame(
    name = c("Actual Location", "Sign Location"),
    color = c("red", "#660000"), 
    lon = c(actual_coords[1], sign_coords[1]),
    lat = c(actual_coords[2], sign_coords[2])
  ), coords = c("lon", "lat"), crs = 4326)
  
  # Build map
  brown_sign_map <- tm_shape(denmark_mainland) +
    tm_polygons(fill = "gray90", border.col = "gray60") +
    tm_shape(denmark_roads) +
    tm_lines(col = "gray40", lwd = 0.5) +
    tm_shape(route_sf) +
    tm_lines(col = "blue", lwd = 2) +
    tm_shape(points_sf) +
    tm_symbols(col = "color", size = 0.2) +
    tm_shape(text_point_left) +
    tm_text("label", size = 0.7, just = c("left", "top")) +
    tm_shape(text_point_right) +
    tm_text("label", size = 0.7, just = c("left", "top")) 
  
  # Save map per attraction
  filename <- paste0("route_", i, ".png")
  tmap_save(brown_sign_map, filename = filename, width = 10, height = 8, units = "in", dpi = 300)
}

# using the animation method found here : https://stackoverflow.com/a/73376411/10710995
filenames <- list.files(
  pattern = "^route_.*\\.png$",
  full.names = FALSE
)

# Extract the number after 'route_' and before '.png'
numbers <- as.numeric(gsub("route_(\\d+)\\.png", "\\1", filenames))

# Order filenames by extracted numeric part
filenames_sorted <- filenames[order(numbers)]

# create animation
av::av_encode_video(filenames_sorted, framerate = 1,
                    output = "brown_signs_denmark.mp4")
