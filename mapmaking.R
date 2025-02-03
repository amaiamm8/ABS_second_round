install.packages("webshot")
install.packages("leaflet")
library(leaflet)
library(dplyr)
webshot::install_phantomjs(force = TRUE)

coordinates <- read.csv("site_data_Amaia.csv")

# Filter based on conditions (e.g., names containing "Place 1")
filtered_coordinates <- coordinates %>%
  filter(Site %in% c(7, 8 , 10, 12 , 26, 34))


map <- leaflet(filtered_coordinates, options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addProviderTiles(providers$OpenStreetMap, 
                   options = providerTileOptions(opacity = 0.3)) %>%
  addCircleMarkers(
    lng = ~Longitude,
    lat = ~Latitude,
    popup = ~Site,
    label = ~Site,
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = "top",
      textsize = "10px",    # Smaller text
      style = list(
        "color" = "#4a4a4a", # Darker grey text
        "font-family" = "Arial",
        "font-style" = "italic",
        "background-color" = "rgba(255, 255, 255, 0.7)", # Semi-transparent white background
        "border-color" = "rgba(0, 0, 0, 0.2)",
        "border-radius" = "3px",
        "padding" = "2px 4px"
      )
    ),
    radius = 3,            # Smaller circles
    fillColor = "#e00b0b", # Grey fill
    color = "#823939",    # Darker grey border
    fillOpacity = 0.5,    # More transparent
    weight = 1            # Thinner border
  ) %>%
  addScaleBar(position = "topright") %>%
  addMiniMap(height=300, width = 300 )
map


# Option 2: Use webshot directly
library(webshot)
htmlwidgets::saveWidget(map, "temp.html", selfcontained = FALSE)

webshot(url = "temp.html", 
        file = "my_map.pdf",
        delay = 5,
        vwidth = 800,
        vheight = 800)

