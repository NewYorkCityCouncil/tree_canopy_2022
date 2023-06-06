source("code/00_load_dependencies.R")

########################################################################################
# Created by: Anne Driscoll
# Last edited on: 6/6/2023
#
# This file creates the map of canopy coverage
########################################################################################



################################################################################
# read in data 
################################################################################

canopy_coverage = read_csv(file.path("data", "input", "raw", "equity_data", 
                                     "equity_data_supp_nta.csv")) %>%
  select(ntacode, canopy_2017_pct, netcanopychange_percent)

nta = st_read("https://data.cityofnewyork.us/api/geospatial/d3qk-pfyz?method=export&format=GeoJSON") %>%
  st_transform(st_crs(4326))

ej_areas = st_read("https://data.cityofnewyork.us/api/geospatial/ykru-djh7?method=export&format=GeoJSON") %>%
  filter(ejdesignat %in% c("EJ Area", "Potential EJ Area")) %>%
  st_union()

council_districts = councildown:::dists %>%
  st_transform(st_crs(4326))


################################################################################
# finish assembling data
################################################################################

nta = nta %>%
  merge(by = "ntacode", all.x = T, canopy_coverage) %>%
  mutate(label = paste0("<strong>Neighborhood Name</strong>: ", ntaname, "<br>",
                        "<strong>2017 canopy coverage:</strong> ", canopy_2017_pct, "<br>", 
                        "<strong>% change in canopy (2010-2017):</strong> ",netcanopychange_percent)) 


breaks = classInt::classIntervals(nta$canopy_2017_pct, n = 5, style = 'jenks')$brks

pal = colorBin(
  palette = rev(colorRampPalette(c("darkgreen", "#E1EFE4"))(5)), 
  bins = round(breaks),
  domain = nta$canopy_2017_pct,
  na.color = "white",
)

################################################################################
# plot city canopy coverage
################################################################################

map = leaflet::leaflet(options = leafletOptions(attributionControl=FALSE, 
                                                zoomControl = FALSE, 
                                                minZoom = 10, 
                                                maxZoom = 15)) %>%
  addPolygons(data = nta, fillColor = ~pal(canopy_2017_pct), weight = 0.5, 
              color = ~pal(canopy_2017_pct), fillOpacity = 1, 
              popup = ~label) %>%
  addCouncilStyle(add_dists = TRUE) %>%
  addLegend_decreasing(position = "topleft", pal = pal, 
                       title = paste0("% area that is tree canopy"),  
                       values = c(0, 1), opacity = 1, decreasing = T, 
                       labFormat = labelFormat(suffix = "%"), 
                       na.label = "NA")

saveWidget(map, file=file.path('visuals', 
                               "NTA_canopy_cover.html"))

plot(st_geometry(nta))
plot(add=T, ej_areas, border="red")
