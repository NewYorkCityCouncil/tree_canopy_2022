# load files from resiliency repo


kde_heat_crop <- raster(file_dl("https://github.com/NewYorkCityCouncil/resiliency/raw/master/data/output/kde_heatmap_cropped.tif")) # cropped to NYC boundaries

median_temp_sf <- read_sf(unzip_sf("https://github.com/NewYorkCityCouncil/resiliency/raw/master/data/output/median_satellite_surface_temperatures.zip")) # read zip version or else it won't work  - shapefile

median_temp_sp <- as(median_temp_sf, "Spatial") # set as spatial points dataframe

# read in boro boundaries
boro <- read_sf("https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=GeoJSON") %>% 
  st_transform("+proj=longlat +datum=WGS84") %>% 
  st_simplify(dTolerance = .00001) # less precise, loads faster

## ------------------- deciding not to do park acreage by nta -------------------
# # read nta park park acreage file
# 
# corr_df <- read_csv('data/input/modified/corr_df.csv')
# 
# # park_access outline
# recode <- corr_df %>% 
#   mutate(bottom25 = ifelse(park_perc > as.numeric(summary(corr_df$park_perc)[5]), 1, 0))


# # join to nta shapefile
# nta_shp <- st_read('https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Neighborhood_Tabulation_Areas_2010/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson') %>%
#   st_transform("+proj=longlat +datum=WGS84")  %>%  left_join(recode, by ="NTACode")

# Palettes & Values -----------------------------------------------------------


# pal <- c('#002a93', '#1845b4', '#3060d6', '#507ded', '#779bf5', '#9db9fd', '#c8d7fb', '#E6E6E6', '#facdbe', '#ffa184', '#e97e64', '#d25a44', '#b43b2b', '#921e16', '#6e0000')

pal<- c('#1d5fd6', '#3c75db', '#5c8be0', '#7ba1e5', '#9ab6e9', '#b9ccee', '#d9e2f3', '#f8f8f8', '#edd7d4', '#e5c4c1', '#ddb2ad', '#d19790', '#c17369', '#b24e42', '#a22a1b')

heat_pal <- colorNumeric(rev(pal), 
                         domain = values(kde_heat_crop),
                         na.color = "transparent")

legend_val <- seq(min(median_temp_sp$zscore), 
                  max(median_temp_sp$zscore), by = 1)

length(legend_val) # create legend numbers

legend_pal <- colorNumeric(colorRamp(pal, interpolate = "spline"),      domain = legend_val)


# # Park Access files ----------------------------
# 
# read in from https://github.com/NewYorkCityCouncil/park_equity_covid_2022/

modzcta_facre <- st_read("https://raw.githubusercontent.com/NewYorkCityCouncil/park_equity_covid_2022/main/data/processed/modzcta_facre.geojson")

# park_access outline
recode <- modzcta_facre %>% 
  mutate(bottom25 = ifelse(facre_pc * 100000 < 27.5, 1, 0))



# source control
rr <- HTML('<small> Source: NYC Parks, US Geological Survey’s Landsat 8 Satellite </small>')

# park_accesslegend  
park_access<- HTML('<div style="color: #23417D;"> Zipcode Park Access (MZCTA) <br> <strong>Bottom 25%</strong> </div>')

# Mapping -----------------------------------------------------------------

map <- 
  leaflet(options = leafletOptions(zoomControl = FALSE, 
                                   minZoom = 11, 
                                   maxZoom = 16)) %>%
  setView(-73.984865,40.710542,11) %>%
  setMapWidgetStyle(list(background= "white")) %>% 
  addRasterImage(kde_heat_crop, colors = heat_pal, opacity = 0.6) %>% 
  # addPolygons(data = nta_shp, weight = 1.3, fill=F, opacity = 1,
  #             color = "#23417D", stroke = nta_shp$bottom25) %>% 
  addPolygons(data = recode, weight = 1.2, fill=F, opacity = 1,
              color = "#23417D", stroke = recode$bottom25) %>% 
  addPolygons(data=boro, stroke = T, fill=F, 
              color = "#666666", weight = 1) %>% 
  addLegend(position = "topleft", 
            pal = legend_pal, values = legend_val, 
            title = paste0("Surface Temperature Deviation from Mean", 
                           "<br>", "(Daytime Summer, 93.5°)"),
            labFormat = labelFormat(prefix = " ",
                                    suffix = "°"))  %>% 
  addControl(rr, position = "bottomright") %>% 
  addControl(park_access, position = "topleft") 



map


mapshot(map, file = "figures/median_income.png", 
        vwidth = 900, vheight = 870)

saveWidget(map, file = "figures/median_income.html")
