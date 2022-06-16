# Forestry Tree Points and Planting Spaces


## Load Libraries -----------------------------------------------

source("code/00_load_dependencies.R")

## Read and Join Data -----------------------------------------------

### Work Orders
WO <- st_read('https://data.cityofnewyork.us/resource/bdjm-n7q4.geojson?$limit=9999999999') %>%
  st_transform("+proj=longlat +datum=WGS84") %>%
  mutate(
    year = substr(createddate, 1,4), 
    month = substr(createddate, 6,7)
  )

WO_df <- WO %>% st_drop_geometry() 

### NTA Shapefile from NYC Planning 
# https://www1.nyc.gov/site/planning/data-maps/open-data/census-download-metadata.page
nta_shp <- st_read('https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Neighborhood_Tabulation_Areas_2010/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson') %>%
  st_transform("+proj=longlat +datum=WGS84") 

### Canopy data from Nature Conservancy Paper
# https://zenodo.org/record/5210261#.Yqt6VOzMKHF
nta_canopy <- read.csv("data/input/raw/canopy_streettree_summaries/canopystreettree_supp_nta.csv", header=TRUE)

### Join nta shapefile and canopy cover data
nta_canopy_shp <- nta_shp %>%
  left_join(nta_canopy, by = c("NTACode" = "ntacode"))

## Tree Plantings Time Series -----------------------------------------------

### Monthly
WO_monthly_plants <- WO_df %>%
  filter(wocategory == "Tree Planting") %>%
  group_by(month, year) %>%
  summarise(monthly_total = n()) %>%
  mutate(year_month = paste0(year, "_", month))

WO_monthly_plants %>%
  filter(year >= 2016) %>%
  ggplot(aes(x = month, y = monthly_total, group = year, color = year)) + geom_line()

### Yearly 
WO_yearly_plants <- WO_df %>%
  filter(wocategory == "Tree Planting") %>%
  group_by(year) %>%
  summarise(yearly_total = n()) 

WO_yearly_plants %>%
  filter(year >= 2016) %>%
  ggplot(aes(x = year, y = yearly_total, group = 1, color = year)) + geom_line()

## Tree Plantings Spatial -----------------------------------------------

### Tree plantings work orders 2016 and on
WO_treeplantings <- WO %>%
  filter(wocategory == "Tree Planting") %>%
  filter(year >=2016)

### Identify NTA for each tree planting work order after 2015
nta_plantings <- st_join(nta_shp, WO_treeplantings) %>%
  group_by(NTACode, NTAName) %>%
  summarise(nta_count = n()) 

## Map

quantile(nta_plantings$nta_count, seq(0,1,0.05), na.rm = TRUE)
#~0%, 25%, 50%, 75%, 90%, 100%

pal_nta <- colorBin(
  palette = c('#e6dfd3', '#cfbea5', '#b79e7a', '#9e7f4f', '#846126'),
  bins = c(0,250,500,700,900,2000),
  domain = nta_plantings$nta_count,
  na.color = "transparent")

map_nta_plantings <- leaflet() %>%
  setView(-73.941281,40.704103, zoom=11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=nta_plantings,
              weight = 1,
              fillColor = ~pal_nta(nta_count),
              color="#E6E6E6",
              fillOpacity = 0.9, 
              popup = lapply(paste("<p>","NTA: ",nta_plantings$NTAName,"</p>", 
                                   "<p>",paste0("Count: ",nta_plantings$nta_count),"</p>"), HTML)) %>%
  addLegend("bottomright", pal = pal_nta, values = nta_plantings$nta_count,
            title = "Number of Tree Plantings",
            opacity = 1)

