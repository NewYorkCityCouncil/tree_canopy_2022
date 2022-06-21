# NTA Correlation Plots


## Load Libraries -----------------------------------------------

source("code/00_load_dependencies.R")
sf_use_s2(FALSE)

## Read and Join Data -----------------------------------------------

### Census data crosswalk
# cross walk from census tract to NTA
# download.file("https://www1.nyc.gov/assets/planning/download/office/planning-level/nyc-population/census2010/nyc2010census_tabulation_equiv.xlsx", destfile = "data/input/nyc_ct_nta_crosswalk.xlsx")
cross_ct_nta <- read_xlsx("data/input/raw/nyc_ct_nta_crosswalk.xlsx", sheet = "NTA to 2010 CT equivalency", skip = 2) %>%
  clean_names() %>%
  slice(2:n()) %>%
  rename(
    Borough = borough,
    NTACode = neighborhood_tabulation_area_nta, 
    NTAName = x7, 
    CountyFIPS = x2010_census_bureau_fips_county_code, 
    BoroCode = x2010_nyc_borough_code, 
    CensusTract = x2010_census_tract
  ) %>%
  mutate(GEO_ID = paste0("1400000US", "36", CountyFIPS, CensusTract))

### Population data to pull from ACS
# https://api.census.gov/data/2019/acs/acs5/subject/variables.html
# Income data at census tract level
census_pop <- getCensus(
  name = "acs/acs5/subject",
  # 2019 since NTAs are 2010 because canopy data is 2017
  vintage = 2019,
  vars = c('NAME', 'GEO_ID', 'S0101_C01_001E'), 
  region = "tract:*", 
  regionin = "state:36+county:005,047,081,085,061")

nta_pop <- cross_ct_nta %>%
  left_join(census_pop, by="GEO_ID") %>%
  group_by(NTACode) %>%
  summarise(nta_pop = sum(S0101_C01_001E, na.rm=TRUE))

### NTA Shapefile from NYC Planning 
# https://www1.nyc.gov/site/planning/data-maps/open-data/census-download-metadata.page
nta_shp <- st_read('https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Neighborhood_Tabulation_Areas_2010/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson') %>%
  st_transform("+proj=longlat +datum=WGS84") 

### Parks Properties 
# https://data.cityofnewyork.us/Recreation/Parks-Properties/enfh-gkve
parks_prop <- read_sf("https://data.cityofnewyork.us/api/geospatial/enfh-gkve?method=export&format=GeoJSON") %>%
  st_transform("+proj=longlat +datum=WGS84") 

nta_parks <- st_intersection(nta_shp, parks_prop) %>% 
  # units is [m^2]
  mutate(intersect_area = as.numeric(round(st_area(.), 0))) %>%
  st_drop_geometry() %>%
  group_by(NTACode, NTAName) %>%
  summarise(park_area = sum(intersect_area)) %>%
  # convert from square meters to acres
  mutate(park_area = park_area / 4047 )

### Temperature data
# https://a816-dohbesp.nyc.gov/IndicatorPublic/VisualizationData.aspx?id=2141,719b87,107,Summarize 
temp <- read.csv("data/input/raw/Daytime Summer Surface Temperature.csv", header = TRUE, skip = 6) %>%
  filter(GeoTypeName == "Neighborhood (NTA)") %>%
  mutate(Geography = sub(" ", "", Geography))

### Canopy data from Nature Conservancy
canopy <- read.csv("data/input/raw/equity_data/equity_data_supp_nta.csv", header = TRUE)

corr_df <- nta_pop %>%
  left_join(nta_parks, by = "NTACode") %>%
  # Note: NTAs that are mostly parks / airports are removed
  left_join(canopy, by = c("NTACode" = "ntacode")) %>%
  left_join(temp, by = c("ntaname" = "Geography")) %>%
  # double check these are the NTAs in CD25
  mutate(
    park_perc = park_area / unit_area_acres, 
    rank_park_perc = rank(park_perc),
    CM25 = as.factor(ifelse(ntaname == "Jackson Heights" | ntaname == "Elmhurst", 1, 0))
    )

## Correlation Plots -----------------------------------------------

# Canopy Cover and Heat Plot (no CM, no park acreage)
plot <- 
  ggplot(data = corr_df, aes(x = canopy_2017_pct, y = Degrees.Fahrenheit)) + 
  geom_point(color = "#2F56A6") + 
  geom_smooth(aes(x = canopy_2017_pct, y = Degrees.Fahrenheit, color = "lm"), method='lm', formula= y~x, se = FALSE) +
  scale_color_manual(values=c("#222222")) +
  ggtitle("Canopy Cover and Heat in NYC", "Comparing Percentage of Canopy Coverage and Daytime Summer Surface Temperature for Every Neighborhood (NTA)") +
  labs(
    x = "Canopy Coverage (%)",
    y = "Average Temperature (Degrees Fahrenheit)",
    caption = expression(paste(italic("Source: NYC DOHMH: Environment & Health Data Portal; Nature Conservancy: The State of the Urban Forest in New York City")))
  ) +
  
  geom_vline(xintercept = median(corr_df$canopy_2017_pct, na.rm=TRUE),
             color ="#666666",linetype = "dashed") +
  geom_hline(yintercept = median(corr_df$Degrees.Fahrenheit, na.rm=TRUE),
             color ="#666666",linetype = "dashed") +
  
  # facet_wrap(~boroname) + 
  
  annotate("text", x = 24.5, y = 92.5, label = "Median Canopy Coverage") +
  annotate("text", x = 47, y = 98.8, label = "Median Temperature") +
  
  theme(legend.position="none", legend.text = element_text(size=8),
        legend.title = element_text(size=10, family = 'Georgia'),
#        text = element_text(family = "Open Sans"),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(family = "Georgia",size = 14),
        axis.title.y = element_text(size = 11, 
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size = 11, 
                                   margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.x = element_text(size = 11, 
                                   margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(size = 11, 
                                    margin = margin(t = 10, r = 0, b = 0, l = 0))) 

ggsave(plot, filename = "visuals/canopy_temp_plot.png", 
       units = c("in"), width= 10, height= 6)

# Canopy Coverage and Heat w/ CD25 (no park acreage)
plot <- 
  ggplot(data = corr_df, aes(x = canopy_2017_pct, y = Degrees.Fahrenheit, color = CM25)) + 
  geom_point() + 
   scale_color_manual(values=c("#2F56A6", "#800000", "#222222")) +
   geom_smooth(aes(x = canopy_2017_pct, y = Degrees.Fahrenheit, color = "lm"), method='lm', formula= y~x, se = FALSE) +
  ggtitle("Canopy Cover and Heat in NYC", "Comparing Percentage of Canopy Coverage and Daytime Summer Surface Temperature for Every Neighborhood (NTA)") +
  labs(
    x = "Canopy Coverage (%)",
    y = "Average Temperature (Degrees Fahrenheit)",
    color = "Percentage\nPark Acreage\n(Rank)",
    caption = expression(paste(italic("Source: NYC DOHMH: Environment & Health Data Portal; Nature Conservancy: The State of the Urban Forest in New York City")))
  ) +
  
  geom_vline(xintercept = median(corr_df$canopy_2017_pct, na.rm=TRUE),
             color ="#666666",linetype = "dashed") +
  geom_hline(yintercept = median(corr_df$Degrees.Fahrenheit, na.rm=TRUE),
             color ="#666666",linetype = "dashed") +
  
  # facet_wrap(~boroname) + 
  
  geom_segment(x=11, y=102, xend=15.15, yend=100.9, arrow = arrow(length = unit(0.5, "cm"))) +
  annotate("text", x = 10.8, y = 102.2, label = "Elmhurst") +
  geom_curve(x=11, y=101, xend=17, yend=99, arrow = arrow(length = unit(0.5, "cm"))) +
  annotate("text", x = 10.5, y = 101.2, label = "Jackson Heights") +
  
  # annotate("text", x = 10, y = 103.5, label = "Low Canopy Coverage\nHigh Temperature") +
  # annotate("text", x = 45, y = 103.5, label = "High Canopy Coverage\nHigh Temperature") +
  # annotate("text", x = 10, y = 93, label = "Low Canopy Coverage\nLow Temperature") +
  # annotate("text", x = 45, y = 93, label = "High Canopy Coverage\nLow Temperature") +
  
  annotate("text", x = 24.5, y = 92.5, label = "Median Canopy Coverage") +
  annotate("text", x = 47, y = 98.8, label = "Median Temperature") +
  
  theme(legend.position="none", legend.text = element_text(size=8),
        legend.title = element_text(size=10, family = 'Georgia'),
        #        text = element_text(family = "Open Sans"),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(family = "Georgia",size = 14),
        axis.title.y = element_text(size = 11, 
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size = 11, 
                                   margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.x = element_text(size = 11, 
                                   margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(size = 11, 
                                    margin = margin(t = 10, r = 0, b = 0, l = 0))) 

ggsave(plot, filename = "visuals/canopy_temp_plot_CD25.png", 
       units = c("in"), width= 10, height= 6)


# Canopy Coverage and Heat w/ Park Acreage (no CD25)
plot <- 
  ggplot(data = corr_df, aes(x = canopy_2017_pct, y = Degrees.Fahrenheit, color = rank_park_perc)) + 
  geom_point() + 
  #  scale_color_manual(values=c("#2F56A6", "#800000", "#222222")) +
  #  geom_smooth(aes(x = canopy_2017_pct, y = Degrees.Fahrenheit, color = "lm"), method='lm', formula= y~x, se = FALSE) +
  ggtitle("Canopy Cover and Heat in NYC", "Comparing Percentage of Canopy Coverage and Daytime Summer Surface Temperature for Every Neighborhood (NTA)") +
  labs(
    x = "Canopy Coverage (%)",
    y = "Average Temperature (Degrees Fahrenheit)",
    color = "Percentage\nPark Acreage\n(Rank)",
    caption = expression(paste(italic("Source: NYC DOHMH: Environment & Health Data Portal; Nature Conservancy: The State of the Urban Forest in New York City")))
  ) +
  
  geom_vline(xintercept = median(corr_df$canopy_2017_pct, na.rm=TRUE),
             color ="#666666",linetype = "dashed") +
  geom_hline(yintercept = median(corr_df$Degrees.Fahrenheit, na.rm=TRUE),
             color ="#666666",linetype = "dashed") +
  
  # facet_wrap(~boroname) + 
  
  annotate("text", x = 25, y = 92.5, label = "Median Canopy Coverage") +
  annotate("text", x = 46, y = 98.8, label = "Median Temperature") +
  
  # geom_segment(x=11, y=102, xend=15.15, yend=100.9, arrow = arrow(length = unit(0.5, "cm"))) +
  # annotate("text", x = 10.8, y = 102.2, label = "Elmhurst") +
  # geom_curve(x=11, y=101, xend=17, yend=99, arrow = arrow(length = unit(0.5, "cm"))) +
  # annotate("text", x = 10.5, y = 101.2, label = "Jackson Heights") +
  
  
  theme(legend.position="right", legend.text = element_text(size=8),
        legend.title = element_text(size=10, family = 'Georgia'),
        #        text = element_text(family = "Open Sans"),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(family = "Georgia",size = 14),
        axis.title.y = element_text(size = 11, 
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size = 11, 
                                   margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.x = element_text(size = 11, 
                                   margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(size = 11, 
                                    margin = margin(t = 10, r = 0, b = 0, l = 0))) 

ggsave(plot, filename = "visuals/canopy_temp_plot_park.png", 
       units = c("in"), width= 10, height= 6)
