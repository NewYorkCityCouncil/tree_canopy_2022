# Forestry Tree Points and Planting Spaces


## Load Libraries -----------------------------------------------

source("code/00_load_dependencies.R")

### Tree Points
TP <- st_read('https://data.cityofnewyork.us/resource/hn5i-inap.geojson?$limit=9999999999') %>%
  st_transform("+proj=longlat +datum=WGS84")
TP_df <- TP %>% st_drop_geometry()

### Planting Spaces
PS <- st_read('https://data.cityofnewyork.us/resource/82zj-84is.geojson?$limit=9999999999') %>%
  st_transform("+proj=longlat +datum=WGS84")
PS_df <- PS %>% st_drop_geometry()


### Block Planting
bp <- read.socrata("https://data.cityofnewyork.us/resource/h426-x5gi.json?$limit=9999999999")

# need to be joined with CSCL datasets
# quetion which areas of NYC have been surveyed and recommended for planting




#### Work Orders

wo <- read.socrata("https://data.cityofnewyork.us/resource/bdjm-n7q4.json?$limit=99999999&$where=wotype%20in%20(%27Tree%20Plant-Park%20Tree%27,%27Tree%20Plant-Street%20Tree%27,%27Tree%20Plant-Street%20Tree%20Block%27)")

wo_clean <- wo %>% 
  mutate(actualfinishdate = lubridate::mdy(strtrim(actualfinishdate,10))) %>% 
  group_by(month_yr = cut(actualfinishdate, breaks = "1 month"), 
           wotype) %>% 
  summarise(count = n()) %>% 
  arrange(month_yr) %>% 
  mutate(wotype=as.factor(wotype),
         month_yr = as.Date(month_yr)) %>% 
  filter(month_yr <= Sys.Date())
             

wo_clean %>% 
ggplot(aes(x=month_yr, y=count, color=wotype, group=wotype)) +
  geom_point(size=0.5) + geom_line() +
  scale_x_date(date_labels = "%Y-%m", breaks = "3 months") +
  hrbrthemes::theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ggtitle("Citywide Race/Ethnicity") + xlab("Percent Change") + ylab("Relative Difference")
  


