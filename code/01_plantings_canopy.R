# Forestry Tree Points and Planting Spaces


## Load Libraries -----------------------------------------------

source("code/00_load_dependencies.R")

## Read and Join Data -----------------------------------------------

### Tree Points
TP <- st_read('https://data.cityofnewyork.us/resource/hn5i-inap.geojson?$limit=9999999999') %>%
  st_transform("+proj=longlat +datum=WGS84")
TP_df <- TP %>% st_drop_geometry()

### Planting Spaces
PS <- st_read('https://data.cityofnewyork.us/resource/82zj-84is.geojson?$limit=9999999999') %>%
  st_transform("+proj=longlat +datum=WGS84")
PS_df <- PS %>% st_drop_geometry()

nta_canopy <- read.csv("data/input/raw/canopy_streettree_summaries/canopystreettree_supp_nta.csv", header=TRUE)

### Join Data
PS_TP <- PS_df %>%
  left_join(TP_df, by= c("globalid" = "plantingspaceglobalid"))

## Data Quality Checks -----------------------------------------------

# globalid's appear as many as 6 times in the joined dataset; one planting space associated with multiple tree points
PS_TP %>%     
  group_by(globalid) %>%    
  summarise(freq = n()) %>%    
  arrange(desc(freq)) %>%
  head()

# example: same planting space associated with 6 tree points
PS_TP %>%
  filter(globalid == "038561B8-7CD6-4CEC-B820-41C01CD4D056")

# 37,807 planting spaces have no associated tree point
summary(is.na(PS_TP$objectid.y))
# All but 15 are marked as empty (most are empty) or retired
summary(as.factor((PS_TP %>% filter(is.na(PS_TP$objectid.y)))$psstatus))

# planted date is NA for 98% of tree points
summary(is.na(TP$planteddate))

# planted date can come befoe created date, i.e.e created date is for the date of the record, not the planting of the tree
PS_TP %>%
  filter(globalid == "3A9970D3-BBA5-49C8-B05C-5BEECB77E31D")

### summary: some planting spaces do not have an associated tree point (planting space status generally marked as empty)
### some planting spaces are associated with multiple tree points (but only one tree point can be active per space according to data dictionary)

## Planting Space History -----------------------------------------------

PS_history <- PS_TP %>%
  group_by(globalid) %>%
  arrange(desc(createddate.y)) %>%
  summarise(
    recent_created = first(createddate.y), 
    recent_tpstructure = first(tpstructure), 
    recent_tpcondition = first(tpcondition), 
    recent_update = first(updateddate.y),
    
    previous_created = nth(createddate.y, 2), 
    previous_tpstructure = nth(tpstructure, 2), 
    previous_tpcondition = nth(tpcondition, 2), 
    previous_update = nth(updateddate.y, 2)
    )


