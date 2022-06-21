# Forestry Tree Points and Planting Spaces


## Load Libraries -----------------------------------------------

source("code/00_load_dependencies.R")

### Tree Points
tp <- vroom::vroom('https://data.cityofnewyork.us/resource/hn5i-inap.csv?$limit=9999999999&$select=globalid,plantingspaceglobalid,createddate,planteddate,riskrating,riskratingdate,location') 

### Planting Spaces
PS <- vroom::vroom('https://data.cityofnewyork.us/resource/82zj-84is.geojson?$limit=9999999') 


### Block Planting
bp <- read.socrata("https://data.cityofnewyork.us/resource/h426-x5gi.json?$limit=9999999999")

# need to be joined with CSCL datasets
# quetion which areas of NYC have been surveyed and recommended for planting




#### Work Orders

wo <- vroom::vroom("https://data.cityofnewyork.us/resource/bdjm-n7q4.csv?$limit=99999999&$where=wocategory=%27Tree%20Planting%27")

# remove columns

wo <- wo %>% select(-c(buildingnumber, streetname, locationdetails,
                       wocontract, sanitationassigneddate, sanitationremovaldate,
                       sanitationupdateddate, sanitationzone, wowoodremains,
                       woequipment, statesenate, stateassembly,congressional, 
                       crossstreet1, crossstreet2, treepointglobalid,
                       plantingspaceglobalid, sidewalkdamage, wowireconflict,
                       crewglobalid, geometry, location )) 



#### inspections
insp <- vroom::vroom("https://data.cityofnewyork.us/resource/4pt5-3vv4.csv?$limit=99999999")

insp1 <- insp %>%  select(inspectiontype, treepointglobalid, plantingspaceglobalid, globalid,
                          inspectiondate, reinspectiondate, parentinspectionglobalid, 
                          swtotaltrees, location)

## join inspections to work order to get tree pt & planting space global ids

wo_insp <- wo %>% 
  left_join(insp1, by = c("inspectionglobalid"  = "globalid") ) %>% 
  left_join(tp, by = c("treepointglobalid"  = "globalid") )
  
# focus on tree-planting work orders
table(wo_insp$wotype)
# Tree Plant-Park Tree 
# 8579 
# Tree Plant-Street Tree 
# 87485 
# Tree Plant-Street Tree Block 
# 20174 
# Tree Removal for Tree Planting 
# 3615

wo_clean <- wo_insp %>% 
  filter(!wotype %in% (c('Misc Work', 'Prune-Traffic 20 Day', 'Stump Removal', 
                       'Tree and Sidewalk Repair', 'Tree Down', 'Tree Removal',
                       'Stump Removal for Tree Planting','Tree Removal for Tree Planting'))) %>%
  mutate(actualfinishdate = lubridate::mdy(strtrim(actualfinishdate,10)),
         planteddate = as.Date(planteddate),
         createddate.x = as.Date(createddate.x),
         finished_or_planted_date = case_when(is.na(actualfinishdate) & 
                                                !is.na(planteddate) ~ planteddate,
                                              !is.na(treepointglobalid) ~ createddate.x,
                                              TRUE ~ actualfinishdate)) %>% 
  filter(wostatus!='Cancel')
  

# how many of those have a planted date or actual finish date?
# check for dups


wo_chart <- wo_clean %>% 
  group_by(#month_yr = cut(actualfinishdate, breaks = "1 month"), 
           yr = cut(actualfinishdate, breaks = "1 year"),
           wotype) %>% 
  summarise(count = n()) %>% 
  arrange(#month_yr,
          yr) %>% 
  mutate(wotype=as.factor(wotype),
         yr = as.Date(yr))
             

wo_chart %>% 
ggplot(aes(x=yr, y=count, color=wotype, group=wotype)) +
  geom_point(size=0.5) + geom_line() +
  scale_x_date(date_labels = "%Y", breaks = "1 year") +
  hrbrthemes::theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ggtitle("Citywide Race/Ethnicity") + xlab("Percent Change") + ylab("Relative Difference")
  


