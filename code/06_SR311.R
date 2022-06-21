# Parks Related 311 calls


## Load Libraries -----------------------------------------------

source("code/00_load_dependencies.R")

## Read Data -----------------------------------------------

# 311 Service Requests
# https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2010-to-Present/erm2-nwe9
# From 01-01-2018 to present for all complaints handled by Parks Department
raw <- read_csv("https://data.cityofnewyork.us/resource/erm2-nwe9.csv?agency=DPR&$where=created_date>='2018-01-01T00:00:00.000'&$limit=999999999999")

# top Parks complaint types
top_complaints <- raw %>%
  group_by(complaint_type) %>%
  summarise(number_of_complaints = n()) %>%
  arrange(desc(number_of_complaints))

