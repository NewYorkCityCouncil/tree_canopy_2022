### Improving Tree Canopy in NYC
Data analysis and visuals for NYCC 6.23.22 ['Oversight - Improving the City's Tree Canopy'](https://legistar.council.nyc.gov/MeetingDetail.aspx?ID=980035&GUID=1AE8CA84-78F5-44F3-A7B9-9FDBF8D396D1) hearing.

A related webpage for this analysis can be found [on the council website](https://council.nyc.gov/data/heat/)

<br>
<br>

#### Main Takeaways
Trees provide multiple benefits for local communities such as improving air quality and mitigating the urban heat island effect.

- Neighborhoods with lower canopy coverage tend to also have higher daytime summer surface temperatures. 

![alt text](https://newyorkcitycouncil.github.io/tree_canopy_2022/visuals/canopy_temp_plot.png)

<br>
<br>

- Residents of Maspeth/Linden Hill, East Elmhurst, and South Ozone Park/Richmond Hill in Queens, Borough Park in Brooklyn, and Mariner's Harbor/Arlington in Staten Island and Unionport in the Bronx experience high heat in the summer without adequate access to parks to cool down. 

![alt text](https://newyorkcitycouncil.github.io/tree_canopy_2022/visuals/temp_park_access_map.png)

<br>
<br>

***  

#### Data Sources 

- [Tree Canopy](https://zenodo.org/record/5210261#.Yqjlt9PMJTY)

- [Park Access](https://github.com/NewYorkCityCouncil/park_equity_covid_2022)

- [Surface Temperature](https://github.com/NewYorkCityCouncil/resiliency)

- 2019 5-Year ACS Survey: *We used R package censusapi to get demographic data

<br>
<br>

#### Methodology 

Methodologies can be referenced in the links for each of the data sources above. 

<br>
<br>

#### Scripts

- [00_load_depencies.R] (https://github.com/NewYorkCityCouncil/tree_canopy_2022/blob/main/code/00_load_dependencies.R): Run this script first to load the necessary packages and functions. 

- [05_nta_equity_correlations.R] (https://github.com/NewYorkCityCouncil/tree_canopy_2022/blob/main/code/05_nta_equity_correlations.R): This script create the correlation plot between canopy coverage and surface temperature in a neighborhood. 

- [07_SurfaceTemp_ParkAccess_Map.R] (https://github.com/NewYorkCityCouncil/tree_canopy_2022/blob/main/code/07_SurfaceTemp_ParkAccess_Map.R): This script creates the surface temperature and park access map. 

- Other scripts are not used for final images. 
