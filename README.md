# Identifying Transportation Inequities in the Boston Region
This repository contains data and mehtodology to support a UPWP project analyzing existing inequities within the Boston Region MPO.
## Purpose
The purpose of this study is to develop a baseline assessment of existing transportation inequities in the Boston region. While the equity policies applied by the MPO and other transportation agencies often take the status quo as a given and attempt to prevent making inequities worse through future investments, this study will take a historical perspective and attempt to identify existing inequities that have been caused by past decisions and identify opportunities for the MPO to reduce the divergent outcomes between population groups.

### Analysis Structure
* Demogrpahic Data Preparation
* Destination Data Preparation
* Conveyal Access Analysis
* Processing Conveyal Outputs
* Interactive App

## Demographic Data Preparation
This study pulls demographic data from the 2016-2020 ACS Census and 2020 Decennial Census to report on income status, minority status, and avaialable household vehicles.
* A person is classified as **low-income** if their household income is less than 200% of the federal poverty level.
* A person is classified as **minority** if they are a race other than White or as Hispanic  or Latino/Latina.
* **Zero-vehicle households** are those that indicate that there are zero operating vehicles available for the household.
Demographic data is pulled by census tract for the whole population and just for the adult population (18 years or older) within the Boston Region MPO service area.

### Pulling Demographic Data
Population proportions are pulled from aggregated ACS data then mapped onto decennial population counts here: [`/analysis/DemoDataPrep_Pull_Census.R`](../blob/main/analysis/DemoDataPrep_Pull_Census.R) 
This script uses TidyCensus calls from functions that are here: [`/functions/census_demo_pull.R`](../blob/main/functions/census_demo_pull.R)
### Dasymetric Population Mapping
To map census demographic data onto conveyal raster outputs, we create a gridded version of the census demographic data that will match the grid used in Conveyal analyses. This process will follow binary dasymetric interpolation as shown [here](https://pysal.org/tobler/notebooks/binary_dasymetric.html) and happens in a python notebook here: [`/notebooks/Dasymetric Population Map.ipynb`]([../blob/main/notebooks/Dasymetric%20Population%20Map.ipynb).
To map the census tract level demographic onto the raster grid, we will allocate population density within a tract based on two filters: residential road and land cover classification. This process starts by pulling road centerlines and buffering the shapes. Then, within the road buffer we find the areas that have a land classification of at least 5% of a 30 meter square area as impervious surface. Then, after applying the daymetric filters, we interpolate the area so that the population density within a tract is assigned to the grid cells that match the conveyal outputs.


## Destination Data Preparation
This project studies access to various types of destinations throughout the MPO region. The destination data was prepared as follows:
* **Higher Education Opportunities**:
* **Healthcare Opportunities**: 

    * **Emergency Healthcare**:   
    * **Nonemergency Healthcare**:   

* **Essentail Places**:
* **Parks and Open Space**:

    * **Open Space**:   
    * **Open Space, Paths**:   
    * **Open Space, Conservation**:   
