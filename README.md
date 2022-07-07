# Identifying Transportation Inequities in the Boston Region
This repository contains data and methodology to support a UPWP project analyzing existing inequities within the Boston Region MPO.
## Purpose
The purpose of this study is to develop a baseline assessment of existing transportation inequities in the Boston region. While the equity policies applied by the MPO and other transportation agencies often take the status quo as a given and attempt to prevent making inequities worse through future investments, this study will take a historical perspective and attempt to identify existing inequities that have been caused by past decisions and identify opportunities for the MPO to reduce the divergent outcomes between population groups.

### Analysis Structure
* Demographic Data Preparation
* Destination Data Preparation
* Conveyal Access Analysis
* Processing Conveyal Outputs
* Interactive App

## Demographic Data Preparation
This study pulls demographic data from the 2016-2020 ACS Census and 2020 Decennial Census to report on income status, minority status, and available household vehicles.
* A person is classified as **low-income** if their household income is less than 200% of the federal poverty level.
* A person is classified as **minority** if they are a race other than White or as Hispanic  or Latino/Latina.
* **Zero-vehicle households** are those that indicate that there are zero operating vehicles available for the household.

Demographic data is pulled by census tract for the whole population and just for the adult population (18 years or older) within the Boston Region MPO service area.

### Pulling Demographic Data
Population proportions are pulled from aggregated ACS data then mapped onto decennial population counts here: [`/analysis/DemoDataPrep_Pull_Census.R`](https://github.com/CTPSSTAFF/existing-inequities/blob/main/analysis/DemoDataPrep_Pull_Census.R) 
This script uses TidyCensus calls from functions that are here: [`/functions/census_demo_pull.R`](https://github.com/CTPSSTAFF/existing-inequities/blob/main/functions/census_demo_pull.R)
### Dasymetric Population Mapping
To map census demographic data onto Conveyal raster outputs, we create a gridded version of the census demographic data that will match the grid used in Conveyal analyses. This process will follow binary dasymetric interpolation as shown [here](https://pysal.org/tobler/notebooks/binary_dasymetric.html) and happens in a python notebook here: [`/notebooks/Dasymetric Population Map.ipynb`](https://github.com/CTPSSTAFF/existing-inequities/blob/main/notebooks/Dasymetric%20Population%20Map.ipynb).
To map the census tract level demographic onto the raster grid, we will allocate population density within a tract based on two filters: residential road and land cover classification. This process starts by pulling road centerlines and buffering the shapes. Then, within the road buffer we find the areas that have a land classification of at least 5% of a 30 meter square area as impervious surface. Then, after applying the dasymetric filters, we interpolate the area so that the population density within a tract is assigned to the grid cells that match the Conveyal outputs.


## Destination Data Preparation
This project studies access to various types of destinations throughout the MPO region. For data that required geocoding, see [`/geocoding/`](https://github.com/CTPSSTAFF/existing-inequities/tree/main/geocoding) for notes on the process. Destination outputs are saved here: [`/output/`](https://github.com/CTPSSTAFF/existing-inequities/tree/main/output) as a geopackage (`DestinationData.gpkg`) and as csv files to be uploaded in Conveyal. The destination data was prepared as follows:
* **Healthcare Opportunities**: Healthcare destination data is prepared in [`/analysis/DestDataPrep_1_Healthcare.R`](https://github.com/CTPSSTAFF/existing-inequities/blob/main/analysis/DestDataPrep_1_Healthcare.R). There are two outputs for healthcare destinations: 1) emergency healthcare and 2) nonemergency healthcare.

    * **Emergency Healthcare**: Emergency healthcare destinations are acute care hospitals located within MPO municipalities.  
    * **Nonemergency Healthcare**: Nonemergency healthcare destinations include emergency healthcare plus medical clinics and community health centers within MPO municipalities.  

* **Essential Places**: Essential places are identified as clusters of essential destinations. 

   * Essential  destinations consist of three types of destinations: **health**, **civic**, and **food**. The **health** type consists of all healthcare destinations (see Nonemergency healthcare destination description above) and retail pharmacies. The **civic** type consists of townhalls, post offices, and libraries. The **food** type consists of farmer's markets, and grocery stores. Essential destinations data prep happens here: [`/analysis/DestDataPrep_2A_EssentialDestinations.R`](https://github.com/CTPSSTAFF/existing-inequities/blob/main/analysis/DestDataPrep_2A_EssentialDestinations.R).  
   * Clusters of essential destinations are identified using density-based scanning [(DBSCAN)](https://www.rdocumentation.org/packages/dbscan/versions/1.1-10/topics/dbscan). Work to identify appropriate clustering parameters is here:[`/analysis/DestDataPrep_2B_EssentialPlaces_Clustering.R`](https://github.com/CTPSSTAFF/existing-inequities/blob/main/analysis/DestDataPrep_2B_EssentialPlaces_Clustering.R). 
   
      * Within the MPO's Inner Core sub-region, clusters were identified as groups of at least four essential destinations linked by a maximum of 161 meters (~ 2 minute walk as the crow flies). 
      * Outside of the Inner Core, clusters were identified as groups of at least four essential destinations linked by a maximum of 483 meters (~ 6 minute walk as the crow flies)  
      
  * We reviewed clustering results and determined that to be considered an essential place in this analysis, the cluster would have to contain at least two types of destinations (health/civic/food) and there are at least five destinations represented in the cluster. 
  * Essential places are represented by the most central destination in the cluster and is weighted by the number of destinations contained in the cluster.
 
* **Higher Education Opportunities**: Higher education opportunities are identified by colleges and universities within MPO municipalities where more than 50% of undergraduates live off-campus and there is graduate enrollment. Enrollment data is from the National Center for Education Statistics for the fall semester of the 2020-21 AY. Joining enrollment data to location data happens here: [`/analysis/DestDataPrep_3_HigherEd.R`](https://github.com/CTPSSTAFF/existing-inequities/blob/main/analysis/DestDataPrep_3_HigherEd.R) Higher education destinations are weighted by enrollment.

* **Parks and Open Space**: There are three types of open space destinations analyzed in this study. Work to identify access points to open spaces happens here: [`/analysis/DestDataPrep_4_Parks.R`](https://github.com/CTPSSTAFF/existing-inequities/blob/main/analysis/DestDataPrep_4_Parks.R). And pedestrian and road networks used to find access points are pulled from OpenStreetMap here: [`/notebooks/OSMPull.ipynb`](https://github.com/CTPSSTAFF/existing-inequities/blob/main/notebooks/OSMPull.ipynb) 

    * **Open Space**: Open space areas are identified as publicly accessible open spaces with a primary purpose of conservation or recreation. Open spaces must be at least partially within an MPO municipality and have an area greater than a half-acre. Access points to these spaces are then identified based on where the boundary of the open space overlaps with the pedestrian and/or road network.
    * **Open Space, Paths**: For shared use paths at least partially within the MPO region, a destination point is identified every 500 feet along the path with at least one point per path segment.
    * **Open Space, Conservation**: To identify access to large parks, we alsoidentified access to large open spaces. Large/Conservation open space access points correspond to open spaces that have an area greater than 124 acres.   

   Open spaces are not weighted by the amount of open space in a polygon, instead, open spaces are represented by how accessible they are from the transportation network. 

* **Employment Opportunities**: Jobs data comes from the 2018 LODES (LEHD Origin-Destination Employment Statistics) data which is prepared in the [Conveyal application](https://docs.conveyal.com/prepare-inputs/upload-spatial-data#lodes-dataset-import). Analyses are run with the total jobs reported in the Lodes dataset.

## Conveyal Access Analysis
