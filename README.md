# Identifying Transportation Inequities in the Boston Region
This repository contains data and methodology to support a UPWP project analyzing existing inequities within the Boston Region MPO.
## Purpose
In 2021 and 2022, the Boston Region MPO conducted a study called Identifying Transportation Inequities in the Boston Region. The purpose of the study was to develop a baseline assessment of existing transportation inequities in the Boston region. Existing MPO policies related to equity align with federal civil rights and environmental justice (EJ) requirements that focus on preventing future discrimination. To improve the effectiveness of current policies and ultimately the transportation outcomes for EJ populations in the Boston region, this study sought to quantify existing transportation inequities among EJ populations and households without access to a vehicle using several destination access and transportation cost metrics. This GitHub page provides documentation for the destination access metrics analyzed using the analysis tool Conveyal.

### Analysis Structure
* Demographic Data Preparation
* Destination Data Preparation
* Conveyal Access Analysis
* Processing Conveyal Outputs
* Interactive App

## Demographic Data Preparation
This study pulls demographic data from the 2016–2020 American Community Survey (ACS) Census and 2020 Decennial Census to determine poverty status, minority status, and household vehicle availability.
* A person is classified as **low-income**  if their family income is less than 200% of the federal poverty level.
* A person is classified as **minority** if they identify as a race other than White or as Hispanic or Latino/a.
* **Zero-vehicle households** are those that indicate that there are zero operating vehicles available for the household.

Demographic data is pulled by census tract for the whole population within the Boston Region MPO area.

### Pulling Demographic Data
Population proportions are pulled from aggregated ACS data then mapped onto decennial population counts here: [`/analysis/DemoDataPrep_Pull_Census.R`](https://github.com/CTPSSTAFF/existing-inequities/blob/main/analysis/DemoDataPrep_Pull_Census.R) 
This script uses TidyCensus calls from functions that are here: [`/functions/census_demo_pull.R`](https://github.com/CTPSSTAFF/existing-inequities/blob/main/functions/census_demo_pull.R)
### Dasymetric Population Mapping
To map Census and ACS demographic data onto Conveyal raster outputs, we create a gridded version of the demographic data that matches the grid used in Conveyal analyses, which consists of approximately 225 meter by 225 meter grid cells. This process follows binary dasymetric interpolation as shown [here](https://pysal.org/tobler/notebooks/binary_dasymetric.html) and happens in a python notebook here: [`/notebooks/Dasymetric Population Map.ipynb`](https://github.com/CTPSSTAFF/existing-inequities/blob/main/notebooks/Dasymetric%20Population%20Map.ipynb).
To map the census tract level demographic onto the raster grid, we allocate population density within a tract based on two filters: residential road and land cover classification. This process starts by pulling road centerlines and buffering the shapes. Then, within the road buffer we find the areas that have a land classification of at least five percent of a 30-meter square area as impervious surface. Then, after applying the dasymetric filters, we interpolate the area so that the population density within a tract is assigned to the grid cells that match the Conveyal outputs.

## Destination Data Preparation
This project analyzes access to various types of destinations throughout the MPO region. For data that required geocoding, see [`/geocoding/`](https://github.com/CTPSSTAFF/existing-inequities/tree/main/geocoding) for notes on the process. Destination outputs are saved here: [`/output/`](https://github.com/CTPSSTAFF/existing-inequities/tree/main/output) as a geopackage (`DestinationData.gpkg`) and as .csv files to be uploaded in Conveyal. The destination data was prepared as follows:
* **Healthcare Opportunities:** Healthcare destination data is prepared in [`/analysis/DestDataPrep_1_Healthcare.R`](https://github.com/CTPSSTAFF/existing-inequities/blob/main/analysis/DestDataPrep_1_Healthcare.R). There are two outputs for healthcare destinations: 1) emergency healthcare and 2) non-emergency healthcare.  Both datasets come from MassGIS.

    * **Emergency Healthcare:** Emergency healthcare destinations are acute care hospitals located within MPO municipalities.  
    * **Non-emergency Healthcare:** Non-emergency healthcare destinations include emergency healthcare plus medical clinics and community health centers within MPO municipalities.  

* **Essential Places:** Essential places are identified as clusters of essential destinations. 
 
   * Essential  destinations consist of three types of destinations: **health**, **civic**, and **food**. The **health** type consists of all healthcare destinations (see the non-emergency healthcare destination description above) and retail pharmacies. The **civic** type consists of townhalls, post offices, and libraries. The **food** type consists of farmer's markets and grocery stores. These data come from several sources, including MassGIS, the Metropolitan Area Planning Council, and the Massachusetts Department of Public Health, and the United States Postal Service. Essential destinations data preparation happens here: [`/analysis/DestDataPrep_2A_EssentialDestinations.R`](https://github.com/CTPSSTAFF/existing-inequities/blob/main/analysis/DestDataPrep_2A_EssentialDestinations.R).  
   * Clusters of essential destinations are identified using density-based scanning [(DBSCAN)](https://www.rdocumentation.org/packages/dbscan/versions/1.1-10/topics/dbscan). Work to identify appropriate clustering parameters is here: [`/analysis/DestDataPrep_2B_EssentialPlaces_Clustering.R`](https://github.com/CTPSSTAFF/existing-inequities/blob/main/analysis/DestDataPrep_2B_EssentialPlaces_Clustering.R). 
   
      * Within the MPO's [Inner Core subregion](https://www.mapc.org/get-involved/subregions/), clusters were identified as groups of at least four essential destinations linked by a maximum of 161 meters (~ 2-minute walk as the crow flies). 
      * Outside of the Inner Core, clusters were identified as groups of at least four essential destinations linked by a maximum of 483 meters (~ 6-minute walk as the crow flies)  
      
  * We reviewed clustering results and determined that to be considered an essential place in this analysis, the cluster must contain at least two types of destinations (health, civic, and/or food) and contain at least five destinations within the cluster.
  * Essential places are represented by the most central destination in the cluster and are weighted by the number of destinations contained in the cluster.
 
* **Higher Education Opportunities:** Higher education opportunities are identified by colleges and universities within MPO municipalities where more than 50 percent of undergraduates live off-campus and/or there is graduate enrollment. Enrollment data is from the National Center for Education Statistics for the fall semester of the 2020–21 academic year and school locations are from MassGIS. Joining enrollment data to location data happens here: [`/analysis/DestDataPrep_3_HigherEd.R`](https://github.com/CTPSSTAFF/existing-inequities/blob/main/analysis/DestDataPrep_3_HigherEd.R) Higher education destinations are weighted by enrollment.

* **Parks and Open Space:** There are three types of open space destinations analyzed in this study. Work to identify access points to open spaces happens here: [`/analysis/DestDataPrep_4_Parks.R`](https://github.com/CTPSSTAFF/existing-inequities/blob/main/analysis/DestDataPrep_4_Parks.R).Pedestrian and road networks used to find access points are pulled from OpenStreetMap here: [`/notebooks/OSMPull.ipynb`](https://github.com/CTPSSTAFF/existing-inequities/blob/main/notebooks/OSMPull.ipynb) 

    * **All Open Space:** This is defined as publicly-accessible open spaces with a primary purpose of conservation or recreation. The data are from MassGIS. Open spaces must be at least partially within an MPO municipality and have an area greater than a half-acre. Access points to these spaces are then identified based on where the boundary of the open space overlaps with the pedestrian and/or road network.
    * **Open Space—Paths:** These are shared use paths at least partially within the MPO region. The destination points are identified every 500 feet along the path with at least one point per path segment.
    * **Open Space—Conservation:** To identify access to large parks, we also analyzed access to large open spaces only. Access points to these locations correspond to open spaces that have an area greater than 124 acres.   

   Open spaces are not weighted by the amount of open space in a polygon. Rather, they are represented by how accessible they are from the transportation network.

* **Job Opportunities:** These data comes from the 2018 LODES (LEHD Origin-Destination Employment Statistics) data, which is prepared in the [Conveyal application](https://docs.conveyal.com/prepare-inputs/upload-spatial-data#lodes-dataset-import). AAnalyses use the total jobs reported in the LODES dataset.

## Conveyal Access Analysis
Access to destinations was calculated with [Conveyal's regional analysis](https://docs.conveyal.com/analysis/regional) for a typical weekday in fall 2019. In order to compare results, separate regional analyses were run for  separate regional analyses were run for every distinct mode and destination combination. Outputs of the Coveyal runs are saved here: [`data/ConveyalRuns/Sept2019`](https://github.com/CTPSSTAFF/existing-inequities/tree/main/data/ConveyalRuns/Sept2019).There are three key components of the Conveyal analysis: 1) origins/destinations, 2) representation of the transportation network, and 3) analysis settings.
* Origins used for this analysis are the center points of a rectangular grid covering the MPO service area. Destinations were uploaded to Conveyal as free form points.
* The transportation network in Conveyal is a network bundle and is a combination of GTFS files and an OpenStreetMap extract.
   * **OSM Extract:** For this work we used the OpenStreetMap layer prepared by Conveyal and MassDOT’s Office of Performance Management and Innovation (OPMI) that uses impedances pulled from Streetlight data. Streetlight impedance data used Sept2019_Streetlight(v.5.3) data which has traffic impedances by time period. The 6:00 AM - 9:00 AM file was used when the analysis time period was the morning peak. The 12:00 PM - 3:00 PM file was used when the analysis time period was midday. No impedances were used for the weekend travel.
   * **GTFS file sources:** 
      * MBTA: from MBTA GTFS [archive](https://cdn.mbta.com/archive/archived_feeds.txt), https://cdn.mbtace.com/archive/20190919.zip
      * Brockton Area Transit Authority: https://transitfeeds.com/p/massdot/94/20190904
      * Cape Ann Transit Authority: https://transitfeeds.com/p/massdot/95/20190920
      * Greater Attleboro and Taunton Region Transit Authority: https://transitfeeds.com/p/massdot/98/20190921
      * Lowell Regional Transit Authority: https://transitfeeds.com/p/massdot/99/20190920
      * Merrimack Valley Regional Transit Authority: https://transitfeeds.com/p/massdot/100/20190914
      * MetroWest Regional Transit Authority: https://transitfeeds.com/p/massdot/101/20190831
      * Montachusett Regional Transit Authority: https://transitfeeds.com/p/massdot/102/20190921
   * Note: transportation management association (TMA) shuttles were not used  due to GTFS availability and inconsistent public access.
* Conveyal settings were selected to align where possible with similar metrics used in the MPO’s disparate impact and disproportionate burden (DI/DB) analysis for the Long-Range Transportation Plan . Settings selected for different modes and time periods are recorded here: [`data/ConveyalRuns/`](https://github.com/CTPSSTAFF/existing-inequities/tree/main/data/ConveyalRuns).

![image](https://user-images.githubusercontent.com/56197359/178547072-19015164-df70-4d37-ac9f-f4fda300b277.png)

## Processing Conveyal Outputs
Conveyal access rasters are downloaded and processed in the script here: [`/analysis/Process_Conveyal.R`](https://github.com/CTPSSTAFF/existing-inequities/blob/main/analysis/Process_Conveyal.R). This process uses the dasymetric raster output to weight access results by different demographic populations and aggregation areas prepared here: [`/analysis/AggregationBoundaries.R`](https://github.com/CTPSSTAFF/existing-inequities/blob/main/analysis/AggregationBoundaries.R). To summarize the access for the entire MPO and within aggregation areas, we find average of access opportunities available to a population where access opportunities within a grid cell are weighted by the population estimated to live within that grid cell. 
* Note: When applying the dasymetric weighting, we do not recommend aggregating at a sub-municipal geographic unit, as the demographic inputs from the census are not appropriate at that scale. 

To compare average access opportunities by population, we calculate a **ratio** where the numerator is the average opportunities accessible by a population of concern (low-income/minority/zero-vehicle-households) and the denominator is the average opportunities accessible by the non-protected population. When a ratio is 1, that indicates parity where both populations have equal access. When the ratio is below 1, this indicates better access to the non-protected population. When the ratio is above 1, this indicates better access to the protected population. All Conveyal runs are recorded here: [`output/access_all_comp.csv`](https://github.com/CTPSSTAFF/existing-inequities/blob/main/output/access_all_comp.csv). 

## Interactive App
To summarize and visualize results, we developed an interactive app which is here: [`/app`](https://github.com/CTPSSTAFF/existing-inequities/tree/main/app). And is hosted online here: (http://shinyapps.ctps.org/ExistingInequities/).
![image](https://user-images.githubusercontent.com/56197359/178550685-1dfda3ad-f546-423f-9036-47776e5f6152.png)
