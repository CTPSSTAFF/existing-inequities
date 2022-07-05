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
* A person is classified as low-income if their household income is less than 200% of the federal poverty level.
* A person is classified as minority if they are a race other than White or as Hispanic  or Latino/Latina.
* Zero-vehicle households are those that indicate that there are zero operating vehicles available for the household.
Demographic data is pulled for the whole populations and just for the adult population (18 years or older) within the Boston Region MPO service area.

Population proportions are pulled from aggregated ACS data then mapped onto decennial population counts here: [`existing-inequities/analysis/DemoDataPrep_Pull_Census.R`] (https://github.com/CTPSSTAFF/existing-inequities/blob/main/analysis/DemoDataPrep_Pull_Census.R)


