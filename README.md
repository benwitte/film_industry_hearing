# Film Industry Permit Analysis

### Note: All Analysis Conducted by New York City Counsel. This is a copy of original repository, found here: https://github.com/NewYorkCityCouncil/film_industry_hearing

**Contents**

This document combines files from Kyle Walker's [tidycensus](https://walkerke.github.io/tidycensus/index.html) and [Open Data](https://data.cityofnewyork.us/City-Government/Film-Permits/tg4x-b46p) to map and analyze the distribution of film permits.

Permit locations were geocoded using [NYC Developer Portal's Geoclient API](https://docs.google.com/document/d/1EoIEPfeFo19xR-PXFcMnF1XYW9p11Oc1ksf74BQC7tk/edit), and mapped using R's [sf](https://r-spatial.github.io/sf/articles/sf1.html) and [leaflet](https://rstudio.github.io/leaflet/) packages.

**Notes on Analysis**

This data was also combined with [311 complaints](https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2010-to-Present/erm2-nwe9) to determine if there were any insightful patterns between permit locations and 311 complaints. This was done in a short case study by creating a .2 mile radius buffer around each permit location and isolating 311 complaints to only those that occured within that radius and during permit hours. Due to time and data quality constraints, however, this analysis was not pursued further.

Top permit locations were gathered based on string names. This does, however, mean that the analysis is subject to miscalculations based on the sequence or manner in which streets are recorded. For example, if a film permit is located on street A between streets B and C, it would be considered different from a film permit located on street A between streets C and B. Further analysis should take this issue into consideration and correct it for a better quality analysis.
