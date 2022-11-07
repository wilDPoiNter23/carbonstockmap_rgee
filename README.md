# carbonstockmap_rgee
Replication code for the paper "Species-based mapping of carbon stocks in saltmarsh: Tianjin coastal zone as a case study". This framework's core principle is to include the remote sensing time-series images and in-situ data. Here, we follow the general idea of the “remote sensing-field” approaches and modify it according to saltmarsh carbon stock accounts.
##  STEP 1: Data preparation
First, it combined all available Sentinel-2 time series images from 2019 to 2021 with position data from a vast number of field surveys to generate a classification dataset. Species of salt marsh plants (S. salsa, P. australis and S. alterniflora) were identified, found, and sampled during the field survey phase. Subsequent laboratory processing steps determined AGB, BGB and carbon conversion coefficients of sampled plants.
##  STEP 2: Classification in GEE
Second, four machine learning methods were utilized to train and classify the saltmarsh vegetation Temporal-Phenological-Spatial feature dataset (Model1: ML-TPS classification model), estimate the classification accuracy, and select the optimal saltmarsh vegetation classification result as the base map for the following carbon stock accounting.
##  STEP 3: Estimation of vegetation AGB and BGB in salt marsh
Thirdly, species-based vegetation indexes-ABG remote sensing inversion models (Model2: VIs-ABG inversion model) and ABG-BGB allometric models (Model3) were developed in order to generate a typical saltmarsh plant carbon pool at a pixel-scale in the Tianjin area.
##  STEP 4: Mapping the carbon stock of vegetation
Finally, the map of plant carbon stocks in saltmarsh was obtained. Blue carbon in other urban areas (including, but not limited to, coastal salt marshes) may be accounted for and mapped by extending this framework and, the results can be compared.

**This repository hosts:**
* R scripts, to process remotely-sensed data, train classification model into Google Earth Engine (need to download 'RGEE' package).
* Create a Google account, if you do not have one, and require access to Earth Engine https://signup.earthengine.google.com.
* Open the QGIS project files to replicate maps with the appropriate layout.
