# NV_noticereview
This is a filtration of water rights applications (.csv files) in the state of Nevada and creation of decimal degree coordinates of the application's section centroid. It converts PLSS to decimal degree latitude and longitude. The code expects previous download of NV PLSS centroids and the unedited, state-provded monthly water application csv.

## Before Using this Code
1. Download the BLM PLSS first section polygons for Nevada (https://gis.blm.gov/nvarcgis/rest/services/BLM_Nevada_Public_Land_Survey_System/PLSS_First_Division_Section/FeatureServer). 
2. Use ArcGIS Pro to import the polygons, run "Feature to Point", selecting that the centroids be inside the polygons.
3. Edit the new point layer to add 2 columns, one for latitude (Lat_dd) and one for longitude (Long_dd). These names are used in the notice review code.
4. Calculate the geometry for both, using geometric coordinate system of WGS 84. 
5. Export this point layer shapefile (and relevant dependents) into a folder with the NV Notice Review project. 

Continue on with use of this Shiny App to filter Nevada Water Rights Applications!