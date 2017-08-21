# sHydrology
A Shiny-Leaflet interface to a stream flow database.

Current built to view WSC HYDAT stream flow data [click here](https://www.ec.gc.ca/rhc-wsc/default.asp?lang=En&n=9018B5EC-1).

### Current functionality:
 * Select gauge from a map
	 * filter locations based on period of record
 * View entire stream flow timeseries
 * Dynamic hydrograph zooming:
	 * drag-and-click zoom
	 * double-click to full extent
	 * optionnally use date picker or slider 
 * Perform as suite of hydrograph separation algorithms (14 in total, see below)
	 * display min/max range (green band) and median separated baseflow (dotted line)
 * Automatic baseflow recession coefficient computation (Linsley et.al., 1975)
 * View data as a table, and export data as *.csv 

### Screenshot:
![Screenshot](/images/screenshot_v1.png)


### Required R-dependent packages:
 * leaflet
 * shiny
 * shinyBS
 * ggplot2
 * RSQLite
 * zoo

### Hydrograph separation methods (found in *\functions\hydrograph_separation.R*):
 1.	**BF.LH:** The Lyne-Hollick digital filter (Lyne and Hollick, 1979), 3-pass sweep with *α=0.925* as discussed in Chapman (1999);
 2.	**BF.CM:** The Chapman-Maxwell digital filter (Chapman and Maxwell, 1996), using automatically computed baseflow recession coefficient (*k*);
 3.	**BF.BE:** The Boughton-Eckhardt digital filter (Boughton, 1993; Eckhardt, 2005) with computed *k* and *BFImax=0.8*;
 4.	**BF.JH:** The Jakeman-Hornberger digital filter (Jakeman and Hornberger, 1993) based on their IHACRES model with *C=0.3* and *α=-0.8*;
 5.	**BF.Cl:** The 'Clarifica' method of Clarifica Inc. (2002);
 6.	**BF.UKn:** The UK Institute of Hydrology (or Wallingford) method (Institute of Hydrology, 1980), sweeping minimum of Piggott et.al. (2005);
 7.	**BF.UKx:** The UK Institute of Hydrology/Wallingford method (Institute of Hydrology, 1980), sweeping maximum of Piggott et.al. (2005);
 8.	**BF.UKm:** The UK Institute of Hydrology/Wallingford method (Institute of Hydrology, 1980), sweeping median;
 9.	**BF.HYSEP.FI:** The HYSEP fixed-interval method (Sloto and Crouse, 1996), with known catchment area;
 10.	**BF.HYSEP.SI:** The HYSEP sliding-interval method (Sloto and Crouse, 1996), with known catchment area;
 11.	**BF.HYSEP.LM:** The HYSEP local minima method (Sloto and Crouse, 1996), with known catchment area;
 12.	**BF.PART1:** The PART method (Rutledge, 1998), with known catchment area, pass 1 of 3 antecedent recession requirement;
 13.	**BF.PART2:** The PART method (Rutledge, 1998), with known catchment area, pass 2 of 3 antecedent recession requirement;
 14.	**BF.PART3:** The PART method (Rutledge, 1998), with known catchment area, pass 3 of 3 antecedent recession requirement.

### Current version: 1.0
**Task list:**

 - [x] Build main Leaflet/Shiny interface
 - [x] Write hydrograph separation routines
 - [ ] Add flow summary tab
 - [ ] Write ecological/environment flow (E-Flow) statistics
 - [ ] Flow duration curve/return periods
 - [ ] Tests for stationarity (Mann-Kendall, double-mass, etc.)
 - [ ] Drought indices (i.e., MDSI)
 - [ ] Hydrograph parsing (rising limb, falling limb, baseflow recession)
 - [ ] Continuous to discrete hydrograph translation

### License

sHydrology hosted on GitHub is released under the MIT license.

### Contributors

Mason Marchidon P.Eng M.ASc, Hydrologist for the [Oak Ridges Moraine Groundwater Program](http://oakridgeswater.ca/)

### References

Boughton, W.C., 1993. A hydrograph-based model for estimating the water yield of ungauged catchments. Hydrology and Water Resources Symposium, Institution of Engineers Australia, Newcastle: 317-324.

Chapman, T.G. and A.I. Maxwell, 1996. Baseflow separation - comparison of numerical methods with tracer experiments.Institute Engineers Australia National Conference. Publ. 96/05, 539-545.

Chapman T.G., 1999. A comparison of algorithms for stream flow recession and baseflow separation. Hydrological Processes 13: 710-714.

Clarifica Inc., 2002. Water Budget in Urbanizing Watersheds: Duffins Creek Watershed. Report prepared for the Toronto and Region Conservation Authority.

Eckhardt, K., 2005. How to construct recursive digital filters forbaseflow separation. Hydrological Processes 19, 507-515.

Institute of Hydrology, 1980. Low Flow Studies report. Wallingford, UK.

Jakeman, A.J. and Hornberger G.M., 1993. How much complexity is warranted in a rainfall-runoff model? Water Resources Research 29: 2637-2649.

Linsley, R.K., M.A. Kohler, J.L.H. Paulhus, 1975. Hydrology for Engineers 2nd ed. McGraw-Hill. 482pp.

Lyne, V. and M. Hollick, 1979. Stochastic time-variable rainfall-runoff modelling. Hydrology and Water Resources Symposium, Institution of Engineers Australia, Perth: 89-92.

Piggott, A.R., S. Moin, C. Southam, 2005. A revised approach to the UKIH method for the calculation of baseflow. Hydrological Sciences Journal 50(5): 911-920.

Rutledge, A.T., 1998. Computer Programs for Describing the Recession of Ground-Water Discharge and for Estimating Mean Ground-Water Recharge and Discharge from Streamflow Records-Update, Water-Resources Investigation Report 98-4148.

Sloto, R.A. and M.Y. Crouse, 1996. HYSEP: A Computer Program for Streamflow Hydrograph Separation and Analysis U.S. Geological Survey Water-Resources Investigations Report 96-4040.