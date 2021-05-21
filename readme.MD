SnoIBM 2.0.0

An individual-based model for evaluating how phenology, growth and survival of juvenile salmonids may respond to altered thermal and flow regimes, presented in:

Fullerton, A.H., N. Sun, M.J. Baerwalde, B.L. Hawkins, and H. Yan. Mechanistic simulations suggest riparian restoration can partly offset climate impacts to juvenile Chinook Salmon in the Snoqualmie River.
  
----------------------------------------------------------------------------------

STEP I: Set up.

Follow these steps to download software, model input files, additional code, and libraries required to replicate our study.

1) Download R and RStudio.

2) Clone or download this repository (https://github.com/aimeefullerton/SnoIBM).

3) Create an R project in RStudio.

4) Install libraries in the setup section of code/ibm-model.R.

5) Acquire the necessary DHSVM-RBM output data (flow and temperature for each scenario). 
These are available upon request and are not stored here due to their very large file size.

----------------------------------------------------------------------------------

STEP II: Run pre-processing scripts to prepare data that will be needed for modeling.

1) code/pre-processing/1_pre-calculate_network_relationships.R

2) code/pre-processing/2_pre-load_wq_data.R

3) code/pre-processing/3_pre-calculate_growth.R

----------------------------------------------------------------------------------

STEP III: Run simulations for riparian and climate scenarios.

To run each simulation, you will need to update some variables describing which scenario(s) you wish to run at the top of ibm_model.R, and then run the script.

1) Calibration scenarios
      riparian.scenario = "climate0"
      climate.scenario = "current_climate_riparian_0"
      period = "calibration"

2) Run Baseline riparian scenario, 10 global climate models
      riparian.scenario = "riparian0"
      climate.scenario = choose one from 'climate.scenario.list'
      period = "historical" or "future"

3) Run Full riparian restoration scenario, 10 global climate models
      riparian.scenario = "riparian1"
      climate.scenario = choose one from 'climate.scenario.list'
      period = "historical" or "future"

4) Run Partial riparian restoration scenario, 10 global climate models
      riparian.scenario = "riparian3"
      climate.scenario = choose one from 'climate.scenario.list'
      period = "historical" or "future"

5) Run Riparian degradation rscenario, 10 global climate models
      riparian.scenario = "riparian2"
      climate.scenario = choose one from 'climate.scenario.list'
      period = "historical" or "future"

After running all scenarios, you should have new directories and files in your data.out folder.

The 'images' directories each contain 730 PNG files that picture the Snoqualmie basin and simulated fish. Black points represent where Chinook salmon will be spawned, red points represent Chinook salmon that have been spawned and are incubating, and blue points represent Chinook salmon that have emerged and are moving around the watershed. Points disappear when fish die or outmigrate. These images are only created for the simulation set by the global variable 'plot.iter' in "Setup".

----------------------------------------------------------------------------------

STEP IV: Generate figures for the manuscript by running the appropriate script in the code/publications folder.

----------------------------------------------------------------------------------

EXPECTED DIRECTORY STRUCTURE:

code/  
.....preprocessing/  
..........1_pre-calculate_network_relationships.R  
..........2_pre-load_wq_data.R  
..........3_pre-calculate_growth.R  
.....publications/  
..........paper_figures.R  
..........report_figures.R  
..........growth.potential.figures.R  
.....functions.R  
.....ibm_model.R  
.....growth_potential_model.R  
  
data.in/  
.....parameters/  
..........parameters.csv  
..........nSpawnersByYear_WDFW.csv  
..........Q_bm.csv  
.....sno.rbm.ssn/  
..........binaryID.db  
..........edges.shp  
..........sites.shp  
..........netID1.dat  
..........accessible_reaches.shp  
..........Basin_snq.shp  
..........chin_spawn_7785.shp  
..........DHSVM_streams_AnadromousUse.csv  
..........dnsegs.RData [calculated in step II-1 above]  
..........jct.list.RData [calculated in step II-1 above]  
..........upsegs.RData [calculated in step II-1 above]  
.....fish.growth.lookup/  
..........st.growth.array.chinook.RData [calculated in step II-3 above]  
.....rbm.data/ [contact aimee.fullerton@noaa.gov for output data from DHSVM-RBM needed here; large file size!]  
..........climate0/  
...............current_climate_riparian_0/  
....................Q.2002.df...Q.2013.df [calculated in step II-2 above from Outflow.Only.csv - this is flow output from DHSVM]  
....................WT.2002.df...WT.2013.df  [calculated in step II-2 above from Tw_output.csv - this is water temperature from RBM]  
...............current_climate_riparian_1/  
...............current_climate_riparian_2/  
...............current_climate_riparian_3/  
..........riparian0/  
...............bcc-csm1-1-m/  
....................Q.1994.df...Q.2005.df [calculated in step II-2 above from Outflow.Only.historical.csv - this is flow output from DHSVM]  
....................Q.2087.df...Q.2099.df [calculated in step II-2 above from Outflow.Only.future.csv - this is flow output from DHSVM]  
....................WT.1994.df...WT.2005.df [calculated in step II-2 above from Tw_output.historical.csv - this is water temperature from RBM]  
....................WT.2087.df...WT.2099.df [calculated in step II-2 above from Tw_output.future.csv - this is water temperature from RBM]  
...............CanESM2/  
...............CCSM4/  
...............CNRM-CM5/  
...............CSIRO-Mk3-6-0/  
...............HadGEM2-CC365/  
...............HadGEM2-ES365/  
...............IPSL-CM5A-MR/  
...............MIROC5/  
...............NorESM1-M/  
..........riparian1/  
...............bcc-csm1-1-m/  
....................Q.1994.df...Q.2005.df [calculated in step II-2 above from Outflow.Only.historical_s1.csv - this is flow output from DHSVM]  
....................Q.2087.df...Q.2099.df [calculated in step II-2 above from Outflow.Only.future_s1.csv - this is flow output from DHSVM]  
....................WT.1994.df...WT.2005.df [calculated in step II-2 above from Tw_output.historical_s1.csv - this is water temperature from RBM]  
....................WT.2087.df...WT.2099.df [calculated in step II-2 above from Tw_output.future_s1.csv - this is water temperature from RBM]  
...............CanESM2/  
...............CCSM4/  
...............CNRM-CM5/  
...............CSIRO-Mk3-6-0/  
...............HadGEM2-CC365/  
...............HadGEM2-ES365/  
...............IPSL-CM5A-MR/  
...............MIROC5/  
...............NorESM1-M/  
..........riparian2/  
...............bcc-csm1-1-m/  
....................Q.1994.df...Q.2005.df [calculated in step II-2 above from Outflow.Only.historical_s2.csv - this is flow output from DHSVM]  
....................Q.2087.df...Q.2099.df [calculated in step II-2 above from Outflow.Only.future_s2.csv - this is flow output from DHSVM]  
....................WT.1994.df...WT.2005.df [calculated in step II-2 above from Tw_output.historical_s2.csv - this is water temperature from RBM]  
....................WT.2087.df...WT.2099.df [calculated in step II-2 above from Tw_output.future_s2.csv - this is water temperature from RBM]  
...............CanESM2/  
...............CCSM4/  
...............CNRM-CM5/  
...............CSIRO-Mk3-6-0/  
...............HadGEM2-CC365/  
...............HadGEM2-ES365/  
...............IPSL-CM5A-MR/  
...............MIROC5/  
...............NorESM1-M/  
..........riparian3/  
...............bcc-csm1-1-m/  
....................Q.1994.df...Q.2005.df [calculated in step II-2 above from Outflow.Only.historical_s3.csv - this is flow output from DHSVM]  
....................Q.2087.df...Q.2099.df [calculated in step II-2 above from Outflow.Only.future_s3.csv - this is flow output from DHSVM]  
....................WT.1994.df...WT.2005.df [calculated in step II-2 above from Tw_output.historical_s3.csv - this is water temperature from RBM]  
....................WT.2087.df...WT.2099.df [calculated in step II-2 above from Tw_output.future_s3.csv - this is water temperature from RBM]  
...............CanESM2/  
...............CCSM4/  
...............CNRM-CM5/  
...............CSIRO-Mk3-6-0/  
...............HadGEM2-CC365/  
...............HadGEM2-ES365/  
...............IPSL-CM5A-MR/  
...............MIROC5/  
...............NorESM1-M/  
..........*a variety of data summary files that get generated during creation of figures will also be stored here
          
data.out/ [created using scripts]  
plots/ [created using scripts]  

----------------------------------------------------------------------------------
