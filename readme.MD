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
