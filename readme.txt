
----------------------------------------------------------------------------------
SnoIBM, v 2.0.0

An individual-based model for evaluating how phenology, growth and survival of juvenile salmonids may respond to altered thermal and flow regimes, presented in:

Fullerton, A.H., N. Sun, M.J. Baerwalde, B.L. Hawkins, and H. Yan. Mechanistic simulations suggest riparian restoration can partly offset climate impacts to juvenile Chinook Salmon in the Snoqualmie River.
----------------------------------------------------------------------------------
STEP I: Set up.

Follow these steps to download software, model input files, additional code, and libraries required to replicate our study.

1) Download R and RStudio.

2) Clone or download model input files and additional code at https://github.com/aimeefullerton/SnoIBM.

3) Create an R project in RStudio or open the 'SnoIBM.Pproj' file.

4) Install libraries in the setup section of SnoIBMv2.0.R.
----------------------------------------------------------------------------------
STEP II: Run simulations for riparian and climate scenarios.

To run each simulation, you will need to update some variables describing which scenario(s) you wish to run at the top of the SnoIBMv2.0.R code, and then run the script. NOTE: The DHSVM-RBM data needed to run the calibration scenarios and the Baseline riparian scenario with 10 global climate models (options 1 and 2 below) are provided in this repository with the assistance of git lfs (Git Large File Size). The DHSVM-RBM data needed to run the riparian scenarios (options 3 through 5 below) are available upon request (not stored here due to their very large file size).

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

STEP III: Generate figures for the manuscript.


