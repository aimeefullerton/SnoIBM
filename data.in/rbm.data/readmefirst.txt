  
  Unit for all files: flow: m3 per 3hour; Tw: deg C; time step: 3-hour

  1. current_climate_current_riparian_baseline: calibrated/validated model runs, considered as benchmark, use current riparian + Livneh meteorological data. Sim period: 2001/10 - 2013/12. 

  2. current_climate_riparian_1: current climate (Livneh) + riparian_1 (full recovery). Sim period: 2001/10 - 2013/12

  3. current_climate_riparian_2: current climate + riparian_2 (deforestation). Sim period: 2001/10 - 2013/12

  4. current_climate_riparian_3: current climate + riparian_3 (moderate recovery). Sim period: 2001/10 - 2013/12

  5. future_climate_current_riparian: contains 10 folders assoicated with 10 selected GCMs; note that there are both historical and future climate runs (all runs based on current riparian). The historical run used each GCM's historical simulation (not Livneh observational data), which is compared with its respective future GCM simulation to get the differences. This is to ensure consistent model biases over the historical and future period. All future climate is under RCP8.5 scenario.

     historical sim period: 1993/10 - 2005/9  (GCM historical period ends in 2005, number of years consistent with the baseline run)
     future sim period: 2087/10 - 2099/9

*** AHF NOTE *** Original files from Ning Sun are on BluPassport Drive. Files in this folder are processed into single water year files.