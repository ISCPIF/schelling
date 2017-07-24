Space Matters
===

Repository for systematic analysis of geosimulation models sensitivity to spatial initial conditions.


## DensityGrids

Contains density grids and classification scripts.

 - `CityNess` : caracterisation and classification of grids
 - `densityGrids`, `densityGrids2`, `explored`, `grids1`, `grids1_reformated` : raw grids


## Schelling

Implementation of the schelling model to study the effect of the space on the simulation results.


### Scala implementation

To run it you should use SBT, and execute: ```sbt run```

### NetLogo implementation

TODO : documenter `Schelling-Densite.nlogo` et `Schelling-Diversite.nlogo` et coupler au générateur NetLogo.

### Visualise the results

Go to the online app : https://clementinegeo.shinyapps.io/visualisation/
Or use RStudio, open the files ui.R or server.R in the "visualisation" folder, and click : ```Run App```

To visualise your own model results, upload csv files named "resultmicro.csv" for cell description across steps, and "resultmacro.csv" for parameters and output measures across steps.


## Sugarscape

 - `Sugarscape.nlogo` : Netlogo modified implementation, includes density generator.

 - `Model.oms` and `Exploration.oms` : OpenMole exploration script

 - `R` scripts : analysis of results
