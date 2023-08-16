# Replication files for "Oligopoly competition between satellite constellations will reduce economic welfare from orbit use"

This repository contains data, code, and images for the paper "Oligopoly competition between satellite constellations will reduce economic welfare from orbit use".

## How is this repository organized?

* /bin contains all of the R code. The code in here runs the benchmark model, calibration, and sensitivity analyses. It also contains functions used in running simulations and generating plots.
    * functions.r contains functions used to solve optimization problems, calculate figures of merit, and other helpers.
    * calibration.r calibrates model parameters.
    * benchmark-simulation.r runs the main analysis.
    * sensitivity-analyses.r runs a sensitivity analysis with particular parameters.
    * sensitivity-analysis.sh is a shell script which runs several variants of sensitivity analysis by calling sensitivity-analyses.r repeatedly.
* /outputs contains output tables (in /results-tables) and data used for generating figures (in /fig-data).
* /bin/plots contains code used for plotting.
* /data contains datasets used in and produced from model analysis.
* /figures contains output figures.

Note that the sensitivity analyses will take a while. This code was run on a machine with an Intel Xeon Gold 6230 and 256 GB RAM using R 4.2.2.