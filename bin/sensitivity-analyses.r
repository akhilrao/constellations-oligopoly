#####
# Functions
#####

# Function to run sensitivity analysis with a given parameter grid and hQ grid
# Inputs:
# 1. params_varying: a list of parameter sets to run the sensitivity analysis over
# 2. hQ2_grid: a data.table of hQ2 grid points
# 3. RESULTS_FILENAME: a string to append to the output filename
# 4. param_names: a string to describe the parameters being varied
# 5. FILENAME_MODIFIER: a string to append to the output filename
# Outputs:
# 1. results_dfrm: a data.table of the results of the sensitivity analysis
do_sensitivity <- function(params_varying, hQ2_grid, RESULTS_FILENAME, param_names="market size (N), maneuver safety radius (rho)", FILENAME_MODIFIER=NULL, ...){
	results_cl <- makeCluster(N_CORES, outfile="outfile.log")
	registerDoSNOW(results_cl)
	pb <- txtProgressBar(max = length(params_varying), style = 3)
	progress <- function(n) setTxtProgressBar(pb, n)
	opts <- list(progress = progress)

	message("\nBeginning calculation... ", length(params_varying), " grid points for ", param_names,", ", N_CORES, " workers.")
	iter_time <- proc.time()[3]
	results <- foreach(i = (1:length(params_varying)), 
						.export=ls(envir=globalenv()), 
						.packages=c("tidyverse", "data.table", "GenSA"),
						.inorder=TRUE,
						.options.snow = opts) %dopar% {

		payoff_grid <- payoff_grid_maker2_dt(params_varying[[i]], hQ2_grid)

		opt1_res <- GenSA(c(250, 1000), fn=swf_1const_gensa, params=params_varying[[i]], lower=c(200, 0), upper=c(1300, 500000))

		variation_results <- reference_results_3 <- list(
			duopoly=solve_model_dt_2(payoff_grid),
			opt1=opt1_res,
			opt2=GenSA(c(490, opt1_res$par[2]*0.65, 510, opt1_res$par[2]*0.35, 1), fn=swf_2const_gensa_reform, params=params_varying[[i]], lower=c(450, 0, 0, 0, 0.5), upper=c(650, 65000, 650, 60000, 1.5), control=list(maxit=1e6, temperature=50)) # inputs redesigned to set the follower's height as a positive offset of the leader's height, and to set the follower's Q as a negative offset of the leader's size
		)

		parlist <- params_varying[[i]]

		if(length(parlist$objects_sh)>1) {parlist$objects_sh <- NULL}

		opt1_res <- variation_results$opt1$par
		opt2_res <- c(variation_results$opt2$par, variation_results$opt2$value)

		opt2_res[3] <- opt2_res[1] + opt2_res[3] # add the leader's height to the follower's height to get the follower's absolute height

		qualities <- cbind(report_qualities_2(params_varying[[i]], eqm=variation_results[[1]], opt1=opt1_res, opt2=opt2_res), params_varying=as.data.frame( parlist ) )

		outcome_labels <- rownames(qualities)

		rownames(qualities) <- NULL
		qualities <- cbind(outcome=outcome_labels, qualities)

		qualities

	}
	iter_time <- round(proc.time()[3] - iter_time,3)
	message("Finished calculation. Total time taken: ", iter_time, " seconds. Time per maximization: ", round(iter_time/length(params_varying),2), " seconds.")

	stopCluster(results_cl)

	results_dfrm <- rbindlist(results)

	# relabel all the column names of results_dfrm that contain params_varying to contain params instead
	colnames(results_dfrm) <- gsub("params_varying.", "params.", colnames(results_dfrm))

	if(length(FILENAME_MODIFIER)>0){
		RESULTS_FILENAME <- paste0(RESULTS_FILENAME, FILENAME_MODIFIER)
	}

	fwrite(results_dfrm, file=paste0("../outputs/fig-data/variation-results",RESULTS_FILENAME,".csv"))

	return(results_dfrm)
}


#####
# Script for sensitivity analyses and plots over parameter variations
#####

library(tidyverse)
library(patchwork)
library(data.table)
library(doSNOW)
library(kableExtra)
library(jsonlite)
library(GenSA)

source("functions.r") # Load functionset

#####
# Code block
#####

RESULTS_FILENAME_Nra <- "--N-radius-preference-sensitivity--REVISION"
RESULTS_FILENAME_atmo <- "--atmo_damages-sensitivity-linear"
RESULTS_FILENAME_sa <- "--preference-sensitivity--REVISION"

FILENAME_MODIFIER <- "--maxit-251"

source("calibration.r", echo=TRUE)

params_element_nobg <- params_element
params_element_nobg$objects_sh <- NULL

N_CORES <- 2 # # number of cores to use in parallelization. don't use too many!!! Rule of thumb: number of cores on your machine - 1 is the max. Use the detectCores() function to find out how many cores you have.

hMin_L <- 420
hMax_L <- 580
m_L <- 12
hMin_F <- 300
hMax_F <- 900
m_F <- 200

QMin_L <- 5000
QMin_F <- 500
QMax_L <- 80000
QMax_F <- 9000
n_L <- 251
n_F <- 251

i_L <- 0:m_L
i_F <- 0:m_F
h_L_grid <- hMin_L + (hMax_L - hMin_L) * i_L / m_L
h_F_grid <- hMin_F + (hMax_F - hMin_F) * i_F / m_F
Q_L_grid <- seq(from = QMin_L, to = QMax_L, length.out = n_L)
Q_F_grid <- seq(from = QMin_F, to = QMax_F, length.out = n_F)

cj_time <- proc.time()[3]
hQ2_grid <- CJ(h_L=h_L_grid, Q_L=Q_L_grid, h_F=h_F_grid, Q_F=Q_F_grid) %>%
	mutate(
		F_design_label = paste(Q_F, h_F, sep="_"),
		L_design_label = paste(Q_L, h_L, sep="_")
	)
setkey(hQ2_grid, L_design_label)
hQ2_grid[, F_designs := sequence(.N), by=L_design_label ]
setkey(hQ2_grid, F_design_label)
hQ2_grid[, L_designs := sequence(.N), by=F_design_label ]
cj_time <- round(proc.time()[3] - cj_time,3)
message("Finished hQ grid construction. Total time taken: ", cj_time, " seconds.")


# ##### Solve over parameter grid, plot sensitivity/variation

# Create grid for atmospheric damages and update params list
variation_grid_atmo <- data.frame(
	atmo_damages = seq(0, 6.5e5, length=24) #10
)
## with background objects version
params_atmo_damages <- rep(list(params_element), times=nrow(variation_grid_atmo))
for(i in seq_along(params_atmo_damages)){
	params_atmo_damages[[i]]$atmo_damages = variation_grid_atmo$atmo_damages[i]
}

# Create grid for preference parameters and update params list
variation_grid_sa <- data.frame(
	S_bar = seq(40, 70, length=10) #10
	) %>%
	mutate(
		a = 9 * ((S_bar^2 + 5625)/8650)
	)

## with background objects version
params_sa <- rep(list(params_element), times=nrow(variation_grid_sa))
for(i in seq_along(params_sa)){
	params_sa[[i]]$S_bar = variation_grid_sa$S_bar[i]
	params_sa[[i]]$a = variation_grid_sa$a[i]
}

# Create grid for N, radius, and preference parameters and update params list
variation_grid_Nra <- expand.grid(
	N = seq(5e6, 2.5e7, length=10), #15
	radius = seq(0.1, 0.7, length=15) #20
	)
# seq(0.05, 2, length=40)
## with background objects version
params_Nra <- rep(list(params_element), times=nrow(variation_grid_Nra))
for(i in seq_along(params_Nra)){
	params_Nra[[i]]$N = variation_grid_Nra$N[i]
	params_Nra[[i]]$radius = variation_grid_Nra$radius[i]
}

# Function to run sensitivity analysis with a given parameter grid and hQ grid
# with background objects
results_Nra <- do_sensitivity(params_varying=params_Nra, hQ2_grid=hQ2_grid, RESULTS_FILENAME=RESULTS_FILENAME_Nra, param_names="market size (N), maneuver safety radius (rho)", FILENAME_MODIFIER=FILENAME_MODIFIER)

results_sa <- do_sensitivity(params_varying=params_sa, hQ2_grid=hQ2_grid, RESULTS_FILENAME=RESULTS_FILENAME_sa, param_names="preference parameters (a and S_bar)", FILENAME_MODIFIER=FILENAME_MODIFIER)

results_atmo <- do_sensitivity(params_varying=params_atmo_damages, hQ2_grid=hQ2_grid, RESULTS_FILENAME=RESULTS_FILENAME_atmo, param_names="environmental damages (f)", FILENAME_MODIFIER=FILENAME_MODIFIER)
