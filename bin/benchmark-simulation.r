# Script to solve for equilibrium and optima of the orbital location game under benchmark calibration.

#####
# Functions/packages block
#####

library(tidyverse)
library(patchwork)
library(data.table)
library(doSNOW)
library(kableExtra)
library(jsonlite)
library(GenSA)
source("functions.r") # Load functionset
library(pryr) # For memory profiling
library(tictoc)

#####
# Code block
#####

RESULTS_FILENAME <- "--benchmark-REVISIONS--cobbdouglas--newLinearGrid--tau-2"

source("calibration.r", echo=TRUE)

params_element$atmo_damages <- 0

# Make solver grid. The values are chosen after experimentation to balance fidelity against grid size.
tic()
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
n_L <- 101
n_F <- 101

# Final grid construction
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

##### Solve under reference calibration, plot outcomes

# Maker payoff grid. This step takes most of the time, especially when the tensor gets large.
iter_time <- proc.time()[3]
message("Beginning calculation...")
pg_time <- proc.time()[3]
payoff_grid <- payoff_grid_maker2_dt(params_element, hQ2_grid)
pg_time <- round(proc.time()[3] - pg_time,3)
message("Finished payoff grid construction. Total time taken: ", pg_time, " seconds.")

# Solve: duopoly, one-constellation planner, two-constellation planner
duopoly <- solve_model_dt_2(payoff_grid)
opt1 <- GenSA(c(289.738, 40264.2), fn=swf_1const_gensa, params=params_element, lower=c(0, 0), upper=c(2000, 50000)) # GenSA is slow and this one is regular, let optim do it
tic()
opt2 <- GenSA(c(490, opt1$par[2]*0.65, 510, opt1$par[2]*0.35, 1), fn=swf_2const_gensa_reform, params=params_element, lower=c(450, 0, 0, 0, 0.5), upper=c(650, 65000, 650, 60000, 1.5), control=list(maxit=1e6, temperature=50)) # inputs redesigned to set the follower's height as a positive offset of the leader's height
toc()

# Gather results into a list
reference_results <- list(
	duopoly=duopoly,
	opt1=opt1,
	opt2=opt2
	)

iter_time <- round(proc.time()[3] - iter_time,3)
message("Finished calculation. Total time taken: ", iter_time, " seconds.")
toc()

# The difference between the row counts tells us how many designs violated a participation constraint. In case you're curious.
dim(hQ2_grid)
dim(payoff_grid)

# The following code is for reporting the results.
opt1_res <- reference_results$opt1$par
opt2_res <- c(reference_results$opt2$par, reference_results$opt2$value)
opt2_res[3] <- opt2_res[1] + opt2_res[3] # add the leader's height to the follower's height to get the follower's absolute height
benchmark_qualities <- report_qualities_2(params_element, eqm=reference_results[[1]], opt1=opt1_res, opt2=opt2_res)
round(benchmark_qualities,2) # This is the main reporting table

# Make a nicely formatted html output
benchmark_qualities[-nrow(benchmark_qualities),] %>%
	kbl(
		caption = "Summary of equilibrium and optimum", 
		digits=2, 
		booktabs = TRUE,
		linesep = "\\addlinespace"
		) %>%
	cat(., file = paste0("../outputs/results-tables/results",RESULTS_FILENAME,".html"))

# Make dataset for bar plot of sizes and congestions of leader and follower under each scenario. See plots.r for the plotting code using this data.
bp_wide <- as.data.frame(benchmark_qualities)
bp_wide$outcome <- rownames(bp_wide)
rownames(bp_wide) <- NULL
bp_long <- pivot_longer(bp_wide, !outcome, names_to="player", values_to="value") %>%
	mutate(
		scenario = 
		case_when(
			player=="L" ~ "Duopoly",
			player=="F" ~ "Duopoly",
			player=="opt2_L" ~ "opt2",
			player=="opt2_F" ~ "opt2",
			player=="opt1" ~ "opt1"
		),
		player =
		case_when(
			player=="L" ~ "Leader",
			player=="F" ~ "Follower",
			player=="opt2_L" ~ "Leader",
			player=="opt2_F" ~ "Follower",
			player=="opt1" ~ "Planner"
		)
	)

# Write the csv out
write.csv(bp_long, file=paste0("../outputs/fig-data/barplot-data-",RESULTS_FILENAME,".csv"))
