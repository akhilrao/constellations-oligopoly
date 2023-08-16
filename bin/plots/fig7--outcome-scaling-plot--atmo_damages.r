# Script to make plots for PNAS constellations and competition paper. This produces Figure 7.

library(tidyverse)
library(patchwork)
library(viridis)
library(data.table)
library(jsonlite)
library(RColorBrewer)

setwd("../")

source("functions.r") # Load standard functionset
source("calibration.r")

# use commandArgs to set the RESULTS_FILENAME variable

args <- commandArgs(trailingOnly = TRUE)

RESULTS_FILENAME <- args[1]

if(length(args)>0){
	RESULTS_FILENAME <- args[1]
}
if(length(args)==0){
	RESULTS_FILENAME <- "--atmo_damages-sensitivity-linear--maxit-BIG"
}

setwd("plots")

#####
# Why does this happen?
#####

bp_scaling_long_full <- fread(paste0("../../outputs/fig-data/variation-results",RESULTS_FILENAME,".csv"))

# Find grid nodes that are closest to the benchmark calibration
## atmo_damages
closest_to_benchmark_atmodamages <- bp_scaling_long_full$params.atmo_damages[which.min( (params_element$atmo_damages - bp_scaling_long_full$params.atmo_damages)^2 )]

### N-radius

# Reassign -- vestigial
bp_scaling_long_atmodamages <- bp_scaling_long_full

### Line plots showing how optimal system sizes and altitudes scale with market size and radius

size_alt_scaling_wide <- bp_scaling_long_atmodamages %>%
	select(outcome, params.atmo_damages, L, F, opt2_L, opt2_F, opt1, min) %>% # nolint
	filter(outcome=="size" | outcome=="altitude") 
	
# Spread size_alt_scaling_wide wider on outcome. Create new columns for each level of outcome.
size_alt_scaling_wider <- size_alt_scaling_wide %>%
	pivot_wider(names_from = outcome, values_from = c(L, F, opt2_L, opt2_F, opt1, min))

# Set entries in columns of size_alt_scaling_wider with _altitude in the name to NA if the corresponding entry in the _size column is <1e-6. E.g. if L_size < 1e-6, set L_altitude to NA.
size_alt_scaling_wider <- size_alt_scaling_wider %>%
	group_by(params.atmo_damages) %>%
	mutate(
		opt2_L_altitude = ifelse(opt2_L_size < F_size, NA, opt2_L_altitude),
		opt2_F_altitude = ifelse(opt2_F_size < F_size, NA, opt2_F_altitude),
		opt1_altitude = ifelse(opt1_size < F_size, NA, opt1_altitude),
		min_altitude = ifelse(min_size < 1e-6, NA, min_altitude)
	)

size_alt_scaling_long_intermediate <- size_alt_scaling_wider %>%
	pivot_longer(
		cols = c(
			L_altitude, 
			L_size, 
			F_altitude, 
			F_size, 
			opt2_L_altitude, 
			opt2_L_size,
			opt2_F_altitude,
			opt2_F_size,
			opt1_altitude,
			opt1_size,
			min_altitude,
			min_size
			), 
		names_to = "player", values_to = "value") %>% # rename all instances of opt2_L and opt2_F to opt2L and opt2F
	mutate(player = str_replace(player, "opt2_L", "opt2L")) %>%
	mutate(player = str_replace(player, "opt2_F", "opt2F")) %>% # Split player and outcome into two columns: player (containing the piece before the last "_") and outcome (containing the piece after the last "_")
	separate(player, c("player", "outcome"), sep = "_", extra = "merge")

# Take the outcome column and do pivot_wider to make it into two columns with values from value column
size_alt_scaling_long <- size_alt_scaling_long_intermediate %>%
	pivot_wider(names_from = outcome, values_from = value) %>%
	filter(player!="min") %>%
	mutate(
		player = case_when(
			player=="L" ~ "Duopoly\nLeader",
			player=="F" ~ "Duopoly\nFollower",
			player=="opt2L" ~ "Two\npublic utility\nconstellations\n(Larger)",
			player=="opt2F" ~ "Two\npublic utility\nconstellations\n(Smaller)",
			player=="opt1" ~ "One\npublic utility\nconstellation"
		)
		) #%>% filter(str_detect(player, "Duopoly")==FALSE)

# Now do the same as above but for total satellites

# add up the total number of satellites in each scenario
size_scaling_total_wide <- size_alt_scaling_wide %>%
	as_tibble() %>%
	group_by(params.atmo_damages) %>% # set the value in the altitude rows to 0 if the corresponding value in the size row for that group is small enough
	mutate(
		opt2_L = ifelse(opt2_L[outcome=="size"] < F[outcome=="size"], 0, opt2_L[outcome=="size"]), 
		opt2_F = ifelse(opt2_F[outcome=="size"] < F[outcome=="size"], 0, opt2_F[outcome=="size"]),
		opt1 = ifelse(opt1[outcome=="size"] < F[outcome=="size"], 0, opt1[outcome=="size"])
	) %>%
	filter(outcome!="altitude") %>%
	mutate(
		total_eqm = L + F,
		total_opt2 = opt2_L + opt2_F,
		total_opt1 = opt1,
		total_min = min
	) %>% select(params.atmo_damages, total_eqm, total_opt2, total_opt1, total_min) 

# reshape to a long dataset that has params.atmo_damages, scenario (from column names), and total (from table entries) as columns
size_scaling_total <- size_scaling_total_wide %>%
	pivot_longer(cols = c(total_eqm, total_opt2, total_opt1, total_min), names_to = "scenario", values_to = "total") %>%
	as_tibble() %>%
	mutate(
		scenario = case_when(
			scenario=="total_eqm" ~ "Duopoly",
			scenario=="total_opt2" ~ "Two\npublic utility\nconstellations",
			scenario=="total_opt1" ~ "One\npublic utility\nconstellation",
			scenario=="total_min" ~ "Minimum\ncoverage\nconstellation"
		)
		) %>% filter(scenario!="Minimum\ncoverage\nconstellation") %>% filter(scenario!="Minimum\ncoverage\nconstellation")

# take mean altitude in each scenario
alt_scaling_total_wide <- size_alt_scaling_wide %>%
	as_tibble() %>%
	group_by(params.atmo_damages) %>% # set the value in the altitude rows to 0 if the corresponding value in the size row for that group is small enough
	mutate(
		opt2_L = ifelse(opt2_L[outcome=="size"] < F[outcome=="size"], NA, opt2_L[outcome=="altitude"]), 
		opt2_F = ifelse(opt2_F[outcome=="size"] < F[outcome=="size"], NA, opt2_F[outcome=="altitude"]),
		opt1 = ifelse(opt1[outcome=="size"] < F[outcome=="size"], NA, opt1[outcome=="altitude"])
	) %>%
	filter(outcome!="size") %>%
	mutate(
		mean_eqm = (L + F)/2,
		# mean_opt2 = as.numeric(!is.na(opt2_F))*((opt2_L + opt2_F)/2) + as.numeric(is.na(opt2_F))*opt2_L,
		mean_opt2 = mean(c(opt2_L, opt2_F), na.rm=TRUE),
		mean_opt1 = opt1,
		mean_min = min
	) %>% select(params.atmo_damages, mean_eqm, mean_opt2, mean_opt1, mean_min) 

# reshape to a long dataset that has params.atmo_damages, scenario (from column names), and total (from table entries) as columns
alt_scaling_total <- alt_scaling_total_wide %>%
	pivot_longer(cols = c(mean_eqm, mean_opt2, mean_opt1, mean_min), names_to = "scenario", values_to = "mean") %>%
	as_tibble() %>%
	mutate(
		scenario = case_when(
			scenario=="mean_eqm" ~ "Duopoly",
			scenario=="mean_opt2" ~ "Two\npublic utility\nconstellations",
			scenario=="mean_opt1" ~ "One\npublic utility\nconstellation",
			scenario=="mean_min" ~ "Minimum\ncoverage\nconstellation"
		)
		) %>% filter(scenario!="Minimum\ncoverage\nconstellation") %>% filter(scenario!="Minimum\ncoverage\nconstellation")

#### Make plots

# plot of total number of satellites across scenario type
size_scaling_total_plot <- ggplot(data = 
	size_scaling_total,
	aes(x=params.atmo_damages, y=total, group=scenario, color=scenario)) +
	geom_line(linewidth=2.5) +
	theme_minimal() +
	labs(x="Environmental damages [$/sat]", y="Total number of satellites [sats]", title="", color="System\ntype") +
	scale_color_brewer(direction=-1,palette="Dark2") +
	scale_x_continuous(labels = scales::comma_format(scale = 1, big.mark = ",")) +
	theme(
		text = element_text(size=20, family="Arial"),
		legend.spacing.y =  unit(1, "cm"),
		legend.key.size = unit(1, 'lines'),
		legend.position = "bottom"
	)

# plot of total number of satellites across scenario type
alt_scaling_mean_plot <- ggplot(data = 
	alt_scaling_total,
	aes(x=params.atmo_damages, y=mean, group=scenario, color=scenario)) +
	geom_line(linewidth=2.5) +
	theme_minimal() +
	labs(x="Environmental damages [$/sat]", y="Mean altitude [sats]", title="", color="System\ntype") +
	scale_color_brewer(direction=-1,palette="Dark2") +
	scale_x_continuous(labels = scales::comma_format(scale = 1, big.mark = ",")) +
	theme(
		text = element_text(size=20, family="Arial"),
		legend.spacing.y =  unit(1, "cm"),
		legend.key.size = unit(1, 'lines'),
		legend.position = "bottom"
	)

ggsave(
	paste0("../../images/size-alt-scaling-atmo--total-mean-", RESULTS_FILENAME,".png"),
	(size_scaling_total_plot | alt_scaling_mean_plot) + 
		plot_annotation(tag_levels = 'A') + plot_layout(guides="collect") & theme(legend.position = 'bottom'),
	width = 12,
	height = 6,
	units = "in",
	dpi = 300
)


ggsave(
	paste0("../../images/size-scaling-atmo--total-mean-", RESULTS_FILENAME,".png"),
	(size_scaling_total_plot) + 
		plot_annotation(tag_levels = 'A') + plot_layout(guides="collect") & theme(legend.position = 'bottom'),
	width = 8,
	height = 6,
	units = "in",
	dpi = 300
)

# plots of altitude and size by constellation (not just overall system)
size_scaling_atmodamages <- ggplot(data = 
	size_alt_scaling_long, 
	aes(x=params.atmo_damages, group=player, color=player)) +
	geom_line(aes(y=size), linewidth=2.5) + 
	theme_minimal() +
	labs(x="Annualized environmental damages\n[$ / sat / year]", y="Constellation size [sats]", title="", color="System\ntype") +
	scale_color_brewer(direction=-1,palette="Dark2") +
	scale_x_continuous(labels = scales::comma_format(big.mark = ",")) +
	theme(
		text = element_text(size=20, family="Arial"),
		legend.spacing.y =  unit(1, "cm"),
        legend.key.size = unit(1, 'lines'),
		legend.position = "bottom"
	)

alt_scaling_atmodamages <- ggplot(data = 
	size_alt_scaling_long, 
	aes(x=params.atmo_damages, group=player, color=player)) +
	geom_line(aes(y=altitude), linewidth=2.5) + 
	theme_minimal() +
	labs(x="Annualized environmental damages\n[$ / sat / year]", y="Constellation altitude [km]", title="", color="System\ntype") +
	scale_color_brewer(direction=-1,palette="Dark2") +
	scale_x_continuous(labels = scales::comma_format(big.mark = ",")) +
	theme(
		text = element_text(size=20, family="Arial"),
		legend.spacing.y =  unit(1, "cm"),
        legend.key.size = unit(1, 'lines'),
		legend.position = "bottom"
	)

ggsave(
	paste0("../../images/size-alt-scaling-atmo-", RESULTS_FILENAME,".png")
	,
	(size_scaling_atmodamages | alt_scaling_atmodamages ) + 
		plot_annotation(tag_levels = 'A') + plot_layout(guides="collect") & theme(legend.position = 'bottom'),
	width = 11.4,
	height = 6,
	units = "in",
	dpi = 300
)

##### Welfare scaling

# The following snipper is a temporary fix for the report_qualities_2 function not including atmospheric damages in the welfare calculations. It should be removed once the function is fixed.
bp_scaling_long_full <- bp_scaling_long_full %>%
	group_by(params.atmo_damages) %>%
	mutate(
		L = ifelse(outcome=="total_surplus", L - params.atmo_damages*L[outcome=="size"]*1e-9, L)
	)

# test %>%
# 	select(outcome, params.atmo_damages, L, F, opt2_L, opt2_F, opt1, min) %>%
# 	filter(outcome=="total_surplus")

welfare_scaling_wide <- bp_scaling_long_full %>%
	select(outcome, params.atmo_damages, L, F, opt2_L, opt2_F, opt1, min) %>%
	filter(outcome=="total_surplus") %>%
	mutate(
		opt1_gain = opt1 - L,
		opt2_gain = opt2_L - L,
		opt1_gain_perc = abs(100*(opt1 - L)/L),
		opt2_gain_perc = abs(100*(opt2_L - L)/L)
	)

welfare_scaling_long_full <- welfare_scaling_wide %>%
	pivot_longer(cols = c(L, F, opt2_L, opt2_F, opt1, min, opt1_gain, opt2_gain, opt1_gain_perc, opt2_gain_perc), names_to = "player", values_to = "value")

welfare_scaling_long_raw <- welfare_scaling_long_full %>%
	filter(str_detect(player, "gain")==FALSE, player!="min") %>%
	drop_na() %>%
	mutate(
		player = case_when(
			player=="L" ~ "Duopoly",
			player=="opt1" ~ "One\npublic utility\nconstellation",
			player=="opt2_L" ~ "Two\npublic utility\nconstellations"
		)
		) %>%
	filter(player!="opt1")

welfare_scaling_long <- welfare_scaling_long_full %>%
	filter(str_detect(player, "gain")==TRUE) 

welfare_scaling <- ggplot(data = 
	welfare_scaling_long %>% filter(str_detect(player, "perc")==FALSE),
	aes(x=params.atmo_damages, group=player, color=player)) +
	geom_line(aes(y=value), linewidth=2.5) + 
	theme_minimal() +
	labs(x="Annualized environmental damages [$ / sat / year]", y="Annual total surplus gain [B $ / year]", title="Economic welfare gain from\npublic utility systems cf. duopoly", color="System\ntype") +
	scale_color_brewer(direction=-1,palette="Set2") +
	scale_x_continuous(labels = scales::comma_format(big.mark = ",")) +
	theme(
		text = element_text(size=30, family="Arial"),
		legend.spacing.y =  unit(1, "cm"),
        legend.key.size = unit(6, 'lines')
	)

welfare_scaling_perc <- ggplot(data = 
	welfare_scaling_long %>% filter(str_detect(player, "perc")==TRUE),
	aes(x=params.atmo_damages, group=player, color=player)) +
	geom_line(aes(y=value), linewidth=2.5) + 
	theme_minimal() +
	labs(x="Annualized environmental damages [$ / sat / year]", y="Annual total surplus gain [% / year]", title="Economic welfare gain from\npublic utility systems cf. duopoly", color="System\ntype") +
	scale_color_brewer(direction=-1,palette="Set2") +
	scale_x_continuous(labels = scales::comma_format(big.mark = ",")) +
	theme(
		text = element_text(size=25, family="Arial")
	)

# Get the colors used in the size-alt plot
size_alt_colors <- brewer.pal(name = "Set1", n = 3)#[c(1,3)]

welfare_scaling_raw <- ggplot(data = 
	welfare_scaling_long_raw %>%
	filter(player!="One\npublic utility\nconstellation"),
	aes(x=params.atmo_damages, group=player, color=player)) +
	geom_line(aes(y=value), linewidth=2.5, alpha = 2) + 
	theme_minimal() +
	geom_hline(aes(yintercept=0), linetype="dashed", linewidth=1.5) +
	labs(x="Annualized environmental damages [$ / sat / year]", y="Annual total surplus [B $ / year]", title="", color="") +
	scale_color_manual(values=size_alt_colors) +
	# scale_color_brewer(direction=-1,palette="Set2") +
	scale_x_continuous(labels = scales::comma_format(big.mark = ",")) +
	theme(
		text = element_text(size=20, family="Arial"),
		legend.spacing.y =  unit(1, "cm"),
        legend.key.size = unit(5, 'lines'),
		legend.position = "right"
	) + guides(color="none")

size_alt_scaling_long_total <- size_alt_scaling_long %>%
	group_by(params.atmo_damages) %>%
	mutate(
		scenario = case_when(
			player=="Duopoly\nLeader" ~ "Duopoly",
			player=="Duopoly\nFollower" ~ "Duopoly",
			player=="Two\npublic utility\nconstellations\n(Larger)" ~ "Two\npublic utility\nconstellations",
			player=="Two\npublic utility\nconstellations\n(Smaller)" ~ "Two\npublic utility\nconstellations",
			player=="One\npublic utility\nconstellation" ~ "One\npublic utility\nconstellation"
		)
		) %>% 
	group_by(params.atmo_damages, scenario) %>%
	mutate(
		size_total = sum(size)
	)


size_scaling_atmodamages2_base <- ggplot(data = 
	size_alt_scaling_long_total %>%
		filter(player!="One\npublic utility\nconstellation"), 
	aes(x=params.atmo_damages)) +
	theme_minimal() +
	theme(
		text = element_text(size=20, family="Arial"),
		legend.spacing.y =  unit(2, "cm"),
        legend.key.size = unit(5, 'lines'),
		legend.position = "right"
	)

size_scaling_atmodamages2_separate <- size_scaling_atmodamages2_base +
	scale_x_continuous(labels = scales::comma_format(big.mark = ",")) +
	geom_line(aes(y=size, group=player, color=player), linewidth=2.5) + 
	labs(x="", y="Constellation size [sats]", title="", color="") +
	scale_color_brewer(direction=-1,palette="Dark2")

size_scaling_atmodamages2_total <- size_scaling_atmodamages2_base +
	scale_x_continuous(labels = scales::comma_format(big.mark = ",")) +
	geom_line(aes(y=size_total, group=scenario, color=scenario), linewidth=2.5) + 
	labs(x="", y="Constellation size [sats]", title="", color="") +
	scale_color_manual(values=size_alt_colors)

# Save Figure 7
ggsave(
	paste0("../../images/MT-welfare-scaling-atmo-", RESULTS_FILENAME,"-total.png"),
	(size_scaling_atmodamages2_total / welfare_scaling_raw )  + 
		plot_annotation(tag_levels = 'A') + 
		plot_layout(guides = "collect"),
	width = 12,
	height = 12,
	units = "in",
	dpi = 300
)

## Additional figure
ggsave(
	paste0("../../images/MT-welfare-scaling-atmo-", RESULTS_FILENAME,".png"),
	(size_scaling_atmodamages2_separate / welfare_scaling_raw )  + 
		plot_annotation(tag_levels = 'A'),
	width = 12,
	height = 12,
	units = "in",
	dpi = 300
)
