# Script to make plots for PNAS constellations and competition paper. This produces the following figures: 3, 4, 6.

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

if(length(args)>0){
	RESULTS_FILENAME <- args[1]
}
if(length(args)==0){
	RESULTS_FILENAME <- "--N-radius-preference-sensitivity--REVISION--maxit-BIG"
}
setwd("plots")

#####
# Why does this happen?
#####

# Read in the data. Convert L and F columns to numeric.
bp_scaling_long_full <- fread(paste0("../../outputs/fig-data/variation-results",RESULTS_FILENAME,".csv"))

bp_scaling_long_full %>% filter(params.N == max(params.N))

# Find grid nodes that are closest to the benchmark calibration
## N
closest_to_benchmark_N <- bp_scaling_long_full$params.N[which.min( (params_element$N - bp_scaling_long_full$params.N)^2 )]
## radius
closest_to_benchmark_radius <- bp_scaling_long_full$params.radius[which.min( (params_element$radius - bp_scaling_long_full$params.radius)^2 )]

### N-radius

# Reassign -- vestigial
bp_scaling_long_Nradius <- bp_scaling_long_full %>% filter(params.N>=2.5e6)

bp_scaling_long_full %>% select(outcome, L) %>% filter(outcome=="altitude")

### Line plots showing how optimal system sizes and altitudes scale with market size and radius

size_alt_scaling_wide <- bp_scaling_long_Nradius %>%
	select(outcome, params.N, params.radius, L, F, opt2_L, opt2_F, opt1, min) %>%
	filter(outcome=="size" | outcome=="altitude")

size_alt_scaling_wide %>% filter(outcome=="altitude", params.N==closest_to_benchmark_N) %>% select(opt2_L) %>% unlist() %>% as.numeric()


# Spread size_alt_scaling_wide wider on outcome. Create new columns for each level of outcome.
size_alt_scaling_wider <- size_alt_scaling_wide %>%
	pivot_wider(names_from = outcome, values_from = c(L, F, opt2_L, opt2_F, opt1, min))

# Set entries in columns of size_alt_scaling_wider with _altitude in the name to NA if the corresponding entry in the _size column is <1e-6. E.g. if L_size < 1e-6, set L_altitude to NA.
size_alt_scaling_wider <- size_alt_scaling_wider %>%
	group_by(params.N, params.radius) %>%
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
	as_tibble() %>%
	mutate(
		player = case_when(
			player=="L" ~ "Duopoly\nLeader",
			player=="F" ~ "Duopoly\nFollower",
			player=="opt2L" ~ "Two\npublic utility\nconstellations\n(Larger)",
			player=="opt2F" ~ "Two\npublic utility\nconstellations\n(Smaller)",
			player=="opt1" ~ "One\npublic utility\nconstellation"
		)
		) %>% filter(str_detect(player, "Duopoly")==FALSE) %>% filter(str_detect(player, "min")==FALSE)


# Now do the same as above but for total satellites

# add up the total number of satellites in each scenario
size_scaling_total_wide <- size_alt_scaling_wide %>%
	as_tibble() %>%
	filter(outcome!="altitude") %>%
	mutate(
		total_eqm = L + F,
		total_opt2 = opt2_L + opt2_F,
		total_opt1 = opt1,
		total_min = min
	) %>% select(params.N, params.radius, total_eqm, total_opt2, total_opt1, total_min) 

# reshape to a long dataset that has params.N, params.radius, scenario (from column names), and total (from table entries) as columns
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
		)

# take mean altitude in each scenario
alt_scaling_total_wide <- size_alt_scaling_wide %>%
	filter(outcome!="size") %>%
	mutate(
		mean_eqm = (L + F)/2,
		mean_opt2 = (opt2_L + opt2_F)/2,
		mean_opt1 = opt1,
		mean_min = min
	) %>% select(params.N, params.radius, mean_eqm, mean_opt2, mean_opt1, mean_min) 

# reshape to a long dataset that has params.N, params.radius, scenario (from column names), and total (from table entries) as columns
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
		)

size_scaling_N <- ggplot(data = 
	size_alt_scaling_long %>% filter(params.radius==closest_to_benchmark_radius), 
	aes(x=params.N/1e6, group=player, color=player)) +
	geom_line(aes(y=(size)), linewidth=2.5) + 
	geom_vline(aes(xintercept=closest_to_benchmark_N/1e6), linetype="dashed", size=1) +
	theme_minimal() +
	labs(x="Market size [Million consumers]", y="Constellation size [sats]", title="", color="System\ntype") +
	scale_color_brewer(direction=-1,palette="Dark2") +
	theme(
		text = element_text(size=20, family="Arial"),
		legend.spacing.y =  unit(1, "cm"),
        legend.key.size = unit(1, 'lines'),
		legend.position = "bottom"
	)

alt_scaling_N <- ggplot(data = 
	size_alt_scaling_long %>% filter(params.radius==closest_to_benchmark_radius), 
	aes(x=params.N/1e6, group=player, color=player)) +
	geom_line(aes(y=altitude), linewidth=2.5) + 
	geom_vline(aes(xintercept=closest_to_benchmark_N/1e6), linetype="dashed", size=1) +
	theme_minimal() +
	labs(x="Market size [Million consumers]", y="Constellation altitude [km]", title="", color="System\ntype") +
	scale_color_brewer(direction=-1,palette="Dark2") +
	theme(
		text = element_text(size=20, family="Arial"),
		legend.spacing.y =  unit(1, "cm"),
        legend.key.size = unit(1, 'lines'),
		legend.position = "bottom"
	)

size_scaling_radius <- ggplot(data = 
	size_alt_scaling_long %>% filter(params.N==closest_to_benchmark_N), 
	aes(x=1000*params.radius, group=player, color=player)) +
	geom_line(aes(y=(size)), linewidth=2.5) + 
	geom_vline(aes(xintercept=closest_to_benchmark_radius*1000), linetype="dashed", size=1) +
	theme_minimal() +
	labs(x="Maneuver safety margin [m]", y="Constellation size [sats]", title="", color="System\ntype") +
	scale_color_brewer(direction=-1,palette="Dark2") +
	theme(
		text = element_text(size=20, family="Arial"),
		legend.spacing.y =  unit(1, "cm"),
        legend.key.size = unit(1, 'lines'),
		legend.position = "bottom"
	)

alt_scaling_radius <- ggplot(data = 
	size_alt_scaling_long %>% filter(params.N==closest_to_benchmark_N), 
	aes(x=1000*params.radius, group=player, color=player)) +
	geom_line(aes(y=altitude), linewidth=2.5) + 
	geom_vline(aes(xintercept=closest_to_benchmark_radius*1000), linetype="dashed", size=1) +
	theme_minimal() +
	labs(x="Maneuver safety margin [m]", y="Constellation altitude [km]", title="", color="System\ntype") +
	scale_color_brewer(direction=-1,palette="Dark2") +
	theme(
		text = element_text(size=20, family="Arial"),
		legend.spacing.y =  unit(1, "cm"),
        legend.key.size = unit(1, 'lines'),
		legend.position = "bottom"
	)

# Save Figure 3
ggsave(
	paste0("../../images/size-alt-scaling-", RESULTS_FILENAME,".png"),
	((size_scaling_N | size_scaling_radius) / (alt_scaling_N | alt_scaling_radius)) + 
		plot_annotation(tag_levels = 'A') + plot_layout(guides="collect") & theme(legend.position = 'bottom'),
	width = 11.4,
	height = 11.4,
	units = "in",
	dpi = 300
)

### Plot totals and means. First, left_join size_scaling_total and alt_scaling_total by params.N, params.radius, and scenario. Then, rename the mean column to "altitude" and the total column to "size" and the scenario column to "player".

size_alt_scaling_long_meantotal <- left_join(size_scaling_total, alt_scaling_total, by = c("params.N", "params.radius", "scenario")) %>%
	as_tibble() %>%
	mutate(
		altitude = mean,
		size = total,
		player = scenario
		) %>%
	select(params.N, params.radius, player, altitude, size) %>%
	as_tibble() %>% filter(str_detect(player, "Minimum\ncoverage\nconstellation")==FALSE)

size_scaling_N_mean <- ggplot(data = 
	size_alt_scaling_long_meantotal %>% filter(params.radius==closest_to_benchmark_radius), 
	aes(x=params.N/1e6, group=player, color=player)) +
	geom_line(aes(y=(size)), linewidth=2.5) + 
	geom_vline(aes(xintercept=closest_to_benchmark_N/1e6), linetype="dashed", size=1) +
	theme_minimal() +
	labs(x="Market size [Million consumers]", y="Constellation size [sats]", title="", color="System\ntype") +
	scale_color_brewer(direction=-1,palette="Dark2") +
	theme(
		text = element_text(size=20, family="Arial"),
		legend.spacing.y =  unit(1, "cm"),
        legend.key.size = unit(1, 'lines'),
		legend.position = "bottom"
	)

alt_scaling_N_mean <- ggplot(data = 
	size_alt_scaling_long_meantotal %>% filter(params.radius==closest_to_benchmark_radius), 
	aes(x=params.N/1e6, group=player, color=player)) +
	geom_line(aes(y=altitude), linewidth=2.5) + 
	geom_vline(aes(xintercept=closest_to_benchmark_N/1e6), linetype="dashed", size=1) +
	theme_minimal() +
	labs(x="Market size [Million consumers]", y="Constellation altitude [km]", title="", color="System\ntype") +
	scale_color_brewer(direction=-1,palette="Dark2") +
	theme(
		text = element_text(size=20, family="Arial"),
		legend.spacing.y =  unit(1, "cm"),
        legend.key.size = unit(1, 'lines'),
		legend.position = "bottom"
	)

size_scaling_radius_mean <- ggplot(data = 
	size_alt_scaling_long_meantotal %>% filter(params.N==closest_to_benchmark_N), 
	aes(x=1000*params.radius, group=player, color=player)) +
	geom_line(aes(y=(size)), linewidth=2.5) + 
	geom_vline(aes(xintercept=closest_to_benchmark_radius*1000), linetype="dashed", size=1) +
	theme_minimal() +
	labs(x="Maneuver safety margin [m]", y="Constellation size [sats]", title="", color="System\ntype") +
	scale_color_brewer(direction=-1,palette="Dark2") +
	theme(
		text = element_text(size=20, family="Arial"),
		legend.spacing.y =  unit(1, "cm"),
        legend.key.size = unit(1, 'lines'),
		legend.position = "bottom"
	)

alt_scaling_radius_mean <- ggplot(data = 
	size_alt_scaling_long_meantotal %>% filter(params.N==closest_to_benchmark_N), 
	aes(x=1000*params.radius, group=player, color=player)) +
	geom_line(aes(y=altitude), linewidth=2.5) + 
	geom_vline(aes(xintercept=closest_to_benchmark_radius*1000), linetype="dashed", size=1) +
	theme_minimal() +
	labs(x="Maneuver safety margin [m]", y="Constellation altitude [km]", title="", color="System\ntype") +
	scale_color_brewer(direction=-1,palette="Dark2") +
	theme(
		text = element_text(size=20, family="Arial"),
		legend.spacing.y =  unit(1, "cm"),
        legend.key.size = unit(1, 'lines'),
		legend.position = "bottom"
	)

ggsave(
	paste0("../../images/size-alt-scaling-", RESULTS_FILENAME,"-mean-total.png"),
	((size_scaling_N_mean | size_scaling_radius_mean) / (alt_scaling_N_mean | alt_scaling_radius_mean)) + 
		plot_annotation(tag_levels = 'A') + plot_layout(guides="collect") & theme(legend.position = 'bottom'),
	width = 11.4,
	height = 11.4,
	units = "in",
	dpi = 300
)


### Line plots showing how optimal system qualities and availabilities scale with market size and radius. If you're curious.

quality_availability_scaling_wide <- bp_scaling_long_full %>%
	select(outcome, params.N, params.radius, L, F, opt2_L, opt2_F, opt1, min) %>%
	filter(outcome=="available_quality" | outcome=="congestion")

# make size_alt_scaling_wide into long, with params.N, altitude, and size as three columns and the existing columns as rows
quality_availability_scaling_long_intermediate <- quality_availability_scaling_wide %>%
	pivot_longer(cols = c(L, F, opt2_L, opt2_F, opt1, min), names_to = "player", values_to = "value")

# Take the outcome column and do pivot_wider to make it into two columns with values from value column
quality_availability_scaling_long <- quality_availability_scaling_long_intermediate %>%
	pivot_wider(names_from = outcome, values_from = value) %>%
	filter(player!="min") %>%
	as_tibble() %>%
	mutate(
		player = case_when(
			player=="L" ~ "Duopoly\nLeader",
			player=="F" ~ "Duopoly\nFollower",
			player=="opt2_L" ~ "Opt2\nLarger",
			player=="opt2_F" ~ "Opt2\nSmaller",
			player=="opt1" ~ "Opt1"
		)
		)

available_quality_scaling_N <- ggplot(data = 
	quality_availability_scaling_long %>% filter(params.radius==closest_to_benchmark_radius), 
	aes(x=params.N/1e6, group=player, color=player)) +
	geom_line(aes(y=available_quality), linewidth=2.5) + 
	geom_vline(aes(xintercept=closest_to_benchmark_N/1e6), linetype="dashed", size=1) +
	theme_minimal() +
	labs(x="Market size [Million consumers]", y="Constellation available_quality [$]", title="", color="System\ntype") +
	scale_color_brewer(direction=-1,palette="Dark2") +
	theme(
		text = element_text(size=30, family="Arial"),
		legend.spacing.y =  unit(1, "cm"),
        legend.key.size = unit(6, 'lines'),
		legend.position = "bottom"
	) + guides(color = "none")

cong_scaling_N <- ggplot(data = 
	quality_availability_scaling_long %>% filter(params.radius==closest_to_benchmark_radius), 
	aes(x=params.N/1e6, group=player, color=player)) +
	geom_line(aes(y=congestion), linewidth=2.5) + 
	geom_vline(aes(xintercept=closest_to_benchmark_N/1e6), linetype="dashed", size=1) +
	theme_minimal() +
	labs(x="Market size [Million consumers]", y="Constellation congestion [%]", title="", color="System\ntype") +
	scale_color_brewer(direction=-1,palette="Dark2") +
	theme(
		text = element_text(size=30, family="Arial"),
		legend.spacing.y =  unit(1, "cm"),
        legend.key.size = unit(6, 'lines'),
		legend.position = "bottom"
	) + guides(color = "none")

available_quality_scaling_radius <- ggplot(data = 
	quality_availability_scaling_long %>% filter(params.N==closest_to_benchmark_N), 
	aes(x=1000*params.radius, group=player, color=player)) +
	geom_line(aes(y=available_quality), linewidth=2.5) + 
	geom_vline(aes(xintercept=closest_to_benchmark_radius*1000), linetype="dashed", size=1) +
	theme_minimal() +
	labs(x="Maneuver safety margin [m]", y="Constellation available_quality [$]", title="", color="System\ntype") +
	scale_color_brewer(direction=-1,palette="Dark2") +
	theme(
		text = element_text(size=30, family="Arial"),
		legend.spacing.y =  unit(1, "cm"),
        legend.key.size = unit(6, 'lines'),
		legend.position = "bottom"
	) + guides(color = "none")

cong_scaling_radius <- ggplot(data = 
	quality_availability_scaling_long %>% filter(params.N==closest_to_benchmark_N), 
	aes(x=1000*params.radius, group=player, color=player)) +
	geom_line(aes(y=congestion), linewidth=2.5) + 
	geom_vline(aes(xintercept=closest_to_benchmark_radius*1000), linetype="dashed", size=1) +
	theme_minimal() +
	labs(x="Maneuver safety margin [m]", y="Constellation congestion [%]", title="", color="System\ntype") +
	scale_color_brewer(direction=-1,palette="Dark2") +
	theme(
		text = element_text(size=30, family="Arial"),
		legend.spacing.y =  unit(1, "cm"),
        legend.key.size = unit(6, 'lines'),
		legend.position = "bottom"
	)

ggsave(
	paste0(
	"../../images/available_quality-alt-scaling", RESULTS_FILENAME,".png"),
	((available_quality_scaling_N | available_quality_scaling_radius) / (cong_scaling_N | cong_scaling_radius)) + 
		plot_annotation(tag_levels = 'A') + plot_layout(guides="collect") & theme(legend.position = 'bottom'),
	width = 24*(17.8/11.4)*0.8,
	height = 24*0.8,
	units = "in",
	dpi = 300
)

ggsave(
	paste0(
	"../../images/available_quality-scaling-small", RESULTS_FILENAME,".png"),
	(available_quality_scaling_N / available_quality_scaling_radius) + 
		plot_annotation(tag_levels = 'A') + plot_layout(guides="collect") & theme(legend.position = 'bottom'),
	width = 24*(17.8/11.4)*0.8,
	height = 24*0.8,
	units = "in",
	dpi = 300
)


### Line plots showing how optimal system total surplus scales with market size and radius

welfare_scaling_wide <- bp_scaling_long_full %>%
	select(outcome, params.N, params.radius, L, F, opt2_L, opt2_F, opt1, min) %>%
	filter(outcome=="total_surplus") %>%
	mutate(
		opt1_gain = opt1 - L,
		opt2_gain = opt2_L - L,
		opt1_gain_perc = 100*(opt1 - L)/L,
		opt2_gain_perc = 100*(opt2_L - L)/L
	)

# make size_alt_scaling_wide into long, with params.N, altitude, and size as three columns and the existing columns as rows
welfare_scaling_long_intermediate <- welfare_scaling_wide %>%
	pivot_longer(cols = c(L, F, opt2_L, opt2_F, opt1, min, opt1_gain, opt2_gain, opt1_gain_perc, opt2_gain_perc), names_to = "player", values_to = "value")

# Take the outcome column and do pivot_wider to make it into two columns with values from value column. 
# radius VERSION: raw values differentiated along radius axis
welfare_scaling_long_radius <- welfare_scaling_long_intermediate %>%
	pivot_wider(names_from = outcome, values_from = value) %>%
	filter(player!="min", params.N == closest_to_benchmark_N) %>%
	as_tibble() %>%
	mutate(
		player = case_when(
			player=="L" ~ "Duopoly\nconstellations",
			player=="opt2_L" ~ "Two\npublic utility\nconstellations",
			player=="opt1" ~ "One\npublic utility\nconstellation"
		)
		) %>% drop_na() %>%
		group_by(player) %>%
		# arrange(-params.radius) %>%
		mutate(
			marginal_TS = c(-diff(total_surplus)/diff(params.radius), NA)
			)

# N VERSION: % change values.
welfare_scaling_long_base <- welfare_scaling_long_intermediate %>%
	pivot_wider(names_from = outcome, values_from = value) %>%
	filter(str_detect(player, "gain")==TRUE) 
	
welfare_scaling_long <- welfare_scaling_long_base %>%
	filter(str_detect(player, "perc")==FALSE) %>%
	as_tibble() %>%
	mutate(
		player = case_when(
			str_detect(player, "opt2") ~ "Two\npublic utility\nconstellations",
			str_detect(player, "opt1") ~ "One\npublic utility\nconstellation"
		)
		) %>% drop_na()

welfare_scaling_long_perc <- welfare_scaling_long_base %>%
	filter(str_detect(player, "perc")==TRUE) %>%
	as_tibble() %>%
	mutate(
		player = case_when(
			str_detect(player, "opt2") ~ "Two\npublic utility\nconstellations",
			str_detect(player, "opt1") ~ "One\npublic utility\nconstellation"
		)
		) %>% drop_na() %>% filter(total_surplus > 0)

size_alt_colors <- brewer.pal(name = "Set1", n = 3)[c(2,3)]

welfare_scaling_N <- ggplot(data = 
	welfare_scaling_long %>% filter(params.radius==closest_to_benchmark_radius), 
	aes(x=params.N/1e6, group=player, color=player)) +
	geom_line(aes(y=total_surplus), linewidth=2.5) + 
	geom_vline(aes(xintercept=10), linetype="dashed", size=1) +
	theme_minimal() +
	labs(x="Market size [Million consumers]", y="Annual total surplus [B $]", title="Economic welfare gain from\npublic utility systems cf. duopoly", color="System\ntype") +
	# scale_color_brewer(palette="Set1") +
	scale_color_manual(values=size_alt_colors) +
	theme(
		text = element_text(size=30, family="Arial"),
		legend.spacing.y =  unit(1, "cm"),
        legend.key.size = unit(6, 'lines')
	)

welfare_scaling_N_perc <- ggplot(data = 
	welfare_scaling_long_perc %>% filter(params.radius==closest_to_benchmark_radius), 
	aes(x=params.N/1e6, group=player, color=player)) +
	geom_line(aes(y=total_surplus), linewidth=2.5) + 
	geom_vline(aes(xintercept=10), linetype="dashed", size=1) +
	theme_minimal() +
	labs(x="Market size [Million consumers]", y="Annual total surplus [%]", title="Economic welfare gain from\npublic utility systems cf. duopoly", color="System\ntype") +
	# scale_color_brewer(palette="Set1") +
	scale_color_manual(values=size_alt_colors) +
	theme(
		text = element_text(size=25, family="Arial")
	)


welfare_scaling_radius <- ggplot(data = 
	welfare_scaling_long_radius, 
	aes(x=1000*params.radius, group=player, color=player)) +
	geom_line(aes(y=total_surplus), linewidth=2.5) +  
	theme_minimal() +
	geom_vline(aes(xintercept=closest_to_benchmark_radius*1000), linetype="dashed", size=1) +
	labs(x="Maneuver safety margin [m]", y="Annual total surplus [B $]", title="TC of higher safety margin (?)\nTB of lower safety margin (?)", color="System\ntype") +
	scale_color_brewer(direction=-1,palette="Dark2") +
	theme(
		text = element_text(size=25, family="Arial")
	)


welfare_scaling_radius_marginal <- ggplot(data = 
	welfare_scaling_long_radius ,#%>% 
		# filter(player!="Duopoly\nconstellations"), 
	aes(x=1000*params.radius, group=player, color=player)) +
	geom_line(aes(y=marginal_TS), linewidth=2.5) + 
	geom_vline(aes(xintercept=closest_to_benchmark_radius*1000), linetype="dashed", size=1) +
	theme_minimal() +
	labs(x="Maneuver safety margin [m]", y="Annual total surplus loss [M $/m]", title="Marginal cost of higher safety margin", color="System\ntype") +
	scale_color_brewer(palette="Set1") +
	# scale_color_manual(values=size_alt_colors) +
	theme(
		text = element_text(size=30, family="Arial"),
		legend.spacing.y =  unit(1, "cm"),
        legend.key.size = unit(3, 'lines')
	)

# Save Figure 4
ggsave(
	paste0(
	"../../images/welfare-scaling-lines--perc", RESULTS_FILENAME,".png"),
	( welfare_scaling_N_perc) + 
		plot_layout(guides = "collect") & theme(legend.position = 'bottom'),
	width = 10,
	height = 8.5,
	units = "in",
	dpi = 300
)

# Save Figure 6
ggsave(
	paste0(
	"../../images/MC-higher-safety", RESULTS_FILENAME,".png"),
	(welfare_scaling_radius_marginal) + 
		plot_layout(guides = "collect") & theme(legend.position = 'bottom'),
	width = 12,
	height = 12,
	units = "in",
	dpi = 300
)

## Additional figures. If you're curious.
ggsave(
	paste0(
	"../../images/MC-higher-safety-with-total", RESULTS_FILENAME,".png"),
	(welfare_scaling_radius / welfare_scaling_radius_marginal) + 
		plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect") & theme(legend.position = 'bottom'),
	width = 24*1.75*0.5,
	height = 24,
	units = "in",
	dpi = 300
)


ggsave(
	paste0(
	"../../images/welfare-scaling-lines", RESULTS_FILENAME,".png"),
	(welfare_scaling_N | welfare_scaling_radius) + 
		plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect") & theme(legend.position = 'bottom'),
	# width = 24*(17.8/11.4)*0.75,
	width = 24*1.75*0.5,
	height = 24*0.5,
	units = "in",
	dpi = 300
)


ggsave(
	paste0(
	"../../images/welfare-scaling-lines--raw", RESULTS_FILENAME,".png"),
	(welfare_scaling_N ) + 
		plot_layout(guides = "collect") & theme(legend.position = 'bottom'),
	width = 10,
	height = 8.5,
	units = "in",
	dpi = 300
)
