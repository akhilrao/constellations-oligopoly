# Script to make plots for PNAS constellations and competition paper. Additional appendix figures.

library(tidyverse)
library(patchwork)
library(viridis)
library(data.table)
library(jsonlite)

setwd("../")

source("functions.r") # Load standard functionset
source("calibration.r")

# use commandArgs to set the RESULTS_FILENAME variable

args <- commandArgs(trailingOnly = TRUE)

if(length(args)>0){
	RESULTS_FILENAME <- args[1]
}
if(length(args)==0){
	RESULTS_FILENAME <- "--preference-sensitivity--REVISION--empgrid2"
}

setwd("plots")

## Tile plots of how welfare and congestion change with S_bar, a

bp_scaling_long_full <- fread(paste0("../../outputs/fig-data/variation-results",RESULTS_FILENAME,".csv"))

# Find grid nodes that are closest to the benchmark calibration
## S_bar
closest_to_benchmark_S_bar <- bp_scaling_long_full$params.S_bar[which.min( (params_element$S_bar - bp_scaling_long_full$params.S_bar)^2 )]

closest_to_benchmark_a <- bp_scaling_long_full$params.a[which.min( (params_element$a - bp_scaling_long_full$params.a)^2 )]

### N-radius


### Line plots showing how optimal system sizes and altitudes scale with market size and radius

size_alt_scaling_wide <- bp_scaling_long_full %>%
	select(outcome, params.a, params.S_bar, L, F, opt2_L, opt2_F, opt1, min) %>%
	filter(outcome=="size" | outcome=="altitude")

# make size_alt_scaling_wide into long, with params.a, altitude, and size as three columns and the existing columns as rows
size_alt_scaling_long_intermediate <- size_alt_scaling_wide %>%
	pivot_longer(cols = c(L, F, opt2_L, opt2_F, opt1, min), names_to = "player", values_to = "value")

# Take the outcome column and do pivot_wider to make it into two columns with values from value column
size_alt_scaling_long <- size_alt_scaling_long_intermediate %>%
	# distinct() %>%
	pivot_wider(names_from = outcome, values_from = value) %>%
	filter(player!="min") %>%
	as_tibble() %>%
	mutate(
		player = case_when(
			player=="L" ~ "Duopoly\nLeader",
			player=="F" ~ "Duopoly\nFollower",
			player=="opt2_L" ~ "Two public utility systems\n(Larger constellation)",
			player=="opt2_F" ~ "Two public utility systems\n(Smaller constellation)",
			player=="opt1" ~ "One public utility system"
		)
		) %>% filter(str_detect(player, "Duopoly")==FALSE)

# size_alt_scaling_long_intermediate %>% distinct(params.a) %>% print(n=25)

# Now do the same as above but for total satellites

# add up the total number of satellites in each scenario
size_alt_scaling_total <- size_alt_scaling_wide %>%
	filter(outcome!="altitude") %>%
	mutate(
		total_eqm = L + F,
		total_opt2 = opt2_L + opt2_F,
		total_opt1 = opt1,
		total_min = min
	) %>% select(params.a, params.S_bar, total_eqm, total_opt2, total_opt1, total_min) 

# reshape to a long dataset that has params.a, params.S_bar, scenario (from column names), and total (from table entries) as columns
size_scaling_total <- size_alt_scaling_total %>%
	pivot_longer(cols = c(total_eqm, total_opt2, total_opt1, total_min), names_to = "scenario", values_to = "total") %>%
	as_tibble() %>%
	filter(scenario!="total_min") %>%
	mutate(
		scenario = case_when(
			scenario=="total_eqm" ~ "Duopoly",
			scenario=="total_opt2" ~ "Two\npublic utility\nconstellations",
			scenario=="total_opt1" ~ "One\npublic utility\nconstellation"
		)
		)

size_scaling_S_bar <- ggplot(data = 
	size_alt_scaling_long, 
	aes(x=params.S_bar, group=player, color=player)) +
	geom_line(aes(y=(size)), linewidth=2.5) + 
	theme_minimal() +
	labs(x="Preferred benchmark bandwidth [Mb/s]", y="Constellation size [sats]", title="", color="System\ntype") +
	scale_color_brewer(direction=-1,palette="Dark2") +
	theme(
		text = element_text(size=30, family="Arial"),
		legend.spacing.y =  unit(1, "cm"),
        legend.key.size = unit(6, 'lines'),
		legend.position = "bottom"
	)

alt_scaling_S_bar <- ggplot(data = 
	size_alt_scaling_long, 
	aes(x=params.S_bar, group=player, color=player)) +
	geom_line(aes(y=altitude), linewidth=2.5) + 
	theme_minimal() +
	labs(x="Preferred benchmark bandwidth [Mb/s]", y="Constellation altitude [km]", title="", color="System\ntype") +
	scale_color_brewer(direction=-1,palette="Dark2") +
	theme(
		text = element_text(size=30, family="Arial"),
		legend.spacing.y =  unit(1, "cm"),
        legend.key.size = unit(6, 'lines'),
		legend.position = "bottom"
	)


size_scaling_S_bar_total <- ggplot(data = 
	size_scaling_total, 
	aes(x=params.S_bar, group=scenario, color=scenario)) +
	geom_line(aes(y=(total)), linewidth=2.5) + 
	theme_minimal() +
	labs(x="Preferred benchmark bandwidth [Mb/s]", y="Constellation size [sats]", title="Total fleet size and bandwidth preference", color="System\ntype") +
	scale_color_brewer(direction=-1,palette="Dark2") +
	theme(
		text = element_text(size=30, family="Arial"),
		legend.spacing.y =  unit(1, "cm"),
        legend.key.size = unit(6, 'lines'),
		legend.position = "bottom"
	)


ggsave(
	paste0("../../images/size-scaling--S_bar", RESULTS_FILENAME,".png"),
	(size_scaling_S_bar | alt_scaling_S_bar) + plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect") & theme(legend.position = 'bottom'),
	width = 12*2,
	height = 12,
	units = "in",
	dpi = 300)

ggsave(
	paste0("../../images/sensitivity--S_bar-total", RESULTS_FILENAME,".png"),
	(size_scaling_S_bar_total) + plot_layout(guides = "collect") & theme(legend.position = 'bottom'),
	width = 16,
	height = 12,
	units = "in",
	dpi = 300)


size_scaling_a <- ggplot(data = 
	size_alt_scaling_long, 
	aes(x=params.a, group=player, color=player)) +
	geom_line(aes(y=(size)), linewidth=2.5) + 
	geom_vline(xintercept=9, linetype="dashed", linewidth=2.5) +
	theme_minimal() +
	labs(x="Latency preference parameter [$/ms]", y="Constellation size [sats]", title="", color="System\ntype") +
	scale_color_brewer(direction=-1,palette="Dark2") +
	theme(
		text = element_text(size=30, family="Arial"),
		legend.spacing.y =  unit(1, "cm"),
        legend.key.size = unit(6, 'lines'),
		legend.position = "bottom"
	)

alt_scaling_a <- ggplot(data = 
	size_alt_scaling_long, 
	aes(x=params.a, group=player, color=player)) +
	geom_line(aes(y=altitude), linewidth=2.5) + 
	geom_vline(xintercept=9, linetype="dashed", linewidth=2.5) +
	theme_minimal() +
	labs(x="Latency preference parameter  [$/ms]", y="Constellation altitude [km]", title="", color="System\ntype") +
	scale_color_brewer(direction=-1,palette="Dark2") +
	theme(
		text = element_text(size=30, family="Arial"),
		legend.spacing.y =  unit(1, "cm"),
        legend.key.size = unit(6, 'lines'),
		legend.position = "bottom"
	)


size_scaling_a_total <- ggplot(data = 
	size_scaling_total, 
	aes(x=params.a, group=scenario, color=scenario)) +
	geom_line(aes(y=(total)), linewidth=2.5) + 
	geom_vline(xintercept=9, linetype="dashed", linewidth=2.5) +
	theme_minimal() +
	labs(x="Latency preference parameter  [$/ms]", y="Constellation size [sats]", title="Total fleet size and bandwidth preference", color="System\ntype") +
	scale_color_brewer(direction=-1,palette="Dark2") +
	theme(
		text = element_text(size=30, family="Arial"),
		legend.spacing.y =  unit(1, "cm"),
        legend.key.size = unit(6, 'lines'),
		legend.position = "bottom"
	)


ggsave(
	paste0("../../images/size-scaling--a", RESULTS_FILENAME,".png"),
	(size_scaling_a | alt_scaling_a) + plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect") & theme(legend.position = 'bottom'),
	width = 12*2,
	height = 12,
	units = "in",
	dpi = 300)

ggsave(
	paste0("../../images/sensitivity--a-total", RESULTS_FILENAME,".png"),
	(size_scaling_a_total) + plot_layout(guides = "collect") & theme(legend.position = 'bottom'),
	width = 16,
	height = 12,
	units = "in",
	dpi = 300)

