# Script to make plots for PNAS constellations and competition paper

library(tidyverse)
library(patchwork)
library(viridis)
library(data.table)
library(jsonlite)

setwd("../")

source("functions.r") # Load standard functionset
source("calibration.r")

RESULTS_FILENAME <- "--N-radius-preference-sensitivity--REVISION--maxit-BIG"

setwd("plots")

#####
# What do we lose from duopoly / gain from regulation?
#####

bp_long <- read_csv("../../outputs/fig-data/barplot-data---benchmark-REVISIONS--cobbdouglas--newLinearGrid--tau-2.csv")

# Assign planner indifferent_theta to 1.5. This doesn't affect solutions.
bp_long[65, "value"] <- 1.5

# Convert indifferent_theta to demand level
bp_long <- bp_long %>%
	mutate(
		value = case_when(
			outcome=="indifferent_theta"&scenario=="Duopoly"&player=="Leader" ~ (1.5 - value)*params_element$N,
			outcome=="indifferent_theta"&scenario=="Duopoly"&player=="Follower" ~ (value-0.5)*params_element$N,
			outcome=="indifferent_theta"&scenario=="opt2"&player=="Leader" ~ (1.5-value)*params_element$N,
			outcome=="indifferent_theta"&scenario=="opt2"&player=="Follower" ~ (value-0.5)*params_element$N,
			outcome=="indifferent_theta"&scenario=="opt1" ~ (value-0.5)*params_element$N,
			TRUE ~ value
		)
	)

## Table of size, altitude of each constellation under each outcome. This makes the raw csv that will be edited to have the right names and formatting.
result_table_long <- bp_long %>%
	drop_na() %>%
	select(-...1) %>%
	filter(
		outcome=="altitude" | 
		outcome=="size" |
		outcome=="bandwidth" |
		outcome=="latency" |
		outcome=="coverage" | 
		outcome=="indifferent_theta")

### Make results_table_long wider. It should have columns for each scenario-player combination, and rows for each outcome. The values in the cells should be rounded to zero decimal places.
result_table_wide <- pivot_wider(result_table_long, names_from = c("scenario", "player"), values_from = "value") %>%
	mutate(
		Duopoly_Leader = round(Duopoly_Leader, 2),
		Duopoly_Follower = round(Duopoly_Follower, 2),
		opt2_Leader = round(opt2_Leader, 2),
		opt2_Follower = round(opt2_Follower, 2),
		opt1_Planner = round(opt1_Planner, 2)
	)

write_csv(result_table_wide, "../../outputs/fig-data/table-1.csv")

## Barplots of welfare and congestion under each outcome

welfare_bar <- bp_long %>% 
	filter(outcome=="total_surplus") %>%
	drop_na() %>%
	mutate(
		perc_change = 100*(value - value[scenario=="Duopoly"])/value[scenario=="Duopoly"],
		scenario = case_when(
			scenario=="Duopoly" ~ "Duopoly",
			scenario=="opt2" ~ "Two\npublic utility\nconstellations",
			scenario=="opt1" ~ "One\npublic utility\nconstellation",
			FALSE ~ "Other"
		)
	)

benchmark_barplot_welfare <- 
	ggplot(welfare_bar, aes(fill=scenario, x=reorder(scenario, value), y=value))+ 
	geom_col(position="dodge")  +
	geom_text(aes(label = round(value,1)), vjust=1.75, size=10) +
	# geom_label(aes(label = round(value,2), fill=NULL), size=8, label.padding = unit(0.25, "lines")) +
	theme_minimal() +
	labs(x="", y="Annual total surplus [Billion $]", title="Economic welfare", fill="Entity type") +
	scale_fill_brewer(palette="Pastel1") +
	theme(
		text = element_text(size=30, family="Arial"),
		axis.text.x = element_blank(),
        legend.key.size = unit(3, 'lines'),
		legend.position = "bottom"
	) + guides(fill="none")

total_sizes <- bp_long %>%
	filter(outcome=="size") %>%
	drop_na() %>%
	group_by(scenario, player) %>%
	mutate(
		own_size = value # since we only have own-congestion in eqm or opt, can ignore pair-size. want to get maneuvers/sat-hour units.
	) %>% select(-value, -...1, -outcome) %>%
	ungroup() %>% group_by(scenario) %>%
	mutate(
		size_share = own_size/sum(own_size)
	)

## New version: directly uses maneuvers/sat-day calculated earlier
congestion_bar <- bp_long %>% 
	filter(outcome=="maneuvers") %>%
	drop_na() %>%
	select(-...1) %>%
	left_join(total_sizes, by=c("player", "scenario")) %>%
	group_by(scenario) %>%
	summarize(
		outcome=outcome[1],
		value = sum(value)
	) %>%
	mutate(
		scenario = case_when(
			scenario=="Duopoly" ~ "Duopoly",
			scenario=="opt2" ~ "Two\npublic utility\nconstellations",
			scenario=="opt1" ~ "One\npublic utility\nconstellation"
		)
	)

benchmark_barplot_congestion <- 
	ggplot(congestion_bar, aes(fill=scenario, x=reorder(scenario, value), y=value))+ 
	geom_col(position="dodge")  +
	geom_text(aes(label = round(value,0)), vjust=1.75, size=10) +
	theme_minimal() +
	labs(x="", y="Total maneuver burden\n[maneuvers/day]", title="Orbital congestion", fill="System type") +
	scale_fill_brewer(palette="Pastel1") +
	theme(
		text = element_text(size=30, family="Arial"),
		axis.text.x = element_blank(),
        legend.key.size = unit(3, 'lines'),
		legend.position = "bottom"
	)

# Save Figure 2
ggsave(
	"../../images/benchmark-barplots.png",
	(benchmark_barplot_welfare | benchmark_barplot_congestion) + plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect") & theme(legend.position = 'bottom'),
	width = 24*1.75*0.5,
	height = 24*0.5,
	units = "in",
	dpi = 300)


## Tile plots of how welfare and congestion change with N, radius (safety margin)

bp_scaling_long_full <- fread(paste0("../../outputs/fig-data/variation-results",RESULTS_FILENAME,".csv"))

# Find grid nodes that are closest to the benchmark calibration
## N
closest_to_benchmark_N <- bp_scaling_long_full$params.N[which.min( (params_element$N - bp_scaling_long_full$params.N)^2 )]
## radius
closest_to_benchmark_radius <- bp_scaling_long_full$params.radius[which.min( (params_element$radius - bp_scaling_long_full$params.radius)^2 )]

### N-radius

# Reassignment -- vestigial
bp_scaling_long_Nradius <- bp_scaling_long_full

# Filter data and make some new variables
bp_scaling_welfare <- bp_scaling_long_Nradius %>% 
	filter(outcome=="total_surplus") %>%
	select(outcome, L, opt2_L, opt1, params.radius, params.N) %>%
	mutate(
		L = pmax(L,1),
		F = pmax(F,1),
		perc_loss_opt2 = (opt2_L - L)/L,
		perc_loss_opt1 = (opt1 - L)/L,
		raw_loss_opt2 = opt2_L - L,
		raw_loss_opt1 = opt1 - L,
		max_loss = pmax(raw_loss_opt2, raw_loss_opt1),
		perc_max_loss = 100*pmax(perc_loss_opt2, perc_loss_opt1),
		better_system = ifelse(opt2_L > opt1, "Two", "One")
	) %>%
	select(params.N, params.radius, perc_loss_opt2, perc_loss_opt1, raw_loss_opt2, raw_loss_opt1, max_loss, perc_max_loss, better_system)

bp_scaling_welfare <- bp_scaling_welfare %>% 
	filter(params.radius<0.65)

# Plots
bp_scaling_Nradius <- ggplot(data = bp_scaling_welfare, 
	aes(x=params.N/1e6, y=1000*params.radius, fill=max_loss)) +
	geom_tile() +
	geom_point(aes(x = closest_to_benchmark_N/1e6, y=1000*closest_to_benchmark_radius), size=5) + 
	theme_minimal() +
	labs(x="Market size [Million consumers]", y="Maneuver safety margin [m]", title="Welfare gain: optimal system\nrelative to duopoly", fill="[Billion $]") +
	scale_fill_distiller(palette="RdPu", direction=1) +
	theme(
		text = element_text(size=25, family="Arial")
	) +
  guides(fill = guide_legend(override.aes = list(shape = NA)))

bp_scaling_Nradius_perc <- ggplot(data = bp_scaling_welfare, 
	aes(x=params.N/1e6, y=1000*params.radius, fill=perc_max_loss)) +
	geom_tile() +
	geom_point(aes(x = closest_to_benchmark_N/1e6, y=1000*closest_to_benchmark_radius), size=5) + 
	theme_minimal() +
	labs(x="Market size [Million consumers]", y="Maneuver safety margin [m]", title="Economic welfare gain:\noptimal system cf. duopoly", fill="[% gain]") +
	scale_fill_distiller(palette="RdPu", direction=1, type="seq") +
	theme(
		text = element_text(size=25, family="Arial")
	) +
  guides(fill = guide_legend(override.aes = list(shape = NA)))

bp_bettersystem_Nradius <- ggplot(data = bp_scaling_welfare, 
	aes(x=params.N/1e6, y=1000*params.radius, fill=better_system)) +
	geom_tile() +
	geom_point(aes(x = closest_to_benchmark_N/1e6, y=1000*closest_to_benchmark_radius), size=5) + 
	theme_minimal() +
	labs(x="Market size [Million consumers]", y="Maneuver safety margin [m]", title="Public utility\nsystem type", fill="Number of\nconstellations") +
	scale_fill_brewer(palette="Dark2", direction=1) +
	theme(
		text = element_text(size=30, family="Arial"),
        legend.key.size = unit(3, 'lines'),
		legend.position = "bottom"
	) +
  guides(fill = guide_legend(override.aes = list(shape = NA)))

bp_scaling_congestion <- bp_scaling_long_Nradius %>% 
	filter(outcome=="avg_maneuvers") %>%
	mutate(
		total_duopoly = L,
		total_opt2 = opt2_L,
		total_opt1 = opt1
		) %>%
	select(outcome, total_duopoly, total_opt2, total_opt1, params.radius, params.N) %>%
	mutate(
		perc_loss_opt2 = (total_opt2 - total_duopoly)/total_duopoly,
		perc_loss_opt1 = (total_opt1 - total_duopoly)/total_duopoly,
		raw_loss_opt2 = total_opt2 - total_duopoly,
		raw_loss_opt1 = total_opt1 - total_duopoly,
		max_loss = pmax(raw_loss_opt2, raw_loss_opt1),
		perc_max_loss = pmax(perc_loss_opt2, perc_loss_opt1),
		better_system = ifelse(total_opt2 < total_opt1, "Two", "One")
	) %>%
	select(params.N, params.radius, perc_loss_opt2, perc_loss_opt1, raw_loss_opt2, raw_loss_opt1, max_loss, perc_max_loss, better_system)

bp_scaling_Nradius_congestion_perc <- ggplot(data = bp_scaling_congestion, 
	aes(x=params.N/1e6, y=1000*params.radius, fill=perc_max_loss)) +
	geom_tile() +
	geom_point(aes(x = closest_to_benchmark_N/1e6, y=1000*closest_to_benchmark_radius), size=5) + 
	theme_minimal() +
	labs(x="Market size [Million consumers]", y="", title="Congestion increase: optimal system\nrelative to duopoly", fill="[% change]") +
	scale_fill_distiller(palette="RdPu", direction=1) +
	theme(
		text = element_text(size=30, family="Arial"),
        legend.key.size = unit(3, 'lines'),
		legend.position = "bottom"
	) +
  guides(fill = guide_legend(override.aes = list(shape = NA)))

bp_scaling_Nradius_congestion <- ggplot(data = bp_scaling_congestion, 
	aes(x=params.N/1e6, y=1000*params.radius, fill=max_loss)) +
	geom_tile() +
	geom_point(aes(x = closest_to_benchmark_N/1e6, y=1000*closest_to_benchmark_radius), size=5) + 
	theme_minimal() +
	labs(x="Market size [Million consumers]", y="", title="Congestion increase: optimal system\nrelative to duopoly", fill="[maneuvers/day]") +
	scale_fill_distiller(palette="RdPu", direction=1) +
	theme(
		text = element_text(size=30, family="Arial"),
        legend.key.size = unit(3, 'lines'),
		legend.position = "bottom"
	) +
  guides(fill = guide_legend(override.aes = list(shape = NA)))

bp_bettersystem_Nradius_congestion <- ggplot(data = bp_scaling_congestion, 
	aes(x=params.N/1e6, y=1000*params.radius, fill=better_system)) +
	geom_tile() +
	geom_point(aes(x = closest_to_benchmark_N/1e6, y=1000*closest_to_benchmark_radius), size=5) + 
	theme_minimal() +
	labs(x="Market size [Million consumers]", y="", title="Congestion-minimizing\npublic utility system type", fill="Number of\nconstellations") +
	scale_fill_brewer(direction=1,palette="Dark2") +
	theme(
		text = element_text(size=30, family="Arial")
	) +
	guides(fill="none")


# Save Figure 5
ggsave(
	"../../images/sensitivity--N-radius-perc.png",
	(bp_scaling_Nradius_perc) + plot_layout(guides = "collect") & theme(legend.position = 'bottom'),
	width = 8.5,
	height = 8.5,
	units = "in",
	dpi = 300)

## Additional figures if you're curious
ggsave(
	"../../images/sensitivity--N-radius-all.png",
	(bp_scaling_Nradius | bp_scaling_Nradius_congestion) / (bp_scaling_Nradius_perc | bp_scaling_Nradius_congestion_perc) + plot_annotation(tag_levels = 'A') + plot_layout(guides="collect"),
	width = 24*(17.8/11.4)*0.75,
	height = 24*0.75,
	units = "in",
	dpi = 300)


ggsave(
	"../../images/sensitivity--N-radius.png",
	(bp_scaling_Nradius | bp_scaling_Nradius_perc) + plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect") & theme(legend.position = 'bottom'),
	width = 24*1.75*0.5,
	height = 24*0.5,
	units = "in",
	dpi = 300)



ggsave(
	"../../images/sensitivity--N-radius-raw.png",
	(bp_scaling_Nradius) + plot_layout(guides = "collect") & theme(legend.position = 'bottom'),
	width = 8.5,
	height = 8.5,
	units = "in",
	dpi = 300)