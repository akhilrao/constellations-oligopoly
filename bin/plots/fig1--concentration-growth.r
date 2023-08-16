# Script to make Figure 1 for PNAS constellations and competition paper.

library(tidyverse)
library(patchwork)
library(viridis)
library(data.table)

#####
# Orbital concentration is rising in LEO
#####

# This dataset comes from combining the UCS sheets of active satellites with JSpOC data on object counts. Compiled by Ethan Berner and Gordon Lewis.
OU_data <- read_csv("../data/ucs-jspoc-soy-panel-2022.csv") %>%
			group_by(Year) %>%
			mutate(median_count_entity = median(Sat_Count_entity)) 

avg_fleet_size <- ggplot(data = OU_data %>% filter(Year>=2000), aes(x=Year)) +
	geom_ribbon(aes(ymin=pmax(mean_count_entity-sd_count_entity,0),ymax=mean_count_entity+sd_count_entity), alpha = 0.15) + 
	geom_line(aes(y=mean_count_entity, linetype="Mean"), linewidth = 1.5) + 
	geom_line(aes(y=median_count_entity, linetype="Median"), linewidth = 1.5) + 
	theme_minimal() + labs(x="Year", y="Satellites", title="Mean & median fleet sizes", linetype="Fleet sizes") +
	theme(
		text = element_text(size=30, family="Arial"),
		legend.spacing.y =  unit(0.25, "cm"),
        legend.key.size = unit(3, 'lines')
	)

hhi_cutoff <- quantile(
	OU_data %>% 
	filter(mean_altitude <= 1500, mean_altitude >=250,Year>=2000) %>% pull(total_sats)
	, probs=c(0.10))[[1]]

orbit_HHI_scaled <- ggplot(data = OU_data %>% 
	filter(mean_altitude <= 1500, mean_altitude >=250,Year>=2000) %>% ungroup() %>%
	mutate(HHI = ifelse(total_sats > hhi_cutoff, HHI, 0)), aes(x=Year)
	) + 
	geom_tile(aes(y=reorder(shell_index_2, mean_altitude), fill=HHI)) + 
	scale_fill_fermenter(type="seq", palette="OrRd", n.breaks=6, direction=1) +
	labs(x = "Year", y = "Altitude", fill = "HHI", title = paste0("Orbital-use concentration:\nshells with >",  hhi_cutoff," satellites")) +
	theme_minimal() + 
	guides(guide_legend(byrow = TRUE)) +
	theme(
		text = element_text(size=30, family="Arial"),
		legend.spacing.y =  unit(0.25, "cm"),
        legend.key.size = unit(3, 'lines')
	)

OU_data2 <- OU_data %>%
	mutate(
		commercial_ind = ifelse(str_detect(Entity_Type, "Commercial"), "Commercial", "Non-commercial"),
		Entity_Name = ifelse(str_detect(Entity_Name, "Iridium"), "Iridium", Entity_Name),
		Entity_Name = ifelse(str_detect(Entity_Type, "Government") | str_detect(Entity_Type, "Military") | str_detect(Entity_Type, "Civil"), Entity_Country, Entity_Name)
		)

biggest_orbit_users_data <- OU_data2 %>% 
	group_by(Entity_Name, commercial_ind, Year) %>%
	summarise(
		entity_sat_count = round(mean(Sat_Count_entity),0)
		) %>%
	filter(Year == 2012 | Year==2022) %>%
	group_by(Year) %>%
	arrange(-entity_sat_count) %>%
	mutate(
		topten_dup = ifelse(str_detect(Entity_Name, "Iridium") & Year==2022 & entity_sat_count <70, 1, 0),
		commercial_ind = ifelse(Entity_Name=="US"| Entity_Name=="PRC", "Non-commercial", commercial_ind)
	) %>% filter(topten_dup==0) %>% slice_max(n=10, order_by=entity_sat_count) 
biggest_orbit_users_data %>% print(n=22)

bou_data_2012 <- biggest_orbit_users_data %>% 
	filter(Year==2012) %>%
	arrange(-entity_sat_count) %>%
	mutate(
		Entity_Name2 = fct_reorder(.f = Entity_Name, .x = as.numeric(as.character(entity_sat_count))),
		rank = rank(entity_sat_count, ties.method="first")
	)

biggest_orbit_users_2012 <- ggplot(data = bou_data_2012, 
	aes(x=rank, y=entity_sat_count, fill=commercial_ind)) +
	geom_col(position="dodge") +
	theme_minimal() +
	labs(x="Fleet size rank", y="Satellites", title="Top 10 orbit users: 2012", fill="Entity type") +
	scale_fill_brewer(palette="Dark2") +
	coord_flip() + 
	theme(
		text = element_text(size=30, family="Arial"),
		axis.text.y = element_blank()
	) + guides(fill="none")

bou_data_2022 <- biggest_orbit_users_data %>% 
	filter(Year==2022) %>%
	arrange(-entity_sat_count) %>%
	mutate(
		Entity_Name = reorder(Entity_Name, entity_sat_count)
	)
biggest_orbit_users_2022 <- ggplot(data = bou_data_2022, 
	aes(x=Entity_Name, y=entity_sat_count, fill=commercial_ind)) +
	geom_col(position="dodge") +
	theme_minimal() +
	labs(x="", y="Satellites", title="Top 10 orbit users: 2022", fill="Entity type") +
	scale_fill_brewer(palette="Dark2") +
	coord_flip() + 
	theme(
		text = element_text(size=30, family="Arial"),
		axis.text.y = element_blank(),
        legend.key.size = unit(3, 'lines')
	)


ggsave(
	paste0("../images/concentration_growth.png"),
	(avg_fleet_size | orbit_HHI_scaled) / (biggest_orbit_users_2012 | biggest_orbit_users_2022) + plot_annotation(tag_levels = 'A') + plot_layout(guides = 'collect'),
	width = 20*(17.8/11.4),
	height = 20,
	units = "in",
	dpi = 300)
