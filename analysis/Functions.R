## Functions

## Descriptive analysis functions

# Create function to find the count and percent of a variable

calc_percent <- function(df = shs_shcs_selected, variable){
  df %>%
    group_by(df[[variable]]) %>%
    tally() %>%
    mutate(percent = round(n/sum(n)*100, 2))
}


## Results analysis functions

# Get the number of heat pumps installed in each year, over a number of runs of the same model

get_results <- function(run_name){
  files <- list.files(path="../../Results processed/", pattern=paste0(run_name, "_.*_model_history.feather"), full.names=TRUE, recursive=FALSE)
  
  dataframes <- lapply(files, function(x) {
    read_feather(x) %>%
      mutate(new_installs = rollsumr(model_heat_pump_installations_at_current_step, 12, fill = NA),
             sample = paste0("sample ", gsub("\\D", "", x))) %>% # Extract number from filename
      filter(step %in% c(11, 23, 35, 47, 59, 71, 83, 95, 107, 119, 131, 143, 155)) %>%
      mutate(year = c("2022", "2023", "2024", "2025", "2026", "2027", "2028", "2029", "2030", "2031", "2032", "2033", "2034")) %>%
      dplyr::select(year, new_installs, sample)
  })
  
  df <- do.call(rbind, dataframes)
  
  return(df %>% mutate(run = run_name))
}

# Perform a Mann Whitney U test for each year and print the results

mann_whitney_u <- function(df){
  for (i in unique(df$year)){
  
  wilcox_test <- wilcox.test(new_installs ~ run, data = df, subset = year == i, exact = FALSE) 
  
  print(paste(i, wilcox_test$statistic, wilcox_test$p.value, sep = "    "))
  }
}

# Perform a proportion test (z-test) for each year for the proportion of households that consider a heat pump

considering_z_test <- function(df, run_1, run_2){
  df %>%
    filter(considering_hps == 1) %>%
    group_by(run, year) %>%
    tally() %>%
    ungroup() %>%
    pivot_wider(id_cols = year, names_from = run, values_from = n) %>%
    rowwise() %>%
    mutate(run_1_tot = length(which(df$run == run_1 & df$year == year)),
           run_2_tot = length(which(df$run == run_2 & df$year == year))) %>%
    rowwise %>%
    summarise(out = list(prop.test(c(cur_data()[[2]], cur_data()[[3]]), c(run_1_tot, run_2_tot)) %>%
                           tidy)) %>%
    ungroup %>%
    unnest(cols = c(out)) %>%
    mutate(year = c("2022", "2023", "2024", "2025", "2026", "2027", "2028", "2029", "2030", "2031", "2032", "2033", "2034")) %>%
    select(year, statistic, p.value, estimate1, estimate2)
}

# Perform a proportion test (z-test) for each year for the proportion of households that consider a heat pump and then go on to install one

installing_z_test <- function(df, run_1, run_2){
  df %>%
    filter(installed_hp == 1) %>%
    group_by(run, year) %>%
    tally() %>%
    ungroup() %>%
    pivot_wider(id_cols = year, names_from = run, values_from = n) %>%
    rowwise() %>%
    mutate(run_1_tot = length(which(df$run == run_1 & df$considering_hps == 1 & df$year == year)),
           run_2_tot = length(which(df$run == run_2 & df$considering_hps == 1 & df$year == year))) %>%
    rowwise %>%
    summarise(out = list(prop.test(c(cur_data()[[2]], cur_data()[[3]]), c(run_1_tot, run_2_tot)) %>%
                           tidy)) %>%
    ungroup %>%
    unnest(cols = c(out)) %>%
    mutate(year = c("2022", "2023", "2024", "2025", "2026", "2027", "2028", "2029", "2030", "2031", "2032", "2033", "2034")) %>%
    select(year, statistic, p.value, estimate1, estimate2)
}

# Get the number of heat pumps installed in each year, over a number of runs for the same model, for the RHI calibartion tests

get_rhi_results <- function(run_name){
  files <- list.files(path="../Results processed/", pattern=paste0(run_name, "_.*_model_history.feather"), full.names=TRUE, recursive=FALSE)
  
  dataframes <- lapply(files, function(x) {
    read_feather(x) %>%
      mutate(new_installs = rollsumr(model_heat_pump_installations_at_current_step, 12, fill = NA),
             sample = paste0("sample ", gsub("\\D", "", x))) %>% # Extract number from filename
      filter(step %in% c(11, 23, 35, 47, 59, 71, 83, 95)) %>%
      mutate(year = c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")) %>%
      dplyr::select(year, new_installs, sample)
  })
  
  df <- do.call(rbind, dataframes)
  
  return(df %>% mutate(run = run_name))
  
}

# Get agent results, for all time-points where a household could be considering adopting a heat pump. Add flags for heat pump consideration and adoption.
# Some results only record awareness and heat pump suitability in the first step, so fill these down (remember to use a different function for the models where awareness changes over time)

get_all_hp_decision_points <- function(run_name){
  files <- list.files(path="../../Results processed/", pattern=paste0(run_name, "_.*_agent_history.feather"), full.names=TRUE, recursive=FALSE)
  
  dataframes <- lapply(files, function(x) {
    read_feather(x) %>%
      arrange(household_id, step) %>%
      fill(c(household_is_heat_pump_aware, household_is_heat_pump_suitable), .direction = "down") %>%
      filter(household_is_renovating_heating_system == TRUE | (household_heating_functioning == FALSE & household_heating_system %in%  c("HEAT_PUMP_AIR_SOURCE", "HEAT_PUMP_GROUND_SOURCE"))) %>%
      dplyr::select(step, household_id, household_heating_functioning, household_is_renovating_heating_system, household_renovation_budget, household_is_heat_pump_suitable, household_is_heat_pump_aware, household_heating_system, household_heating_system_previous, household_is_off_gas_grid, household_weight_boiler_gas, household_weight_boiler_electric, household_weight_boiler_oil, household_weight_heat_pump_air_source, household_weight_heat_pump_ground_source) %>%
      mutate(considering_hps = ifelse(!is.na(household_weight_heat_pump_air_source) | !is.na(household_weight_heat_pump_ground_source), 1, 0),
             installed_hp = ifelse(household_heating_system %in%  c("HEAT_PUMP_AIR_SOURCE", "HEAT_PUMP_GROUND_SOURCE") & considering_hps == 1, 1, 0),
             year = case_when(step <= 11 ~ "2022",
                              step <= 23 ~ "2023",
                              step <= 35 ~ "2024",
                              step <= 47 ~ "2025",
                              step <= 59 ~ "2026",
                              step <= 71 ~ "2027",
                              step <= 83 ~ "2028",
                              step <= 95 ~ "2029",
                              step <= 107 ~ "2030",
                              step <= 119 ~ "2031",
                              step <= 131 ~ "2032",
                              step <= 143 ~ "2033",
                              TRUE ~ "2034"))
  })
  
  df <- do.call(rbind, dataframes)
  
  return(df %>% mutate(run = run_name))
}

# Adapt this for the social influence model

get_all_hp_decision_points_influence <- function(run_name){
  files <- list.files(path="../../Results processed/", pattern=paste0(run_name, "_.*_agent_history.feather"), full.names=TRUE, recursive=FALSE)
  
  dataframes <- lapply(files, function(x) {
    read_feather(x) %>%
      arrange(household_id, step) %>%
      fill(c(household_is_heat_pump_aware, household_is_heat_pump_suitable), .direction = "down") %>%
      filter(household_is_renovating_heating_system == TRUE | (household_heating_functioning == FALSE & household_heating_system %in%  c("HEAT_PUMP_AIR_SOURCE", "HEAT_PUMP_GROUND_SOURCE"))) %>%
      dplyr::select(step, household_id, household_heating_functioning, household_is_renovating_heating_system, household_renovation_budget, household_is_heat_pump_suitable, household_is_heat_pump_aware, household_heating_system, household_heating_system_previous, household_is_off_gas_grid,
                    household_cost_weight_boiler_gas, household_cost_weight_boiler_electric, household_cost_weight_boiler_oil, household_cost_weight_heat_pump_air_source, household_cost_weight_heat_pump_ground_source,
                    household_neighbours_weight_boiler_gas, household_neighbours_weight_boiler_electric, household_neighbours_weight_boiler_oil, household_neighbours_weight_heat_pump_air_source, household_neighbours_weight_heat_pump_ground_source,
                    household_combined_weight_boiler_gas, household_combined_weight_boiler_electric, household_combined_weight_boiler_oil, household_combined_weight_heat_pump_air_source, household_combined_weight_heat_pump_ground_source) %>%
      mutate(considering_hps = ifelse(!is.na(household_combined_weight_heat_pump_air_source) | !is.na(household_combined_weight_heat_pump_ground_source), 1, 0),
             installed_hp = ifelse(household_heating_system %in%  c("HEAT_PUMP_AIR_SOURCE", "HEAT_PUMP_GROUND_SOURCE") & considering_hps == 1, 1, 0),
             year = case_when(step <= 11 ~ "2022",
                              step <= 23 ~ "2023",
                              step <= 35 ~ "2024",
                              step <= 47 ~ "2025",
                              step <= 59 ~ "2026",
                              step <= 71 ~ "2027",
                              step <= 83 ~ "2028",
                              step <= 95 ~ "2029",
                              step <= 107 ~ "2030",
                              step <= 119 ~ "2031",
                              step <= 131 ~ "2032",
                              step <= 143 ~ "2033",
                              TRUE ~ "2034"))
  })
  
  df <- do.call(rbind, dataframes)
  
  return(df %>% mutate(run = run_name))
}

# Adapt for scenario 2 of the social influence model

get_all_hp_decision_points_influence_sc2 <- function(run_name){
  files <- list.files(path="../../Results processed/", pattern=paste0(run_name, "_.*_agent_history.feather"), full.names=TRUE, recursive=FALSE)
  
  dataframes <- lapply(files, function(x) {
    read_feather(x) %>%
      arrange(household_id, step) %>%
      fill(c(household_is_heat_pump_aware, household_is_heat_pump_suitable), .direction = "down") %>%
      filter(household_is_renovating_heating_system == TRUE | (household_heating_functioning == FALSE & household_heating_system %in%  c("HEAT_PUMP_AIR_SOURCE", "HEAT_PUMP_GROUND_SOURCE")) |
               household_heating_functioning == FALSE & step >= 47) %>%
      dplyr::select(step, household_id, household_heating_functioning, household_is_renovating_heating_system, household_renovation_budget, household_is_heat_pump_suitable, household_is_heat_pump_aware, household_heating_system, household_heating_system_previous, household_is_off_gas_grid,
                    household_cost_weight_boiler_gas, household_cost_weight_boiler_electric, household_cost_weight_boiler_oil, household_cost_weight_heat_pump_air_source, household_cost_weight_heat_pump_ground_source,
                    household_neighbours_weight_boiler_gas, household_neighbours_weight_boiler_electric, household_neighbours_weight_boiler_oil, household_neighbours_weight_heat_pump_air_source, household_neighbours_weight_heat_pump_ground_source,
                    household_combined_weight_boiler_gas, household_combined_weight_boiler_electric, household_combined_weight_boiler_oil, household_combined_weight_heat_pump_air_source, household_combined_weight_heat_pump_ground_source) %>%
      mutate(considering_hps = ifelse(!is.na(household_combined_weight_heat_pump_air_source) | !is.na(household_combined_weight_heat_pump_ground_source), 1, 0),
             installed_hp = ifelse(household_heating_system %in%  c("HEAT_PUMP_AIR_SOURCE", "HEAT_PUMP_GROUND_SOURCE") & considering_hps == 1, 1, 0),
             year = case_when(step <= 11 ~ "2022",
                              step <= 23 ~ "2023",
                              step <= 35 ~ "2024",
                              step <= 47 ~ "2025",
                              step <= 59 ~ "2026",
                              step <= 71 ~ "2027",
                              step <= 83 ~ "2028",
                              step <= 95 ~ "2029",
                              step <= 107 ~ "2030",
                              step <= 119 ~ "2031",
                              step <= 131 ~ "2032",
                              step <= 143 ~ "2033",
                              TRUE ~ "2034"))
  })
  
  df <- do.call(rbind, dataframes)
  
  return(df %>% mutate(run = run_name))
}

# Adapt for scenario 2 of the green attitudes model

get_all_hp_decision_points_green_sc2 <- function(run_name){
  files <- list.files(path="../../Results processed/", pattern=paste0(run_name, "_.*_agent_history.feather"), full.names=TRUE, recursive=FALSE)
  
  dataframes <- lapply(files, function(x) {
    read_feather(x) %>%
      arrange(household_id, step) %>%
      fill(c(household_is_heat_pump_aware, household_is_heat_pump_suitable), .direction = "down") %>%
      filter(household_is_renovating_heating_system == TRUE | (household_heating_functioning == FALSE & household_heating_system %in%  c("HEAT_PUMP_AIR_SOURCE", "HEAT_PUMP_GROUND_SOURCE")) |
               household_heating_functioning == FALSE & step >= 47) %>%
      dplyr::select(step, household_id, household_heating_functioning, household_is_renovating_heating_system, household_renovation_budget, household_is_heat_pump_suitable, household_is_heat_pump_aware, household_heating_system, household_heating_system_previous, household_is_off_gas_grid,
                    household_cost_weight_boiler_gas, household_cost_weight_boiler_electric, household_cost_weight_boiler_oil, household_cost_weight_heat_pump_air_source, household_cost_weight_heat_pump_ground_source,
                    household_combined_weight_boiler_gas, household_combined_weight_boiler_electric, household_combined_weight_boiler_oil, household_combined_weight_heat_pump_air_source, household_combined_weight_heat_pump_ground_source) %>%
      mutate(considering_hps = ifelse(!is.na(household_combined_weight_heat_pump_air_source) | !is.na(household_combined_weight_heat_pump_ground_source), 1, 0),
             installed_hp = ifelse(household_heating_system %in%  c("HEAT_PUMP_AIR_SOURCE", "HEAT_PUMP_GROUND_SOURCE") & considering_hps == 1, 1, 0),
             year = case_when(step <= 11 ~ "2022",
                              step <= 23 ~ "2023",
                              step <= 35 ~ "2024",
                              step <= 47 ~ "2025",
                              step <= 59 ~ "2026",
                              step <= 71 ~ "2027",
                              step <= 83 ~ "2028",
                              step <= 95 ~ "2029",
                              step <= 107 ~ "2030",
                              step <= 119 ~ "2031",
                              step <= 131 ~ "2032",
                              step <= 143 ~ "2033",
                              TRUE ~ "2034"))
  })
  
  df <- do.call(rbind, dataframes)
  
  return(df %>% mutate(run = run_name))
}

# Create a slightly different function for scenario 2, because households can get a heat pump in a breakdown situation after the boiler ban is announced.

get_all_hp_decision_points_sc2 <- function(run_name){
  files <- list.files(path="../../Results processed/", pattern=paste0(run_name, "_.*_agent_history.feather"), full.names=TRUE, recursive=FALSE)
  
  dataframes <- lapply(files, function(x) {
    read_feather(x) %>%
      arrange(household_id, step) %>%
      fill(c(household_is_heat_pump_aware, household_is_heat_pump_suitable), .direction = "down") %>%
      filter(household_is_renovating_heating_system == TRUE |
               (household_heating_functioning == FALSE & household_heating_system %in%  c("HEAT_PUMP_AIR_SOURCE", "HEAT_PUMP_GROUND_SOURCE")) |
               household_heating_functioning == FALSE & step >= 47) %>%
      dplyr::select(step, household_id, household_heating_functioning, household_is_renovating_heating_system, household_renovation_budget, household_is_heat_pump_suitable, household_is_heat_pump_aware, household_heating_system, household_heating_system_previous, household_is_off_gas_grid, household_weight_boiler_gas, household_weight_boiler_electric, household_weight_boiler_oil, household_weight_heat_pump_air_source, household_weight_heat_pump_ground_source) %>%
      mutate(considering_hps = ifelse(!is.na(household_weight_heat_pump_air_source) | !is.na(household_weight_heat_pump_ground_source), 1, 0),
             installed_hp = ifelse(household_heating_system %in%  c("HEAT_PUMP_AIR_SOURCE", "HEAT_PUMP_GROUND_SOURCE") & considering_hps == 1, 1, 0),
             year = case_when(step <= 11 ~ "2022",
                              step <= 23 ~ "2023",
                              step <= 35 ~ "2024",
                              step <= 47 ~ "2025",
                              step <= 59 ~ "2026",
                              step <= 71 ~ "2027",
                              step <= 83 ~ "2028",
                              step <= 95 ~ "2029",
                              step <= 107 ~ "2030",
                              step <= 119 ~ "2031",
                              step <= 131 ~ "2032",
                              step <= 143 ~ "2033",
                              TRUE ~ "2034"))
  })
  
  df <- do.call(rbind, dataframes)
  
  return(df %>% mutate(run = run_name))
}

# Find the median of results for the RHI calibration

get_rhi_calibration_results_median <- function(run_name){
  files <- list.files(path="../../Results processed/", pattern=paste0(run_name, "_.*_model_history.feather"), full.names=TRUE, recursive=FALSE)
  
  dataframes <- lapply(files, function(x) {
    read_feather(x) %>%
      mutate(new_installs = rollsumr(model_heat_pump_installations_at_current_step, 12, fill = NA),
             sample = paste0("sample ", gsub("\\D", "", x))) %>% # Extract number from filename
      filter(step %in% c(11, 23, 35, 47, 59, 71, 83, 95)) %>%
      mutate(year = c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")) %>%
      dplyr::select(year, new_installs, sample)
  })
  
  df <- do.call(rbind, dataframes)
  
  return(df %>% mutate(run = run_name) %>% group_by(run, year) %>%
           summarise(results = median(new_installs)))
  
}

## Mathematical functions

# Find the mode

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

## Data visualisation functions

# Create a box plot to compare the results of two models

comparison_boxplot <- function(df, legend_title, legend_label_1, legend_label_2){
  df %>%
    ggplot(aes(x = year, y = new_installs, fill = run)) +
    geom_boxplot() +
    ylim(0,NA) +
    theme_classic() +
    labs(y = "Heat pump installations",
         x = "Year",
         fill = legend_title) +
    scale_fill_discrete(labels = c(legend_label_1, legend_label_2)) +
    geom_vline(xintercept = seq(0.5, length(df$year), by = 1), color="gray", size=.5, alpha=.5) # set vertical lines between x groups
}


# Create a box plot without legend labels

comparison_boxplot_no_leg <- function(df, legend_title){
  df %>%
    ggplot(aes(x = year, y = new_installs, fill = run)) +
    geom_boxplot() +
    ylim(0,NA) +
    theme_classic() +
    labs(y = "Heat pump installations",
         x = "Year",
         fill = legend_title) +
    geom_vline(xintercept = seq(0.5, length(df$year), by = 1), color="gray", size=.5, alpha=.5) # set vertical lines between x groups
}

# Create a chart to show the proportion of households considering heat pumps between models

percent_considering_hps_chart <- function(df, legend_title, legend_label_1, legend_label_2){
  df %>%
    group_by(run, year, considering_hps) %>%
    tally() %>%
    mutate(percent = n/sum(n)) %>%
    filter(considering_hps == 1) %>%
    ggplot(aes(x = year, y = percent, fill = run)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_classic() +
    labs(y = "Percent of households considering heat pumps",
         x = "Year",
         fill = legend_title) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_discrete(labels = c(legend_label_1, legend_label_2))
  
}

# Create chart with no legend names


percent_considering_hps_chart_no_leg <- function(df, legend_title){
  df %>%
    group_by(run, year, considering_hps) %>%
    tally() %>%
    mutate(percent = n/sum(n)) %>%
    filter(considering_hps == 1) %>%
    ggplot(aes(x = year, y = percent, fill = run)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_classic() +
    labs(y = "Percent of households considering heat pumps",
         x = "Year",
         fill = legend_title) +
    scale_y_continuous(labels = scales::percent)
  
}

# Create a chart to show the percentage of those who considered heat pumps and go on to install one, by model

percent_installing_hps_chart <- function(df, legend_title, legend_label_1, legend_label_2) {
  df %>%
    filter(considering_hps == 1) %>%
    group_by(run, year, installed_hp) %>%
    tally() %>%
    mutate(percent = n/sum(n)) %>%
    filter(installed_hp == 1) %>%
    ggplot(aes(x = year, y = percent, fill = run)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_classic() +
    labs(y = "Percent of households installing a heat pump",
         x = "Year",
         fill = legend_title) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_discrete(labels = c(legend_label_1, legend_label_2))
}

# Create a chart with no legend

percent_installing_hps_chart_no_leg <- function(df, legend_title) {
  df %>%
    filter(considering_hps == 1) %>%
    group_by(run, year, installed_hp) %>%
    tally() %>%
    mutate(percent = n/sum(n)) %>%
    filter(installed_hp == 1) %>%
    ggplot(aes(x = year, y = percent, fill = run)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_classic() +
    labs(y = "Percent of households installing a heat pump",
         x = "Year",
         fill = legend_title) +
    scale_y_continuous(labels = scales::percent)
}