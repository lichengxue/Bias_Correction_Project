# Set this to TRUE to execute the data processing, or FALSE to just load the function.
run <- FALSE

if (run) {
  # Define the main project directory
  base_dir <- "E:/Bias_Correction_Revision"
  
  #' A function to read, combine, and save data for a given stock.
  #'
  #' @param stock_name The name of the subdirectory (e.g., "Mackerel").
  #' @param output_filename The name for the final saved .RDS file.
  #' @param base_path The root directory for the project.
  process_and_save_data <- function(stock_name, output_filename, base_path) {
    
    # 1. Construct the full path to the data subdirectory
    data_path <- file.path(base_path, stock_name)
    
    # 2. Generate the full paths for all 24 CSV files
    full_file_paths <- file.path(data_path, sprintf("Result1_OM_%d.csv", 1:24))
    
    # 3. Read and combine all files in a single, efficient step
    # purrr::map_dfr maps the read_csv function over the file paths and
    # row-binds the results into one data frame.
    combined_data <- purrr::map_dfr(full_file_paths, readr::read_csv, .id = "source_om")
    
    # 4. Construct the full output path and save the final data frame
    output_path <- file.path(base_path, output_filename)
    saveRDS(combined_data, output_path)
    
    # 5. Print a message to confirm completion
    message(paste("✅ Successfully processed and saved data for:", stock_name))
  }
  
  # --- Now, simply call the function for each case ---
  
  process_and_save_data(stock_name = "Mackerel", output_filename = "Result1_M2.RDS", base_path = base_dir)
  process_and_save_data(stock_name = "Haddock", output_filename = "Result1_H2.RDS", base_path = base_dir)
  process_and_save_data(stock_name = "GBK", output_filename = "Result1_F2.RDS", base_path = base_dir)
  
}
# 1. Load required libraries
library(tidyverse)

# 2. Define inputs and processing logic in one place
stock_files <- c(Flounder = "Result1_F.RDS", Mackerel = "Result1_M.RDS", Haddock = "Result1_H.RDS")

# Define the two main scenarios to process
scenarios <- list(
  BC_ON  = list(om_values = c(1, 5, 9, 13), om_id = 1),
  BC_OFF = list(om_values = c(4, 8, 12, 16), om_id = 4)
)

# Process all data using `map` to avoid manual loops
all_data <- map_dfr(names(stock_files), function(stock_name) {
  
  raw_data <- readRDS(stock_files[stock_name])
  
  # Process both scenarios (BC_ON and BC_OFF) for the current stock
  map_dfr(scenarios, function(scenario) {
    raw_data %>%
      filter(OM %in% scenario$om_values) %>%
      mutate(
        Index = if_else(OM %in% c(1, 4, 5, 8), "Rec", "Rec+NAA")
      ) %>%
      filter(EM %in% c(1, 4)) %>%
      group_by(Index, EM, nsim) %>%
      summarise(
        mean_recruitment_relative = median(NAA_1_est / NAA_1_true - 1, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(OM_ID = scenario$om_id) # Add the scenario identifier (1 or 4)
  }) %>%
    mutate(Stock = stock_name) # Add the stock name
})

# 3. Create the final summary data needed for the plot
plot_data <- all_data %>%
  group_by(Stock, Index, EM, OM_ID) %>%
  summarise(
    median_rec = median(mean_recruitment_relative, na.rm = TRUE),
    Q1 = quantile(mean_recruitment_relative, 0.25, na.rm = TRUE),
    Q3 = quantile(mean_recruitment_relative, 0.75, na.rm = TRUE),
    min_val = boxplot.stats(mean_recruitment_relative)$stats[1],
    max_val = boxplot.stats(mean_recruitment_relative)$stats[5],
    .groups = "drop"
  ) %>%
  mutate(
    Type = if_else(EM == OM_ID, "Self-test", "Cross-test"),
    Type = paste0(Type, "\n", Index, " RE")
  )

# 4. Define the final plotting function (using your custom boxplot code)
bc.plot <- function(data, var = "median_rec", y_title = "Relative Error", plot_title = "Median Recruitment") {
  
  ggplot(data, aes(x = factor(EM), y = .data[[var]])) +
    facet_grid(Stock ~ factor(Type, levels = c("Self-test\nRec RE", "Cross-test\nRec RE",
                                               "Self-test\nRec+NAA RE", "Cross-test\nRec+NAA RE")), scales = "free") +
    # Draw the box from Q1 to Q3
    geom_rect(aes(xmin = as.numeric(factor(EM)) - 0.4, xmax = as.numeric(factor(EM)) + 0.4,
                  ymin = Q1, ymax = Q3), fill = "white", color = "black", alpha = 0.5) +
    # Draw vertical lines for the whiskers
    geom_segment(aes(x = as.numeric(factor(EM)), xend = as.numeric(factor(EM)),
                     y = min_val, yend = Q1), color = "black") +
    geom_segment(aes(x = as.numeric(factor(EM)), xend = as.numeric(factor(EM)),
                     y = Q3, yend = max_val), color = "black") +
    # Draw a horizontal line for the median
    geom_segment(aes(x = as.numeric(factor(EM)) - 0.4, xend = as.numeric(factor(EM)) + 0.4,
                     y = .data[[var]], yend = .data[[var]]), color = "black", size = 1) +
    # Customize labels and theme
    labs(x = "Estimation Model", y = y_title) +
    theme_bw() +
    ggtitle(plot_title) +
    geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
    scale_x_discrete(labels = c("1" = "BC-ON", "4" = "BC-OFF")) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none",
      axis.title = element_text(face = "bold", size = 10),
      axis.text = element_text(face = "bold", size = 10),
      strip.text = element_text(face = "bold", size = 10),
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5)
    )
}

# 5. Create and save the final plot
final_plot <- bc.plot(plot_data, var = "median_rec", y_title = "Median Relative Error", plot_title = "Recruitment")

ggsave("Median_Rec.PNG", final_plot, width = 8, height = 6, dpi = 300)

print(final_plot)

# 1. Load required libraries
library(tidyverse)

# 2. Define inputs for all stocks and scenarios
stock_files <- c(
  Flounder = "Result1_F.RDS",
  Mackerel = "Result1_M.RDS",
  Haddock  = "Result1_H.RDS"
)
scenarios <- list(
  BC_ON  = list(om_values = c(1, 5, 9, 13), om_id = 1),
  BC_OFF = list(om_values = c(4, 8, 12, 16), om_id = 4)
)

# 3. Process all data in a single, combined workflow
all_summaries <- map_dfr(scenarios, function(scenario) {
  map_dfr(stock_files, function(filepath) {
    readRDS(filepath) %>%
      filter(OM %in% scenario$om_values) %>%
      mutate(Index = if_else(OM %in% c(1, 4, 5, 8), "Rec", "Rec+NAA")) %>%
      filter(EM %in% c(1, 4)) %>%
      group_by(Index, EM, nsim) %>%
      summarise(mean_ssb_relative = median(SSB_est / SSB_true - 1, na.rm = TRUE), .groups = "drop")
  }, .id = "Stock") %>%
    mutate(OM = scenario$om_id) # Add the scenario ID (1 or 4) for self/cross testing
})

# 4. Prepare the final data structure for plotting
plot_data <- all_summaries %>%
  group_by(Stock, Index, EM, OM) %>%
  summarise(
    median_ssb = median(mean_ssb_relative, na.rm = TRUE),
    Q1 = quantile(mean_ssb_relative, 0.25, na.rm = TRUE),
    Q3 = quantile(mean_ssb_relative, 0.75, na.rm = TRUE),
    min_val = boxplot.stats(mean_ssb_relative)$stats[1],
    max_val = boxplot.stats(mean_ssb_relative)$stats[5],
    .groups = "drop"
  ) %>%
  mutate(
    # Create the "Self-test" vs "Cross-test" column
    Type = if_else(EM == OM, "Self-test", "Cross-test"),
    Type = paste0(Type, "\n", Index, " RE")
  )

# 5. Define the plotting function with your custom boxplot style
create_custom_boxplot <- function(data, var = "median_ssb", y_title = "Relative Error", plot_title = "Median SSB") {
  
  ggplot(data, aes(x = factor(EM), y = .data[[var]])) +
    facet_grid(Stock ~ factor(Type, levels = c("Self-test\nRec RE", "Cross-test\nRec RE",
                                               "Self-test\nRec+NAA RE", "Cross-test\nRec+NAA RE")), scales = "free") +
    # Draw the box from Q1 to Q3
    geom_rect(aes(xmin = as.numeric(factor(EM)) - 0.4, xmax = as.numeric(factor(EM)) + 0.4,
                  ymin = Q1, ymax = Q3), fill = "white", color = "black", alpha = 0.5) +
    # Draw vertical lines for the whiskers
    geom_segment(aes(x = as.numeric(factor(EM)), xend = as.numeric(factor(EM)),
                     y = min_val, yend = Q1), color = "black") +
    geom_segment(aes(x = as.numeric(factor(EM)), xend = as.numeric(factor(EM)),
                     y = Q3, yend = max_val), color = "black") +
    # Draw a horizontal line for the median
    geom_segment(aes(x = as.numeric(factor(EM)) - 0.4, xend = as.numeric(factor(EM)) + 0.4,
                     y = .data[[var]], yend = .data[[var]]), color = "black", linewidth = 1) +
    # Customize labels and theme
    labs(x = "Estimation Model", y = y_title) +
    theme_bw() +
    ggtitle(plot_title) +
    geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
    scale_x_discrete(labels = c("1" = "BC-ON", "4" = "BC-OFF")) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none",
      axis.title = element_text(face = "bold", size = 10),
      axis.text = element_text(face = "bold", size = 10),
      strip.text = element_text(face = "bold", size = 10),
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5)
    )
}

# 6. Create and save the final plot
final_ssb_plot <- create_custom_boxplot(plot_data, 
                                        var = "median_ssb", 
                                        y_title = "Median Relative Error", 
                                        plot_title = "SSB")

ggsave("Median_SSB.PNG", final_ssb_plot, width = 8, height = 6, dpi = 300)

print(final_ssb_plot)

# 1. Load required libraries
library(tidyverse)
library(ggplot2)

# 2. Define inputs and key mappings
stock_files <- c(
  Flounder = "Result1_F.RDS",
  Mackerel = "Result1_M.RDS",
  Haddock = "Result1_H.RDS"
)

# --- MODIFIED SECTION ---
# Define the 4 OM scenarios to plot, with custom names and in the desired order.
# This simultaneously renames, reorders, and filters the scenarios.
om_scenarios <- list(
  "Rec\nIID" = 1:4,
  "Rec\nAR1_y" = 9:12,
  "NAA+Rec\nIID" = 5:8,
  "NAA+Rec\nAR1_y" = 13:16
)
# --- END MODIFIED SECTION ---


# Create labels for bias correction settings for clearer plots
bias_labels <- c(
  `1` = "P:On O:On",
  `2` = "P:Off O:On",
  `3` = "P:On O:Off",
  `4` = "P:Off O:Off"
)


# 3. Process all data to create a master data frame
all_data <- map_dfr(names(stock_files), function(stock_name) {
  raw_data <- readRDS(stock_files[stock_name])
  
  # Iterate over each of the 4 defined OM scenarios
  map_dfr(names(om_scenarios), function(scenario_name) {
    raw_data %>%
      # Filter for OMs in the current scenario (e.g., 1-4 for "Rec,IID")
      filter(OM %in% om_scenarios[[scenario_name]]) %>%
      # Keep all 4 estimation models (EMs) for each OM
      group_by(OM, EM, nsim) %>%
      summarise(
        # Calculate the relative error for each simulation run
        re_recruitment = median((NAA_1_est / NAA_1_true) - 1, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      # Add columns to identify the scenario and stock
      mutate(
        OM_Scenario = scenario_name,
        Stock = stock_name
      )
  })
})


# 4. Create the final summary data needed for the plot
plot_data <- all_data %>%
  # Determine the bias correction setting of the OM (1, 2, 3, or 4)
  mutate(OM_bias_ID = (OM - 1) %% 4 + 1) %>%
  # Determine if the EM is "Correct" (matches OM) or "Mismatched"
  mutate(MatchType = if_else(EM == OM_bias_ID, "Correct", "Mismatched")) %>%
  group_by(Stock, OM_Scenario, OM_bias_ID, EM, MatchType) %>%
  # Calculate final summary statistics for plotting (median, quantiles)
  summarise(
    median_re = median(re_recruitment, na.rm = TRUE),
    Q1 = quantile(re_recruitment, 0.25, na.rm = TRUE),
    Q3 = quantile(re_recruitment, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Convert numeric IDs to descriptive factor labels for plotting
  mutate(
    OM_label = factor(bias_labels[OM_bias_ID], levels = bias_labels),
    EM_label = factor(bias_labels[EM], levels = bias_labels),
    # Ensure the plot's facet order respects the new `om_scenarios` order
    OM_Scenario = factor(OM_Scenario, levels = names(om_scenarios))
  )


# 5. Create and save the final plot
final_plot <- ggplot(plot_data, aes(x = OM_label, y = median_re, color = EM_label)) +
  # Use point-ranges for a clean look, dodged by EM
  geom_pointrange(
    aes(ymin = Q1, ymax = Q3, shape = MatchType),
    position = position_dodge(width = 0.8),
    size = 0.7
  ) +
  # Facet by Stock and the core OM scenario
  facet_grid(Stock ~ OM_Scenario, scales = "free") +
  # Add a horizontal line at zero for reference
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  # Manually set colors to be distinct
  scale_color_brewer(palette = "Dark2", name = "Estimation Model (EM):") +
  # Use a solid circle for the correct EM and an X for mismatched EMs
  scale_shape_manual(values = c("Correct" = 19, "Mismatched" = 4), name = "Match Type:") +
  # Customize labels and theme
  labs(
    x = "Operating Model Bias Correction",
    y = "Median Relative Error in Recruitment",
    title = "Performance of Estimation Models Across Operating Model Scenarios"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.title = element_text(face = "bold", size = 12),
    strip.text = element_text(face = "bold", size = 11), # Increased size for new labels
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

# Save the plot with adjusted dimensions for 4 columns
ggsave("OM_EM_Comparison_Plot_Revised_Rec.png", final_plot, width = 12, height = 8, dpi = 300)

# Print the plot to the R console
print(final_plot)

# Set this to TRUE to execute the data processing, or FALSE to just load the function.
run <- FALSE

if (run) {
  # Define the main project directory
  base_dir <- "E:/Bias_Correction_Revision"
  
  #' A function to read, combine, and save data for a given stock.
  #'
  #' @param stock_name The name of the subdirectory (e.g., "Mackerel").
  #' @param output_filename The name for the final saved .RDS file.
  #' @param base_path The root directory for the project.
  process_and_save_data <- function(stock_name, output_filename, base_path) {
    # 1. Construct the full path to the data subdirectory
    data_path <- file.path(base_path, stock_name)
    
    # 2. Generate the full paths for all 24 CSV files
    full_file_paths <- file.path(data_path, sprintf("Result2_OM_%d.csv", 1:24))
    
    # 3. Read and combine all files in a single, efficient step
    # purrr::map_dfr maps the read_csv function over the file paths and
    # row-binds the results into one data frame.
    combined_data <- purrr::map_dfr(full_file_paths, readr::read_csv, .id = "source_om")
    
    # 4. Construct the full output path and save the final data frame
    output_path <- file.path(base_path, output_filename)
    saveRDS(combined_data, output_path)
    
    # 5. Print a message to confirm completion
    message(paste("✅ Successfully processed and saved data for:", stock_name))
  }
  
  # --- Now, simply call the function for each case ---
  
  process_and_save_data(stock_name = "Mackerel", output_filename = "Result2_M2.RDS", base_path = base_dir)
  process_and_save_data(stock_name = "Haddock", output_filename = "Result2_H2.RDS", base_path = base_dir)
  process_and_save_data(stock_name = "GBK", output_filename = "Result2_F2.RDS", base_path = base_dir)
}
# 1. Load Required Libraries
library(dplyr)
library(ggplot2)
library(tidyr) # For the complete() function

# 2. Load and Combine Initial Data
# This section should be run only once.
dat1 <- read.csv("Result2_F.csv")
dat2 <- read.csv("Result2_H.csv")
dat3 <- read.csv("Result2_M.csv")

combined_data <- bind_rows(
  mutate(dat1, Stock = "Flounder"),
  mutate(dat2, Stock = "Haddock"),
  mutate(dat3, Stock = "Mackerel")
) %>%
  # Select only the necessary columns
  select(OM, nsim, EM, dAIC, wAIC, Stock)

# 3. Define Scenarios and Labels
# Mapping OM numbers to descriptive names with newlines for plotting
index_map <- c(
  `1` = "Rec\nIID", `4` = "Rec\nIID",
  `5` = "Rec\nAR1y", `8` = "Rec\nAR1y",
  `9` = "NAA\nIID", `12` = "NAA\nIID",
  `13` = "NAA\nAR1y", `16` = "NAA\nAR1y"
)

# 4. Process Data to Calculate Selection Rates
# This is the core data processing pipeline
summary_data <- combined_data %>%
  # Filter for the OMs and EMs of interest
  filter(OM %in% c(1, 4, 5, 8, 9, 12, 13, 16), EM %in% c(1, 4)) %>%
  # Create grouping variables for OM type and scenario index
  mutate(
    BC_Status = if_else(OM %in% c(1, 5, 9, 13), "OM:BC-ON", "OM:BC-OFF"),
    Index = index_map[as.character(OM)],
    Correct_EM_ID = if_else(BC_Status == "OM:BC-ON", 1, 4)
  ) %>%
  # Group by each unique simulation run to find the best model
  group_by(Stock, BC_Status, Index, nsim) %>%
  summarise(
    # Find the EM that was selected (had the minimum dAIC)
    Selected_EM = EM[which.min(dAIC)],
    # Check if the correct EM was selected
    Correct_EM_Was_Selected = Selected_EM == first(Correct_EM_ID),
    # Check if the selection was "significant" (dAIC difference >= 2)
    Is_Significant_Selection = max(dAIC) - min(dAIC) >= 2,
    .groups = "drop"
  ) %>%
  # Now, group by scenario to calculate proportions
  group_by(Stock, BC_Status, Index) %>%
  summarise(
    # Proportion where the correct EM was selected at all
    Rate_any_dAIC = mean(Correct_EM_Was_Selected, na.rm = TRUE),
    # Proportion where the correct EM was selected AND the choice was significant
    Rate_dAIC_gt2 = mean(Correct_EM_Was_Selected & Is_Significant_Selection, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Pivot the data into a long format suitable for plotting
  pivot_longer(
    cols = c(Rate_any_dAIC, Rate_dAIC_gt2),
    names_to = "Selection_Type",
    values_to = "Rate"
  ) %>%
  # Make the labels for the legend more descriptive
  mutate(
    Selection_Type = recode(Selection_Type,
                            "Rate_any_dAIC" = "Correct EM selected (dAIC > 0)",
                            "Rate_dAIC_gt2" = "Correct EM selected (dAIC > 2)"
    ),
    # Ensure factors are ordered correctly for plotting
    BC_Status = factor(BC_Status, levels = c("OM:BC-ON", "OM:BC-OFF")),
    Index = factor(Index, levels = c("Rec\nIID", "Rec\nAR1y", "NAA\nIID", "NAA\nAR1y")),
    Selection_Type = factor(Selection_Type, levels = c("Correct EM selected (dAIC > 0)", "Correct EM selected (dAIC > 2)"))
  ) %>%
  # IMPORTANT: Ensure all combinations exist, filling missing ones with a Rate of 0
  complete(Stock, BC_Status, Index, Selection_Type, fill = list(Rate = 0))


# 5. Create and Save the Final Plot
aic_selection_plot <- ggplot(summary_data, aes(x = BC_Status, y = Rate, fill = Selection_Type)) +
  # Create facets for each stock and scenario
  facet_grid(Stock ~ Index) +
  # Create the dodged bar chart
  geom_bar(position = "dodge", stat = "identity", color = "black", width = 0.7) +
  # Use a clear color scheme
  scale_fill_manual(
    values = c(
      "Correct EM selected (dAIC > 0)" = "#74c476", # Soft Green
      "Correct EM selected (dAIC > 2)" = "#238b45"  # Dark Green
    )
  ) +
  # Improve labels and titles
  labs(
    x = "Operating Model Bias Status",
    y = "Proportion of Simulations",
    fill = "Selection Criterion:",
    title = "Proportion of Times AIC Selected the Correct Estimation Model"
  ) +
  # Apply a clean theme and adjust text elements for readability
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10)
  )

# Print the plot to the viewer
print(aic_selection_plot)

# Save the final plot to a file
ggsave("AIC_select.PNG", aic_selection_plot, width = 10, height = 8, dpi = 300)

# 1. Load Required Libraries
library(tidyverse)
library(ggplot2)

# 2. Define Inputs
stock_files <- c(Flounder = "Result2_F.RDS", Mackerel = "Result2_M.RDS", Haddock = "Result2_H.RDS")

# Define the two main scenarios to process
scenarios <- list(
  BC_ON  = list(om_values = c(1, 5, 9, 13), om_id = 1),
  BC_OFF = list(om_values = c(4, 8, 12, 16), om_id = 4)
)

# 3. Process Data for Recruitment Sigma Relative Error
all_data <- map_dfr(names(stock_files), function(stock_name) {
  
  raw_data <- readRDS(stock_files[stock_name])
  
  # Process both scenarios (BC_ON and BC_OFF) for the current stock
  map_dfr(scenarios, function(scenario) {
    raw_data %>%
      # Add the Stock column here to make it available for grouping
      mutate(Stock = stock_name) %>% 
      filter(OM %in% scenario$om_values, EM %in% c(1, 4)) %>%
      mutate(
        Index = if_else(OM %in% c(1, 4, 5, 8), "Rec", "Rec+NAA")
      ) %>%
      # Group by each simulation to calculate the median relative error
      group_by(Stock, Index, EM, nsim) %>%
      summarise(
        # Calculate relative error for Rec_sigma
        re_value = median(Rec_sigma_est / Rec_sigma_true - 1, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(OM_ID = scenario$om_id) # Add the scenario identifier
  })
})

# 4. Create the final summary data needed for the plot
plot_data <- all_data %>%
  # Group by the main factors to get summary stats for the boxplot
  group_by(Stock, Index, EM, OM_ID) %>%
  summarise(
    median_re = median(re_value, na.rm = TRUE),
    Q1 = quantile(re_value, 0.25, na.rm = TRUE),
    Q3 = quantile(re_value, 0.75, na.rm = TRUE),
    # Use boxplot.stats to get whisker ends (handles outliers)
    min_val = boxplot.stats(re_value)$stats[1],
    max_val = boxplot.stats(re_value)$stats[5],
    .groups = "drop"
  ) %>%
  mutate(
    # Create the 'Type' column for faceting
    Type = if_else(EM == OM_ID, "Self-test", "Cross-test"),
    Type = paste0(Type, "\n", Index, " RE")
  )

# 5. Define the final plotting function
create_re_plot <- function(data, y_title, plot_title) {
  
  # Define the specific order for all four facet columns
  facet_levels <- c(
    "Self-test\nRec RE", "Cross-test\nRec RE",
    "Self-test\nRec+NAA RE", "Cross-test\nRec+NAA RE"
  )
  
  ggplot(data, aes(x = factor(EM), y = median_re)) +
    facet_grid(Stock ~ factor(Type, levels = facet_levels), scales = "free_y") +
    
    # Add a horizontal line at 0 for reference (no error)
    geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
    
    # Draw the box from Q1 to Q3
    geom_rect(aes(xmin = as.numeric(factor(EM)) - 0.4, xmax = as.numeric(factor(EM)) + 0.4,
                  ymin = Q1, ymax = Q3), fill = "white", color = "black", alpha = 0.5) +
    
    # Draw vertical lines for the whiskers
    geom_segment(aes(x = as.numeric(factor(EM)), xend = as.numeric(factor(EM)),
                     y = min_val, yend = Q1), color = "black") +
    geom_segment(aes(x = as.numeric(factor(EM)), xend = as.numeric(factor(EM)),
                     y = Q3, yend = max_val), color = "black") +
    
    # Draw a horizontal line for the median
    geom_segment(aes(x = as.numeric(factor(EM)) - 0.4, xend = as.numeric(factor(EM)) + 0.4,
                     y = median_re, yend = median_re), color = "black", size = 1) +
    
    # Customize labels and theme
    labs(x = "Estimation Model", y = y_title, title = plot_title) +
    scale_x_discrete(labels = c("1" = "BC-ON", "4" = "BC-OFF")) +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
      axis.text.y = element_text(face = "bold"),
      strip.text = element_text(face = "bold", size = 11),
      legend.position = "none"
    )
}

# 6. Create and save the final plot
final_plot <- create_re_plot(
  plot_data,
  y_title = "Median Relative Error",
  plot_title = "Relative Error of Recruitment Sigma (σ)"
)

ggsave("Rec_sigma.PNG", final_plot, width = 9, height = 7, dpi = 300)

print(final_plot)

# 1. Load Required Libraries
library(tidyverse)
library(ggplot2)

# 2. Define Inputs
stock_files <- c(Flounder = "Result2_F.RDS", Mackerel = "Result2_M.RDS", Haddock = "Result2_H.RDS")

# Define the two main scenarios to process
scenarios <- list(
  BC_ON  = list(om_values = c(1, 5, 9, 13), om_id = 1),
  BC_OFF = list(om_values = c(4, 8, 12, 16), om_id = 4)
)

# 3. Process Data for NAA Sigma Relative Error
all_data <- map_dfr(names(stock_files), function(stock_name) {
  
  raw_data <- readRDS(stock_files[stock_name])
  
  # Process both scenarios (BC_ON and BC_OFF) for the current stock
  map_dfr(scenarios, function(scenario) {
    raw_data %>%
      # Add the Stock column here to make it available for grouping
      mutate(Stock = stock_name) %>% 
      filter(OM %in% scenario$om_values, EM %in% c(1, 4)) %>%
      mutate(
        Index = if_else(OM %in% c(1, 4, 5, 8), "Rec", "Rec+NAA")
      ) %>%
      # Group by each simulation to calculate the median relative error
      group_by(Stock, Index, EM, nsim) %>%
      summarise(
        # Calculate relative error for NAA_sigma
        re_value = median(NAA_sigma_est / NAA_sigma_true - 1, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(OM_ID = scenario$om_id) # Add the scenario identifier
  })
})

# 4. Create the final summary data needed for the plot
plot_data <- all_data %>%
  # We only care about scenarios where NAA_sigma is relevant
  filter(Index == "Rec+NAA") %>%
  # Group by the main factors to get summary stats for the boxplot
  group_by(Stock, Index, EM, OM_ID) %>%
  summarise(
    median_re = median(re_value, na.rm = TRUE),
    Q1 = quantile(re_value, 0.25, na.rm = TRUE),
    Q3 = quantile(re_value, 0.75, na.rm = TRUE),
    # Use boxplot.stats to get whisker ends (handles outliers)
    min_val = boxplot.stats(re_value)$stats[1],
    max_val = boxplot.stats(re_value)$stats[5],
    .groups = "drop"
  ) %>%
  mutate(
    # Create the 'Type' column for faceting
    Type = if_else(EM == OM_ID, "Self-test", "Cross-test"),
    Type = paste0(Type, "\n", Index, " RE")
  )

# 5. Define the final plotting function
create_re_plot <- function(data, y_title, plot_title) {
  
  # Define the specific order for the facet columns
  facet_levels <- c("Self-test\nRec+NAA RE", "Cross-test\nRec+NAA RE")
  
  ggplot(data, aes(x = factor(EM), y = median_re)) +
    facet_grid(Stock ~ factor(Type, levels = facet_levels), scales = "free_y") +
    
    # Add a horizontal line at 0 for reference (no error)
    geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
    
    # Draw the box from Q1 to Q3
    geom_rect(aes(xmin = as.numeric(factor(EM)) - 0.4, xmax = as.numeric(factor(EM)) + 0.4,
                  ymin = Q1, ymax = Q3), fill = "white", color = "black", alpha = 0.5) +
    
    # Draw vertical lines for the whiskers
    geom_segment(aes(x = as.numeric(factor(EM)), xend = as.numeric(factor(EM)),
                     y = min_val, yend = Q1), color = "black") +
    geom_segment(aes(x = as.numeric(factor(EM)), xend = as.numeric(factor(EM)),
                     y = Q3, yend = max_val), color = "black") +
    
    # Draw a horizontal line for the median
    geom_segment(aes(x = as.numeric(factor(EM)) - 0.4, xend = as.numeric(factor(EM)) + 0.4,
                     y = median_re, yend = median_re), color = "black", size = 1) +
    
    # Customize labels and theme
    labs(x = "Estimation Model", y = y_title, title = plot_title) +
    scale_x_discrete(labels = c("1" = "BC-ON", "4" = "BC-OFF")) +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
      axis.text.y = element_text(face = "bold"),
      strip.text = element_text(face = "bold", size = 11),
      legend.position = "none"
    )
}

# 6. Create and save the final plot
final_plot <- create_re_plot(
  plot_data,
  y_title = "Median Relative Error",
  plot_title = "Relative Error of NAA Sigma (σ)"
)

ggsave("NAA_sigma.PNG", final_plot, width = 7, height = 7, dpi = 300)

print(final_plot)

# 1. Load Required Libraries
library(tidyverse)
library(ggplot2)

# 2. Define a Function to Process Data for a Single Stock
# This avoids repeating the same code block multiple times.
process_stock_data <- function(file_path, stock_name) {
  readRDS(file_path) %>%
    # Filter for the OMs and EMs of interest at the start
    filter(
      OM %in% c(1, 4, 5, 8, 9, 12, 13, 16),
      EM %in% c(1, 4)
    ) %>%
    # Group by each simulation run to calculate the SSPB
    group_by(OM, EM, nsim) %>%
    summarise(
      # Calculate Symmetric Signed Percentage Bias (SSPB)
      sspb = 100 * sign(median(log(NAA_1_est / NAA_1_true), na.rm = TRUE)) *
        (exp(abs(median(log(NAA_1_est / NAA_1_true), na.rm = TRUE))) - 1),
      .groups = "drop"
    ) %>%
    # Add columns for stock name, OM type, and scenario index
    mutate(
      Stock = stock_name,
      OM_Type = if_else(OM %in% c(1, 5, 9, 13), "BC-ON", "BC-OFF"),
      Index = if_else(OM %in% c(1, 4, 5, 8), "Rec", "Rec+NAA")
    )
}

# 3. Process All Stock Files and Combine
# Use map2_dfr to apply the function to each file and row-bind the results
stock_files <- c(Flounder = "Result1_F.RDS", Mackerel = "Result1_M.RDS", Haddock = "Result1_H.RDS")
all_data <- map2_dfr(stock_files, names(stock_files), process_stock_data)

# 4. Create Final Summary Data for Plotting
plot_data <- all_data %>%
  mutate(
    # Determine if it's a "Self-test" or "Cross-test"
    Correct_EM = if_else(OM_Type == "BC-ON", 1, 4),
    Test_Type = if_else(EM == Correct_EM, "Self-test", "Cross-test"),
    # Create the final faceting variable
    Type = paste0(Test_Type, "\n", Index, " RE")
  ) %>%
  # Group by all faceting and aesthetic variables to get summary stats
  group_by(Stock, Type, EM) %>%
  summarise(
    median_sspb = median(sspb, na.rm = TRUE),
    Q1 = quantile(sspb, 0.25, na.rm = TRUE),
    Q3 = quantile(sspb, 0.75, na.rm = TRUE),
    min_val = boxplot.stats(sspb)$stats[1],
    max_val = boxplot.stats(sspb)$stats[5],
    .groups = "drop"
  )

# 5. Define the Final Plotting Function
create_sspb_plot <- function(data, y_title, plot_title) {
  
  # Define the specific order for all four facet columns
  facet_levels <- c(
    "Self-test\nRec RE", "Cross-test\nRec RE",
    "Self-test\nRec+NAA RE", "Cross-test\nRec+NAA RE"
  )
  
  ggplot(data, aes(x = factor(EM), y = median_sspb)) +
    facet_grid(Stock ~ factor(Type, levels = facet_levels), scales = "free_y") +
    
    # Add a horizontal line at 0 for reference (no bias)
    geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
    
    # Draw the box from Q1 to Q3
    geom_rect(aes(xmin = as.numeric(factor(EM)) - 0.4, xmax = as.numeric(factor(EM)) + 0.4,
                  ymin = Q1, ymax = Q3), fill = "white", color = "black", alpha = 0.5) +
    
    # Draw vertical lines for the whiskers
    geom_segment(aes(x = as.numeric(factor(EM)), xend = as.numeric(factor(EM)),
                     y = min_val, yend = Q1), color = "black") +
    geom_segment(aes(x = as.numeric(factor(EM)), xend = as.numeric(factor(EM)),
                     y = Q3, yend = max_val), color = "black") +
    
    # Draw a horizontal line for the median
    geom_segment(aes(x = as.numeric(factor(EM)) - 0.4, xend = as.numeric(factor(EM)) + 0.4,
                     y = median_sspb, yend = median_sspb), color = "black", size = 1) +
    
    # Customize labels and theme
    labs(x = "Estimation Model", y = y_title, title = plot_title) +
    scale_x_discrete(labels = c("1" = "BC-ON", "4" = "BC-OFF")) +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
      axis.text.y = element_text(face = "bold"),
      strip.text = element_text(face = "bold", size = 11),
      legend.position = "none"
    )
}

# 6. Create and Save the Final Plot
final_plot <- create_sspb_plot(
  plot_data,
  y_title = "Symmetric Signed Percentage Bias",
  plot_title = "Median Symmetric Signed Percentage Bias of Recruitment"
)

ggsave("Median_Rec_SSPB.PNG", final_plot, width = 9, height = 7, dpi = 300)

print(final_plot)

# 1. Load Required Libraries
library(tidyverse)
library(ggplot2)

# 2. Define a Function to Process Data for a Single Stock
# This avoids repeating the same code block multiple times.
process_stock_data <- function(file_path, stock_name) {
  readRDS(file_path) %>%
    # Filter for the OMs and EMs of interest at the start
    filter(
      OM %in% c(1, 4, 5, 8, 9, 12, 13, 16),
      EM %in% c(1, 4)
    ) %>%
    # Group by each simulation run to calculate the SSPB
    group_by(OM, EM, nsim) %>%
    summarise(
      # Calculate Symmetric Signed Percentage Bias (SSPB) for SSB
      sspb = 100 * sign(median(log(SSB_est / SSB_true), na.rm = TRUE)) *
        (exp(abs(median(log(SSB_est / SSB_true), na.rm = TRUE))) - 1),
      .groups = "drop"
    ) %>%
    # Add columns for stock name, OM type, and scenario index
    mutate(
      Stock = stock_name,
      OM_Type = if_else(OM %in% c(1, 5, 9, 13), "BC-ON", "BC-OFF"),
      Index = if_else(OM %in% c(1, 4, 5, 8), "Rec", "Rec+NAA")
    )
}

# 3. Process All Stock Files and Combine
# Use map2_dfr to apply the function to each file and row-bind the results
stock_files <- c(Flounder = "Result1_F.RDS", Mackerel = "Result1_M.RDS", Haddock = "Result1_H.RDS")
all_data <- map2_dfr(stock_files, names(stock_files), process_stock_data)

# 4. Create Final Summary Data for Plotting
plot_data <- all_data %>%
  mutate(
    # Determine if it's a "Self-test" or "Cross-test"
    Correct_EM = if_else(OM_Type == "BC-ON", 1, 4),
    Test_Type = if_else(EM == Correct_EM, "Self-test", "Cross-test"),
    # Create the final faceting variable
    Type = paste0(Test_Type, "\n", Index, " RE")
  ) %>%
  # Group by all faceting and aesthetic variables to get summary stats
  group_by(Stock, Type, EM) %>%
  summarise(
    median_sspb = median(sspb, na.rm = TRUE),
    Q1 = quantile(sspb, 0.25, na.rm = TRUE),
    Q3 = quantile(sspb, 0.75, na.rm = TRUE),
    min_val = boxplot.stats(sspb)$stats[1],
    max_val = boxplot.stats(sspb)$stats[5],
    .groups = "drop"
  )

# 5. Define the Final Plotting Function
create_sspb_plot <- function(data, y_title, plot_title) {
  
  # Define the specific order for all four facet columns
  facet_levels <- c(
    "Self-test\nRec RE", "Cross-test\nRec RE",
    "Self-test\nRec+NAA RE", "Cross-test\nRec+NAA RE"
  )
  
  ggplot(data, aes(x = factor(EM), y = median_sspb)) +
    facet_grid(Stock ~ factor(Type, levels = facet_levels), scales = "free_y") +
    
    # Add a horizontal line at 0 for reference (no bias)
    geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
    
    # Draw the box from Q1 to Q3
    geom_rect(aes(xmin = as.numeric(factor(EM)) - 0.4, xmax = as.numeric(factor(EM)) + 0.4,
                  ymin = Q1, ymax = Q3), fill = "white", color = "black", alpha = 0.5) +
    
    # Draw vertical lines for the whiskers
    geom_segment(aes(x = as.numeric(factor(EM)), xend = as.numeric(factor(EM)),
                     y = min_val, yend = Q1), color = "black") +
    geom_segment(aes(x = as.numeric(factor(EM)), xend = as.numeric(factor(EM)),
                     y = Q3, yend = max_val), color = "black") +
    
    # Draw a horizontal line for the median
    geom_segment(aes(x = as.numeric(factor(EM)) - 0.4, xend = as.numeric(factor(EM)) + 0.4,
                     y = median_sspb, yend = median_sspb), color = "black", size = 1) +
    
    # Customize labels and theme
    labs(x = "Estimation Model", y = y_title, title = plot_title) +
    scale_x_discrete(labels = c("1" = "BC-ON", "4" = "BC-OFF")) +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
      axis.text.y = element_text(face = "bold"),
      strip.text = element_text(face = "bold", size = 11),
      legend.position = "none"
    )
}

# 6. Create and Save the Final Plot
final_plot <- create_sspb_plot(
  plot_data,
  y_title = "Symmetric Signed Percentage Bias",
  plot_title = "Median Symmetric Signed Percentage Bias of SSB"
)

ggsave("Median_SSB_SSPB.PNG", final_plot, width = 9, height = 7, dpi = 300)

print(final_plot)

# 1. Load Required Libraries
library(tidyverse)
library(ggplot2)

# 2. Define a Function to Process Data for a Single Stock
process_stock_data <- function(file_path, stock_name) {
  readRDS(file_path) %>%
    # Filter for the OMs and EMs of interest at the start
    filter(
      OM %in% c(1, 4, 5, 8, 9, 12, 13, 16),
      EM %in% c(1, 4)
    ) %>%
    # Group by each simulation run to calculate the log relative error
    group_by(OM, EM, nsim) %>%
    summarise(
      # Calculate log relative error for SSB
      log_re = median(log(SSB_est / SSB_true), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Add columns for stock name, OM type, and scenario index
    mutate(
      Stock = stock_name,
      OM_Type = if_else(OM %in% c(1, 5, 9, 13), "BC-ON", "BC-OFF"),
      Index = if_else(OM %in% c(1, 4, 5, 8), "Rec", "Rec+NAA")
    )
}

# 3. Process All Stock Files and Combine
# Use map2_dfr to apply the function to each file and row-bind the results
stock_files <- c(Flounder = "Result1_F.RDS", Mackerel = "Result1_M.RDS", Haddock = "Result1_H.RDS")
all_data <- map2_dfr(stock_files, names(stock_files), process_stock_data)

# 4. Create Final Summary Data for Plotting
plot_data <- all_data %>%
  mutate(
    # Determine if it's a "Self-test" or "Cross-test"
    Correct_EM = if_else(OM_Type == "BC-ON", 1, 4),
    Test_Type = if_else(EM == Correct_EM, "Self-test", "Cross-test"),
    # Create the final faceting variable
    Type = paste0(Test_Type, "\n", Index, " RE")
  ) %>%
  # Group by all faceting and aesthetic variables to get summary stats
  group_by(Stock, Type, EM) %>%
  summarise(
    median_log_re = median(log_re, na.rm = TRUE),
    Q1 = quantile(log_re, 0.25, na.rm = TRUE),
    Q3 = quantile(log_re, 0.75, na.rm = TRUE),
    min_val = boxplot.stats(log_re)$stats[1],
    max_val = boxplot.stats(log_re)$stats[5],
    .groups = "drop"
  )

# 5. Define the Final Plotting Function
create_log_re_plot <- function(data, y_title, plot_title) {
  
  # Define the specific order for all four facet columns
  facet_levels <- c(
    "Self-test\nRec RE", "Cross-test\nRec RE",
    "Self-test\nRec+NAA RE", "Cross-test\nRec+NAA RE"
  )
  
  ggplot(data, aes(x = factor(EM), y = median_log_re)) +
    facet_grid(Stock ~ factor(Type, levels = facet_levels), scales = "free_y") +
    
    # Add a horizontal line at 0 for reference (no error)
    geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
    
    # Draw the box from Q1 to Q3
    geom_rect(aes(xmin = as.numeric(factor(EM)) - 0.4, xmax = as.numeric(factor(EM)) + 0.4,
                  ymin = Q1, ymax = Q3), fill = "white", color = "black", alpha = 0.5) +
    
    # Draw vertical lines for the whiskers
    geom_segment(aes(x = as.numeric(factor(EM)), xend = as.numeric(factor(EM)),
                     y = min_val, yend = Q1), color = "black") +
    geom_segment(aes(x = as.numeric(factor(EM)), xend = as.numeric(factor(EM)),
                     y = Q3, yend = max_val), color = "black") +
    
    # Draw a horizontal line for the median
    geom_segment(aes(x = as.numeric(factor(EM)) - 0.4, xend = as.numeric(factor(EM)) + 0.4,
                     y = median_log_re, yend = median_log_re), color = "black", size = 1) +
    
    # Customize labels and theme
    labs(x = "Estimation Model", y = y_title, title = plot_title) +
    scale_x_discrete(labels = c("1" = "BC-ON", "4" = "BC-OFF")) +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
      axis.text.y = element_text(face = "bold"),
      strip.text = element_text(face = "bold", size = 11),
      legend.position = "none"
    )
}

# 6. Create and Save the Final Plot
final_plot <- create_log_re_plot(
  plot_data,
  y_title = "Log Relative Error",
  plot_title = "Median Log Relative Error of SSB"
)

ggsave("Median_SSB_logRE.PNG", final_plot, width = 9, height = 7, dpi = 300)

print(final_plot)


# 1. Load Required Libraries
library(tidyverse)
library(ggplot2)

# 2. Define a Function to Process Data for a Single Stock
process_stock_data <- function(file_path, stock_name) {
  readRDS(file_path) %>%
    # Filter for the OMs and EMs of interest at the start
    filter(
      OM %in% c(1, 4, 5, 8, 9, 12, 13, 16),
      EM %in% c(1, 4)
    ) %>%
    # Group by each simulation run to calculate the log relative error
    group_by(OM, EM, nsim) %>%
    summarise(
      # Calculate log relative error for Recruitment (NAA_1)
      log_re = median(log(NAA_1_est / NAA_1_true), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Add columns for stock name, OM type, and scenario index
    mutate(
      Stock = stock_name,
      OM_Type = if_else(OM %in% c(1, 5, 9, 13), "BC-ON", "BC-OFF"),
      Index = if_else(OM %in% c(1, 4, 5, 8), "Rec", "Rec+NAA")
    )
}

# 3. Process All Stock Files and Combine
# Use map2_dfr to apply the function to each file and row-bind the results
stock_files <- c(Flounder = "Result1_F.RDS", Mackerel = "Result1_M.RDS", Haddock = "Result1_H.RDS")
all_data <- map2_dfr(stock_files, names(stock_files), process_stock_data)

# 4. Create Final Summary Data for Plotting
plot_data <- all_data %>%
  mutate(
    # Determine if it's a "Self-test" or "Cross-test"
    Correct_EM = if_else(OM_Type == "BC-ON", 1, 4),
    Test_Type = if_else(EM == Correct_EM, "Self-test", "Cross-test"),
    # Create the final faceting variable
    Type = paste0(Test_Type, "\n", Index, " RE")
  ) %>%
  # Group by all faceting and aesthetic variables to get summary stats
  group_by(Stock, Type, EM) %>%
  summarise(
    median_log_re = median(log_re, na.rm = TRUE),
    Q1 = quantile(log_re, 0.25, na.rm = TRUE),
    Q3 = quantile(log_re, 0.75, na.rm = TRUE),
    min_val = boxplot.stats(log_re)$stats[1],
    max_val = boxplot.stats(log_re)$stats[5],
    .groups = "drop"
  )

# 5. Define the Final Plotting Function
create_log_re_plot <- function(data, y_title, plot_title) {
  
  # Define the specific order for all four facet columns
  facet_levels <- c(
    "Self-test\nRec RE", "Cross-test\nRec RE",
    "Self-test\nRec+NAA RE", "Cross-test\nRec+NAA RE"
  )
  
  ggplot(data, aes(x = factor(EM), y = median_log_re)) +
    facet_grid(Stock ~ factor(Type, levels = facet_levels), scales = "free_y") +
    
    # Add a horizontal line at 0 for reference (no error)
    geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
    
    # Draw the box from Q1 to Q3
    geom_rect(aes(xmin = as.numeric(factor(EM)) - 0.4, xmax = as.numeric(factor(EM)) + 0.4,
                  ymin = Q1, ymax = Q3), fill = "white", color = "black", alpha = 0.5) +
    
    # Draw vertical lines for the whiskers
    geom_segment(aes(x = as.numeric(factor(EM)), xend = as.numeric(factor(EM)),
                     y = min_val, yend = Q1), color = "black") +
    geom_segment(aes(x = as.numeric(factor(EM)), xend = as.numeric(factor(EM)),
                     y = Q3, yend = max_val), color = "black") +
    
    # Draw a horizontal line for the median
    geom_segment(aes(x = as.numeric(factor(EM)) - 0.4, xend = as.numeric(factor(EM)) + 0.4,
                     y = median_log_re, yend = median_log_re), color = "black", size = 1) +
    
    # Customize labels and theme
    labs(x = "Estimation Model", y = y_title, title = plot_title) +
    scale_x_discrete(labels = c("1" = "BC-ON", "4" = "BC-OFF")) +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
      axis.text.y = element_text(face = "bold"),
      strip.text = element_text(face = "bold", size = 11),
      legend.position = "none"
    )
}

# 6. Create and Save the Final Plot
final_plot <- create_log_re_plot(
  plot_data,
  y_title = "Log Relative Error",
  plot_title = "Median Log Relative Error of Recruitment"
)

ggsave("Median_Rec_logRE.PNG", final_plot, width = 9, height = 7, dpi = 300)

print(final_plot)

# 1. Load Required Libraries
library(tidyverse)
library(ggplot2)

# 2. Define a Function to Process and Combine Data
# This function reads all stock files, combines them, and calculates relative errors.
load_and_process_data <- function(files) {
  
  # Read and combine all data files, adding a 'Stock' column
  map2_dfr(files, names(files), ~read.csv(.x) %>% mutate(Stock = .y)) %>%
    # Filter for only the OMs and EMs of interest
    filter(
      OM %in% c(1, 4, 5, 8, 9, 12, 13, 16),
      EM %in% c(1, 4)
    ) %>%
    # Calculate relative error for the AR(1) parameter
    mutate(
      Rho_y_relative = Rho_y_est / Rho_y_true - 1
    ) %>%
    # Add columns to define the scenario type for plotting
    mutate(
      OM_Type = if_else(OM %in% c(1, 5, 9, 13), "BC-ON", "BC-OFF"),
      Index = if_else(OM %in% c(1, 4, 5, 8), "Rec", "Rec+NAA"),
      Correct_EM = if_else(OM_Type == "BC-ON", 1, 4),
      Test_Type = if_else(EM == Correct_EM, "Self-test", "Cross-test"),
      Type = paste0(Test_Type, "\n", Index, " RE")
    )
}

# 3. Define the Plotting Function
create_re_boxplot <- function(data, var, y_title, plot_title) {
  
  # Define the specific order for the facet columns
  facet_levels <- c(
    "Self-test\nRec RE", "Cross-test\nRec RE",
    "Self-test\nRec+NAA RE", "Cross-test\nRec+NAA RE"
  )
  
  ggplot(data, aes(x = factor(EM), y = .data[[var]])) +
    facet_grid(Stock ~ factor(Type, levels = facet_levels), scales = "free_y") +
    geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
    geom_boxplot(outlier.shape = NA) + # Use geom_boxplot directly
    labs(x = "Estimation Model", y = y_title, title = plot_title) +
    scale_x_discrete(labels = c("1" = "BC-ON", "4" = "BC-OFF")) +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
      axis.text.y = element_text(face = "bold"),
      strip.text = element_text(face = "bold", size = 11),
      legend.position = "none"
    )
}

# 4. Execute the Workflow
# Define the stock files
stock_files <- c(Flounder = "Result2_F.csv", Mackerel = "Result2_M.csv", Haddock = "Result2_H.csv")

# Load and process all data in one step
plot_data <- load_and_process_data(stock_files)

# Create and save the final plot
final_plot <- create_re_boxplot(
  data = plot_data,
  var = "Rho_y_relative",
  y_title = "Relative Error",
  plot_title = "AR(1)-year Coefficient"
)

ggsave("Rho.PNG", final_plot, width = 9, height = 7, dpi = 300)

# Print the plot to the viewer
print(final_plot)

# 1. Load Required Libraries
library(tidyverse)
library(ggplot2)

# 2. Define a Function to Process and Combine Data
# This function reads all stock files, combines them, and prepares them for plotting.
load_and_process_data <- function(files) {
  
  # Read and combine all data files, adding a 'Stock' column
  map2_dfr(files, names(files), ~read.csv(.x) %>% mutate(Stock = .y)) %>%
    # Select only the necessary columns to keep the data frame tidy
    select(Stock, OM, nsim, EM, dAIC) %>%
    # Filter for only the OMs and EMs of interest
    filter(
      OM %in% c(1, 4, 5, 8, 9, 12, 13, 16),
      EM %in% c(1, 4)
    ) %>%
    # Add columns to define the scenario type for plotting
    mutate(
      OM_Type = if_else(OM %in% c(1, 5, 9, 13), "BC-ON", "BC-OFF"),
      Index = if_else(OM %in% c(1, 4, 5, 8), "Rec", "Rec+NAA"),
      Correct_EM = if_else(OM_Type == "BC-ON", 1, 4),
      Test_Type = if_else(EM == Correct_EM, "Self-test", "Cross-test"),
      Type = paste0(Test_Type, "\n", Index, " RE")
    )
}

# 3. Define the Plotting Function
create_re_boxplot <- function(data, var, y_title, plot_title) {
  
  # Define the specific order for the facet columns
  facet_levels <- c(
    "Self-test\nRec RE", "Cross-test\nRec RE",
    "Self-test\nRec+NAA RE", "Cross-test\nRec+NAA RE"
  )
  
  ggplot(data, aes(x = factor(EM), y = .data[[var]])) +
    facet_grid(Stock ~ factor(Type, levels = facet_levels), scales = "free_y") +
    geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
    geom_boxplot(outlier.shape = NA) + # Use geom_boxplot directly
    labs(x = "Estimation Model", y = y_title, title = plot_title) +
    scale_x_discrete(labels = c("1" = "BC-ON", "4" = "BC-OFF")) +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
      axis.text.y = element_text(face = "bold"),
      strip.text = element_text(face = "bold", size = 11),
      legend.position = "none"
    )
}

# 4. Execute the Workflow
# Define the stock files
stock_files <- c(Flounder = "Result2_F.csv", Mackerel = "Result2_M.csv", Haddock = "Result2_H.csv")

# Load and process all data in one step
plot_data <- load_and_process_data(stock_files)

# Create and save the final plot for dAIC
final_plot <- create_re_boxplot(
  data = plot_data,
  var = "dAIC",
  y_title = "dAIC",
  plot_title = "dAIC"
)

ggsave("dAIC.PNG", final_plot, width = 9, height = 7, dpi = 300)

# Print the plot to the viewer
print(final_plot)

# reorder <- function(data) {
#   tmp1 <- data[data$OM %in% 5:8,]
#   tmp1$OM <- tmp1$OM + 4
#   tmp2 <- data[data$OM %in% 9:12,]
#   tmp2$OM <- tmp2$OM - 4
#   tmp3 <- data[!data$OM %in% 5:12,]
#   data <- rbind(tmp1,tmp2,tmp3)
#   return(data)
# }

# Read the data
# data1 <- read.csv("Result2_F.csv")
# data2 <- read.csv("Result2_M.csv") 
# data3 <- read.csv("Result2_H.csv")

# data1 <- reorder(data1)
# write.csv(data1,"Result2_F.csv",row.names = F)
# 
# data2 <- reorder(data2)
# write.csv(data2,"Result2_M.csv",row.names = F)
# 
# data3 <- reorder(data3)
# write.csv(data3,"Result2_H.csv",row.names = F)

# data1 <- read.csv("Result1_F.csv")
# data2 <- read.csv("Result1_M.csv")
# data3 <- read.csv("Result1_H.csv")
# 
# data1 <- reorder(data1)
# write.csv(data1,"Result1_F.csv",row.names = F)
# 
# data2 <- reorder(data2)
# write.csv(data2,"Result1_M.csv",row.names = F)
# 
# data3 <- reorder(data3)
# write.csv(data3,"Result1_H.csv",row.names = F)

# Load necessary libraries
library(ggplot2)
library(reshape2)
library(dplyr)
library(gridExtra)
library(cowplot)

# Define a function to create the correlation plot
generate_correlation_plot <- function(data, title = "Correlation Plot", filename = "Correlation_plot.png") {
  # Define mapping from variable names to Greek labels
  variable_labels <- list(
    "Rec_sigma_est" = expression(bold(sigma[Rec])),
    "NAA_sigma_est" = expression(bold(sigma[NAA])),
    "Rho_y_est" = expression(bold(rho[year])),
    "Rec_par_est" = expression(bold(mu[Rec]))
  )
  
  # Prepare plotting for multiple plots
  plot_list <- list()
  
  for (num in unique(data$OM)) {
    for (name in unique(data$Stock)) {
      BC <- data %>% filter(Stock == name, OM == num) %>% select(BC) %>% unique()
      tmp <- data %>% filter(Stock == name, OM == num) %>% select(-c(OM, EM, Stock, BC)) %>% select(where(~ any(. != 0)))
      # Calculate the correlation matrix and p-values
      M <- cor(tmp)
      testRes <- cor.mtest(tmp, conf.level = 0.95)
      
      # Melt the correlation matrix and p-values into long format for ggplot
      M_melted <- melt(M)
      colnames(M_melted) <- c("Var1", "Var2", "Correlation")
      p_values <- melt(testRes$p)
      colnames(p_values) <- c("Var1", "Var2", "p_value")
      
      # Combine correlation and p-value data
      plot_data <- left_join(M_melted, p_values, by = c("Var1", "Var2"))
      
      # Filter to keep only the lower triangle of the correlation matrix (excluding diagonal)
      plot_data <- plot_data %>% filter(match(Var1, levels(plot_data$Var1)) > match(Var2, levels(plot_data$Var2)))
      
      # Extract subset of variable_labels for existing variables
      unique_vars <- levels(c(plot_data$Var1, plot_data$Var2))
      label_subset <- variable_labels[unique_vars]
      
      if (title == "Cross-test") {
        if(BC$BC == "BC ON") {
          BC$BC = "BC OFF"
        } else {
          BC$BC = "BC ON"
        }
      }
      
      # Create the ggplot for the correlation matrix
      p <- ggplot(plot_data, aes(x = Var1, y = Var2, fill = Correlation, size = abs(Correlation))) +
        geom_point(shape = 21, alpha = 0.8, color = "NA") +  # Bubble for correlation magnitude
        geom_text(data = filter(plot_data, p_value <= 0.05), aes(label = round(Correlation, 2)), color = "black", size = 4, fontface = "bold") +  # Add number for significant correlations
        geom_text(data = filter(plot_data, p_value > 0.05), aes(label = round(Correlation, 2)), color = "black", size = 3, fontface = "bold", alpha = 0.3) +  # Add number for significant correlations
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), guide = "none") +  # Adjust color scale for negative and positive correlations, remove legend
        scale_size_continuous(range = c(1, 10), guide = "none") +  # Bubble size for correlation magnitude, remove legend
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              axis.text.y = element_text(size = 10),
              axis.title = element_blank(),
              panel.grid.major = element_line(color = "grey80"),  # Add grid lines
              panel.border = element_blank(),
              panel.background = element_blank(),
              legend.position = "none") +  # Remove legend
        coord_fixed() +
        ggtitle(parse(text = paste0("'", name, " (EM: ", BC, ")'"))) +
        theme(plot.title = element_text(hjust = 0.5, size = 12)) +
        labs(x = "Variable 1", y = "Variable 2") +
        scale_x_discrete(labels = function(x) sapply(x, function(v) ifelse(!is.null(variable_labels[[v]]), parse(text = variable_labels[[v]]), v))) +
        scale_y_discrete(labels = function(x) sapply(x, function(v) ifelse(!is.null(variable_labels[[v]]), parse(text = variable_labels[[v]]), v)))
      
      
      plot_list <- append(plot_list, list(p))
    }
  }
  
  # Combine all plots into a single grid
  final_plot <- plot_grid(plotlist = plot_list, ncol = 3, align = "hv")
  final_plot <- final_plot + 
    ggtitle(title) + 
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
  
  # Print the final plot
  print(final_plot)
  ggsave(filename, plot = final_plot, width = 20, height = 10, units = "cm", dpi = 200)
}

manipulation <- function(data) {
  data$Match = data$OM %% 4
  data$Match[data$Match == 0] = 4
  data$Match = ifelse(data$Match == data$EM, TRUE, FALSE)
  data <- data[data$Match == TRUE,] %>%
    select(OM, EM, Rec_sigma_est,NAA_sigma_est, Rho_y_est, Rec_par_est) 
  return(data)
}

library(dplyr); library(corrplot)
dat1 <- read.csv("Result2_F.csv")
dat2 <- read.csv("Result2_H.csv")
dat3 <- read.csv("Result2_M.csv") 

#### Rec iid ####
data1 <- dat1 %>% filter(OM %in% c(1,4))
data2 <- dat2 %>% filter(OM %in% c(1,4))
data3 <- dat3 %>% filter(OM %in% c(1,4))

data1 <- manipulation(data1) 
data2 <- manipulation(data2) 
data3 <- manipulation(data3) 

# Add the "Stock" column to each dataframe
data1$Stock <- "Flounder"
data3$Stock <- "Mackerel"
data2$Stock <- "Haddock"

# Combine the dataframes using rbind
combined_data <- rbind(data1, data2, data3) %>%  
  mutate(BC = ifelse(OM == 1, "BC ON", "BC OFF"))

generate_correlation_plot(combined_data, title = "Self-test", filename = "Correlation_plot_Rec_iid.PNG")

#### Rec ar1_y ####
data1 <- dat1 %>% filter(OM %in% c(5,8))
data2 <- dat2 %>% filter(OM %in% c(5,8))
data3 <- dat3 %>% filter(OM %in% c(5,8))

data1 <- manipulation(data1) 
data2 <- manipulation(data2) 
data3 <- manipulation(data3) 

# Add the "Stock" column to each dataframe
data1$Stock <- "Flounder"
data3$Stock <- "Mackerel"
data2$Stock <- "Haddock"

# Combine the dataframes using rbind
combined_data <- rbind(data1, data2, data3) %>%  
  mutate(BC = ifelse(OM == 5, "BC ON", "BC OFF"))

generate_correlation_plot(combined_data, title = "Self-test", filename = "Correlation_plot_Rec_ar1y.PNG")

#### NAA iid ####
data1 <- dat1 %>% filter(OM %in% c(9,12))
data2 <- dat2 %>% filter(OM %in% c(9,12))
data3 <- dat3 %>% filter(OM %in% c(9,12))

data1 <- manipulation(data1) 
data2 <- manipulation(data2) 
data3 <- manipulation(data3) 

# Add the "Stock" column to each dataframe
data1$Stock <- "Flounder"
data3$Stock <- "Mackerel"
data2$Stock <- "Haddock"

# Combine the dataframes using rbind
combined_data <- rbind(data1, data2, data3) %>%  
  mutate(BC = ifelse(OM == 9, "BC ON", "BC OFF"))

generate_correlation_plot(combined_data, title = "Self-test", filename = "Correlation_plot_NAA_iid.PNG")

data1 <- dat1 %>% filter(OM %in% c(13,16))
data2 <- dat2 %>% filter(OM %in% c(13,16))
data3 <- dat3 %>% filter(OM %in% c(13,16))

data1 <- manipulation(data1) 
data2 <- manipulation(data2) 
data3 <- manipulation(data3) 

# Add the "Stock" column to each dataframe
data1$Stock <- "Flounder"
data3$Stock <- "Mackerel"
data2$Stock <- "Haddock"

# Combine the dataframes using rbind
combined_data <- rbind(data1, data2, data3) %>%  
  mutate(BC = ifelse(OM == 13, "BC ON", "BC OFF"))

generate_correlation_plot(combined_data, title = "Self-test", filename = "Correlation_plot_NAA_ar1y.PNG")

#### Rec iid mismatch ####
manipulation <- function(data) {
  data$Match = data$OM %% 4
  data$Match[data$Match == 0] = 4
  data$Match = ifelse(data$Match == data$EM, TRUE, FALSE)
  data <- data[data$Match == FALSE,] %>%
    select(OM, EM, Rec_sigma_est,NAA_sigma_est, Rho_y_est, Rec_par_est) 
  return(data)
}

data1 <- dat1 %>% filter(OM %in% c(1,4))
data2 <- dat2 %>% filter(OM %in% c(1,4))
data3 <- dat3 %>% filter(OM %in% c(1,4))

data1 <- manipulation(data1) 
data2 <- manipulation(data2) 
data3 <- manipulation(data3) 

# Add the "Stock" column to each dataframe
data1$Stock <- "Flounder"
data3$Stock <- "Mackerel"
data2$Stock <- "Haddock"

data <- rbind(data1, data2, data3) %>%  
  mutate(BC = ifelse(OM == 1, "BC ON", "BC OFF"))

combined_data <- data %>%
  filter((OM == 1 & EM == 4) | (OM == 4 & EM == 1))
# Define the labels as mathematical expressions

generate_correlation_plot(combined_data, title = "Cross-test", filename = "Correlation_plot_Rec_iid_mismatch.PNG")

#### Rec ar1_y mismatch ####
manipulation <- function(data) {
  data$Match = data$OM %% 4
  data$Match[data$Match == 0] = 4
  data$Match = ifelse(data$Match == data$EM, TRUE, FALSE)
  data <- data[data$Match == FALSE,] %>%
    select(OM, EM, Rec_sigma_est,NAA_sigma_est, Rho_y_est, Rec_par_est) 
  return(data)
}

data1 <- dat1 %>% filter(OM %in% c(5,8))
data2 <- dat2 %>% filter(OM %in% c(5,8))
data3 <- dat3 %>% filter(OM %in% c(5,8))

data1 <- manipulation(data1) 
data2 <- manipulation(data2) 
data3 <- manipulation(data3) 

# Add the "Stock" column to each dataframe
data1$Stock <- "Flounder"
data3$Stock <- "Mackerel"
data2$Stock <- "Haddock"

# Combine the dataframes using rbind
data <- rbind(data1, data2, data3) %>%  
  mutate(BC = ifelse(OM == 5, "BC ON", "BC OFF"))

combined_data <- data %>%
  filter((OM == 5 & EM == 4) | (OM == 8 & EM == 1))

generate_correlation_plot(combined_data, title = "Cross-test", filename = "Correlation_plot_Rec_ar1y_mismatch.PNG")

#### NAA iid mismatch ####
manipulation <- function(data) {
  data$Match = data$OM %% 4
  data$Match[data$Match == 0] = 4
  data$Match = ifelse(data$Match == data$EM, TRUE, FALSE)
  data <- data[data$Match == FALSE,] %>%
    select(OM, EM, Rec_sigma_est,NAA_sigma_est, Rho_y_est, Rec_par_est) 
  return(data)
}

data1 <- dat1 %>% filter(OM %in% c(9,12))
data2 <- dat2 %>% filter(OM %in% c(9,12))
data3 <- dat3 %>% filter(OM %in% c(9,12))

data1 <- manipulation(data1) 
data2 <- manipulation(data2) 
data3 <- manipulation(data3) 

# Add the "Stock" column to each dataframe
data1$Stock <- "Flounder"
data3$Stock <- "Mackerel"
data2$Stock <- "Haddock"

data <- rbind(data1, data2, data3) %>%  
  mutate(BC = ifelse(OM == 9, "BC ON", "BC OFF"))

combined_data <- data %>%
  filter((OM == 9 & EM == 4) | (OM == 12 & EM == 1))

generate_correlation_plot(combined_data, title = "Cross-test", filename = "Correlation_plot_NAA_iid_mismatch.PNG")

#### NAA ar1y mismatch ####
manipulation <- function(data) {
  data$Match = data$OM %% 4
  data$Match[data$Match == 0] = 4
  data$Match = ifelse(data$Match == data$EM, TRUE, FALSE)
  data <- data[data$Match == FALSE,] %>%
    select(OM, EM, Rec_sigma_est,NAA_sigma_est, Rho_y_est, Rec_par_est) 
  return(data)
}

data1 <- dat1 %>% filter(OM %in% c(13,16))
data2 <- dat2 %>% filter(OM %in% c(13,16))
data3 <- dat3 %>% filter(OM %in% c(13,16))

data1 <- manipulation(data1) 
data2 <- manipulation(data2) 
data3 <- manipulation(data3) 

# Add the "Stock" column to each dataframe
data1$Stock <- "Flounder"
data3$Stock <- "Mackerel"
data2$Stock <- "Haddock"

# Combine the dataframes using rbind
data <- rbind(data1, data2, data3) %>%  
  mutate(BC = ifelse(OM == 13, "BC ON", "BC OFF"))

combined_data <- data %>%
  filter((OM == 13 & EM == 4) | (OM == 16 & EM == 1))

generate_correlation_plot(combined_data, title = "Cross-test", filename = "Correlation_plot_NAA_ar1y_mismatch.PNG")

# 1. SETUP: Load libraries and define inputs
# ============================================
library(tidyverse)

# Define stocks and their corresponding input files
STOCK_CONFIG <- list(
  Flounder = "Result1_F.RDS",
  Mackerel = "Result1_M.RDS",
  Haddock  = "Result1_H.RDS"
)

# Define the OM scenarios we want to analyze
SCENARIO_CONFIG <- list(
  BC_ON  = list(om_values = c(1, 5, 9, 13), id = 1),
  BC_OFF = list(om_values = c(4, 8, 12, 16), id = 4)
)


# 2. HELPER FUNCTION: Process data for one stock and one scenario
# ===============================================================
process_data <- function(file_path, stock_name, scenario) {
  # --- This is the corrected line ---
  readRDS(file_path) %>%
    filter(OM %in% scenario$om_values, EM %in% c(1, 4)) %>%
    mutate(
      Index = if_else(OM %in% c(1, 4, 5, 8), "Rec", "Rec+NAA")
    ) %>%
    # Reshape from wide to long to handle all age classes at once
    pivot_longer(
      cols = matches("^NAA_\\d+_(est|true)$"),
      names_to = c("age", ".value"),
      names_pattern = "NAA_(\\d+)_(est|true)"
    ) %>%
    # Calculate relative error for all ages
    summarise(
      NAA_relative = median((est / true) - 1, na.rm = TRUE),
      .by = c(Index, EM, nsim) # Group by these columns for the summary
    ) %>%
    mutate(
      Stock = stock_name,
      OM_ID = scenario$id # Tag data with the OM scenario ID (1 or 4)
    )
}


# 3. DATA PROCESSING: Loop through all stocks and scenarios
# =========================================================
all_results <- map_dfr(names(STOCK_CONFIG), function(stock_name) {
  map_dfr(SCENARIO_CONFIG, function(scenario) {
    process_data(STOCK_CONFIG[[stock_name]], stock_name, scenario)
  })
})


# 4. FINAL SUMMARIZATION: Calculate quantiles for plotting
# ========================================================
plot_data <- all_results %>%
  summarise(
    median_NAA_relative = median(NAA_relative, na.rm = TRUE),
    Q1 = quantile(NAA_relative, 0.25, na.rm = TRUE),
    Q3 = quantile(NAA_relative, 0.75, na.rm = TRUE),
    min_val = boxplot.stats(NAA_relative)$stats[1],
    max_val = boxplot.stats(NAA_relative)$stats[5],
    .by = c(Stock, Index, EM, OM_ID) # Group by these columns for the final summary
  ) %>%
  mutate(
    Type = if_else(EM == OM_ID, "Self-test", "Cross-test"),
    Type = paste0(Type, "\n", Index, " RE")
  )


# 5. PLOTTING FUNCTION: Define the plotting function once
# ======================================================
create_naa_plot <- function(data) {
  
  facet_levels <- c("Self-test\nRec RE", "Cross-test\nRec RE", 
                    "Self-test\nRec+NAA RE", "Cross-test\nRec+NAA RE")
  
  ggplot(data, aes(x = factor(EM), y = median_NAA_relative)) +
    facet_grid(Stock ~ factor(Type, levels = facet_levels), scales = "free_y") +
    
    # Manually draw boxplot components
    geom_rect(aes(xmin = as.numeric(factor(EM)) - 0.4, xmax = as.numeric(factor(EM)) + 0.4,
                  ymin = Q1, ymax = Q3), fill = "white", alpha = 0.5, color = "black") +
    geom_segment(aes(x = as.numeric(factor(EM)), xend = as.numeric(factor(EM)), y = min_val, yend = Q1)) +
    geom_segment(aes(x = as.numeric(factor(EM)), xend = as.numeric(factor(EM)), y = Q3, yend = max_val)) +
    geom_segment(aes(x = as.numeric(factor(EM)) - 0.4, xend = as.numeric(factor(EM)) + 0.4,
                     y = median_NAA_relative, yend = median_NAA_relative), linewidth = 1) +
    
    # Theming and labels
    geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
    scale_x_discrete(labels = c("1" = "BC-ON", "4" = "BC-OFF")) +
    labs(
      x = "Estimation Model", 
      y = "Median Relative Error", 
      title = "Numbers-at-Age"
    ) +
    theme_bw(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      axis.title = element_text(face = "bold"),
      axis.text.y = element_text(face = "bold"),
      strip.text = element_text(face = "bold", size = 10)
    )
}


# 6. GENERATE AND SAVE FINAL PLOT
# =================================
final_plot <- create_naa_plot(plot_data)

ggsave("Median_NAA_Combined.PNG", final_plot, width = 8, height = 6, dpi = 300)

print(final_plot)