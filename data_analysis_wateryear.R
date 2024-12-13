# Analyze MacroSheds data
library(ggplot2)
library(dplyr)
library(scales)
library(here)
library(forcats)

############################################
# STEP 1: Load and Filter Data
############################################

# Load processed data
processed_data <- readRDS(here("data/Processed/processed_macrosheds_data.rds"))

# Step 1: Count the number of observations per site, year, and month
data_with_counts <- processed_data %>%
    mutate(year = year(date), month = month(date)) %>%
    group_by(site_code, year, month) %>%
    summarize(obs_per_month = n(), .groups = "drop")

# Step 2: Apply the filtering criterion - Minimum 1 observation per month for at least 10 months in a year
filtered_1_obs <- data_with_counts %>%
    filter(obs_per_month >= 1) %>%
    group_by(site_code, year) %>%
    summarize(months_with_1_obs = n_distinct(month), .groups = "drop") %>%
    filter(months_with_1_obs >= 10)

# Step 3: Join the filtered years back to the main dataset
filtered_data_1_obs <- processed_data %>%
    mutate(year = year(date)) %>%
    semi_join(filtered_1_obs, by = c("site_code", "year"))

# Step 4: Ensure water_year column is correctly retained
filtered_data_1_obs <- filtered_data_1_obs %>%
    mutate(water_year = if_else(month(date) >= 10, year(date) + 1, year(date)))

############################################
# STEP 2: Calculate VWM DOC and Analyze Trends
############################################

# Calculate Volume Weighted Mean (VWM) DOC for each site and water year
vwm_doc <- filtered_data_1_obs %>%
    group_by(site_code, water_year) %>%
    summarize(
        VWM_DOC = sum(val.x * val.y, na.rm = TRUE) / sum(val.y, na.rm = TRUE),  # Calculate VWM DOC
        Avg_Q = mean(val.y, na.rm = TRUE),  # Calculate average discharge
        .groups = "drop"
    )

# Step 2: Determine significant trends for VWM DOC
significant_trends <- vwm_doc %>%
    group_by(site_code) %>%
    summarize(
        p_value = tryCatch(
            summary(lm(VWM_DOC ~ water_year))$coefficients[2, 4],
            error = function(e) NA  # Handle errors gracefully
        ),
        slope = tryCatch(
            summary(lm(VWM_DOC ~ water_year))$coefficients[2, 1],
            error = function(e) NA
        ),
        .groups = "drop"
    ) %>%
    mutate(significant = !is.na(p_value) & p_value < 0.05)  # Significance threshold of 0.05

# Merge significance data back into the VWM DOC data
vwm_doc <- vwm_doc %>%
    left_join(significant_trends, by = "site_code")

############################################
# STEP 3: Add Site Metadata
############################################

# Join metadata (site_fullname, domain, ws_status) into the VWM DOC data
vwm_doc <- vwm_doc %>%
    left_join(
        filtered_data_1_obs %>%
            select(site_code, site_fullname, domain, ws_status) %>%
            distinct(),
        by = "site_code"
    )

############################################
# STEP 4: Identify Significant Trends
############################################

# Extract the last points for significant trendlines
last_points <- vwm_doc %>%
    filter(significant == TRUE) %>%
    group_by(site_code) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(site_code, site_fullname, domain, water_year, VWM_DOC)


# Generate a list of significant sites with slopes and p-values
significant_sites_list <- vwm_doc %>%
    filter(significant == TRUE) %>%
    group_by(site_code) %>%
    summarize(
        slope = first(slope),
        p_value = first(p_value),
        domain = first(domain),
        .groups = "drop"
    )

# Create a clean table for the console
significant_sites_list %>%
    kable(
        col.names = c("Site Code", "Slope", "P-value", "Domain"),
        caption = "Significant Trends by Site"
    ) %>%
    kable_styling(
        bootstrap_options = c("striped", "hover", "condensed"),
        full_width = FALSE
    )



############################################
# STEP 5: Create Scatterplot with Trends
############################################

# Calculate the total number of unique sites
n_total_sites <- vwm_doc %>%
    distinct(site_code) %>%
    nrow()

# Calculate the number of sites with significant trends
n_significant_sites <- vwm_doc %>%
    filter(significant == TRUE) %>%
    distinct(site_code) %>%
    nrow()

# Print the numbers for verification
print(paste("Total sites:", n_total_sites))
print(paste("Significant sites:", n_significant_sites))

# Scatterplot for VWM DOC trends
scatterplot_vwm_doc <- ggplot(vwm_doc, aes(x = water_year, y = VWM_DOC, group = site_code)) +
    geom_point(aes(color = significant), alpha = 0.6, size = 2) +
    scale_y_log10(labels = scales::label_number()) +
    geom_smooth(
        data = subset(vwm_doc, significant == TRUE),
        method = "lm", se = FALSE, color = "blue", linewidth = 0.75
    ) +
    scale_color_manual(values = c("grey", "black")) +
    labs(
        x = "Water Year",
        y = "Log VWM DOC (mg/L)",
        title = "Scatterplot of VWM DOC Trends Over Time",
        color = "Trend Significance"
    ) +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.title = element_text(size = 14),
        strip.background = element_rect(fill = "grey80", color = "black"),
        strip.text = element_text(color = "black")
    ) +
    annotate("text", x = min(vwm_doc$water_year), y = 0.1,
             label = paste(n_total_sites, "sites,", n_significant_sites, "significant"),
             hjust = 0, vjust = 0, size = 4, color = "blue")

# Save scatterplot
ggsave(here("output/analysis_figs/sig_trend.png"), plot = scatterplot_vwm_doc, width = 9, height = 7, dpi = 300)

############################################
# STEP 6: Scatterplot for Experimental vs Non-Experimental Sites
############################################

scatterplot_experimental <- ggplot(vwm_doc, aes(x = water_year, y = VWM_DOC, group = site_code)) +
    geom_point(aes(color = ifelse(significant, "black", "grey")), alpha = 0.6, size = 2) +
    scale_y_log10(labels = scales::label_number()) +
    geom_smooth(
        data = subset(vwm_doc, significant == TRUE),
        method = "lm", se = FALSE,
        aes(color = ifelse(ws_status == "experimental", "darkolivegreen3", "blue")),
        linewidth = 1.0
    ) +
    scale_color_manual(values = c("black", "blue", "darkolivegreen3", "grey")) +
    labs(
        x = "Water Year",
        y = "Log VWM DOC (mg/L)",
        title = "VWM DOC Trends for Experimental vs Non-Experimental Sites"
    ) +
    theme_minimal() +
    theme(
        legend.position = "none",  # Remove the legend
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.title = element_text(size = 14)
    ) +
    facet_wrap(~ ws_status, scales = "free_y")

# Save experimental plot
ggsave(here("output/analysis_figs/sig_trend_exp.png"), plot = scatterplot_experimental, width = 9, height = 6, dpi = 300)

# Generate a list of significant sites with slopes and p-values for experimental vs non-experimental
significant_sites_exp_vs_nonexp <- vwm_doc %>%
    filter(significant == TRUE) %>%
    group_by(site_code) %>%
    summarize(
        slope = first(slope),
        p_value = first(p_value),
        domain = first(domain),
        ws_status = first(ws_status),  # Include experimental vs non-experimental status
        .groups = "drop"
    )

# Create a clean table for the console with experimental vs non-experimental grouping
significant_sites_exp_vs_nonexp %>%
    kable(
        col.names = c("Site Code", "Slope", "P-value", "Domain", "Watershed Status"),
        caption = "Significant Trends by Site (Experimental vs Non-Experimental)"
    ) %>%
    kable_styling(
        bootstrap_options = c("striped", "hover", "condensed"),
        full_width = FALSE
    )

