# Analyze MacroSheds data
# Load libraries
library(ggplot2)
library(dplyr)
library(ggrepel)
library(here)
library(grid)
library(maps)


# Load processed data
processed_data <- readRDS(here("data/Processed/processed_macrosheds_data.rds"))


# Create the base plot with all domains
yearly_plot <- ggplot(data_with_slope, aes(x = water_year, y = mean_val)) +
    geom_line(size = 0.75, color = "black") +  # Single line for each domain
    geom_point(size = 0.5) +  # Add points to highlight yearly means
    # Add a red trendline using geom_smooth with a linear model
    geom_smooth(method = "lm", color = "red", linetype = "solid", se = FALSE, size = 0.75) +
    # Use facet_wrap to create a separate panel for each domain, ordered by total_obs
    facet_wrap(~ domain, scales = "free_y") +  # Separate by domain, free_y allows different y-axis scales
    # Annotate the total number of observations (n=__) in black for each domain
    geom_text(aes(label = paste0("n = ", scales::comma(total_obs))),
              x = 1975, y = Inf, hjust = 0, vjust = 2, color = "black", size = 1.75) +
    # Annotate the slope in red for each domain
    geom_text(aes(label = paste0("Slope: ", round(slope, 3))),
              x = 1975, y = Inf, hjust = 0, vjust = 3.5, color = "red", size = 1.75) +
    theme_minimal() +
    theme(
        legend.position = "none",  # Remove legend
        text = element_text(size = 9),  # Adjust text size
        plot.title = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
        panel.grid = element_blank(),  # Remove gridlines
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.25)  # Add a black border around each facet
    ) +
    labs(x = "Water Year", y = "Mean DOC", title = "DOC by Water Year")

# Return the plot
yearly_plot

# Create the faceted plot for all domains
faceted_plot <- create_faceted_yearly_mean_plot(mean_doc_by_domain_year)

# Save the plot
ggsave(here("figures/seasonal/water_year.png"), plot = faceted_plot, width = 8, height = 6, dpi = 300)


# Calculate the average VWM for each domain and water year
average_vwm_water_year <- vwm_results %>%
    group_by(domain, Water_Year) %>%
    summarize(Mean_VWM = mean(VWM, na.rm = TRUE), .groups = 'drop')  # Calculate the mean VWM

# Create a faceted plot for the average VWM by domain and water year
vwm_water_year_plot <- ggplot(average_vwm_water_year, aes(x = Water_Year, y = Mean_VWM, group = domain)) +
    geom_point(size = 2) +  # Add points for mean VWM
    geom_line(size = 1) +   # Connect points with lines
    #geom_smooth(method = "lm", color = "red", se = FALSE, linewidth = 0.5) +
    labs(title = "Average VWM DOC by Water Year and Domain",
         x = "Water Year",
         y = "Mean VWM DOC (mg/L)") +
    theme_minimal() +theme(legend.position = "none",  # Remove legend for clarity in faceting
                           axis.text.x = element_text(angle = 45, hjust = 1),
                           panel.border = element_rect(color = "black", fill = NA, size = 1),
                           axis.ticks = element_line(color = "black"),
                           axis.title = element_text(size = 14),
                           strip.background = element_rect(fill = "grey80", color = "black"),
                           strip.text = element_text(color = "black") ) + theme(legend.position = "none") +  # Remove legend for clarity in faceting
    facet_wrap(~ domain, nrow = 5, scales = "free")  # Unique y-axis for each facet

# Display the plot
print(vwm_water_year_plot)

# Save the average VWM by water year plot if desired
ggsave("figures/vwm/average_vwm_by_water_year_faceted_by_domain.png", plot = vwm_water_year_plot, width = 10, height = 10, dpi = 300)



##########################################################################################
####### scatterplot of water year vs DOC VWM with trendlines for sign. relationships ######

##### thinking through minimum data requirements first

# Step 1: Count the number of observations per site, year, and month
data_with_counts <- conus_doc_chem %>%
    mutate(year = year(date), month = month(date)) %>%
    group_by(site_code, year, month) %>%
    summarize(obs_per_month = n(), .groups = "drop")

# Step 2: Filter for each criterion

conus_doc_chem <- conus_doc_chem %>%
    mutate(year = year(date))  # Extract year from the date column

# Criterion 1: Minimum 2 observations per month for at least 10 months in a year
filtered_2_obs <- data_with_counts %>%
    filter(obs_per_month >= 2) %>%
    group_by(site_code, year) %>%
    summarize(months_with_2_obs = n_distinct(month), .groups = "drop") %>%
    filter(months_with_2_obs >= 10)



# Join back to the main data to get only those years that meet the criteria
filtered_data_2_obs <- conus_doc_chem %>%
    semi_join(filtered_2_obs, by = c("site_code", "year"))

# Criterion 2: Minimum 1 observation per month for at least 10 months in a year
filtered_1_obs <- data_with_counts %>%
    filter(obs_per_month >= 1) %>%
    group_by(site_code, year) %>%
    summarize(months_with_1_obs = n_distinct(month), .groups = "drop") %>%
    filter(months_with_1_obs >= 10)

# Join back to the main data to get only those years that meet the criteria
filtered_data_1_obs <- conus_doc_chem %>%
    semi_join(filtered_1_obs, by = c("site_code", "year"))

# Step 3: Compare data retention for each criterion
data_retention <- data.frame(
    Original_Data = nrow(conus_doc_chem),
    Filtered_2_Obs = nrow(filtered_data_2_obs),
    Filtered_1_Obs = nrow(filtered_data_1_obs)
)
site_retention <- data.frame(
    Original_Sites = n_distinct(conus_doc_chem$site_code),
    Filtered_2_Obs_Sites = n_distinct(filtered_data_2_obs$site_code),
    Filtered_1_Obs_Sites = n_distinct(filtered_data_1_obs$site_code)
)

print(data_retention)
# Original_Data   Filtered_2_Obs   Filtered_1_Obs
#    37743            20853           26741
print(site_retention)
#     89                32              60

############### going to move forward with Filtered_1_obs


library(ggplot2)
library(dplyr)


filtered_data_1_obs <- filtered_data_1_obs %>%
    mutate(
        water_year = if_else(month(date) >= 10, year(date) + 1, year(date))  # Add 1 to the year for months Oct-Dec
    )

# Step 1: Calculate VWM DOC for each site and water year using filtered_data_1_obs
vwm_doc <- filtered_data_1_obs %>%
    group_by(site_code, water_year) %>%
    summarize(
        VWM_DOC = sum(val.x * val.y, na.rm = TRUE) / sum(val.y, na.rm = TRUE),  # Calculate VWM DOC
        Avg_Q = mean(val.y, na.rm = TRUE),  # Calculate average discharge for each site and water year
        .groups = "drop"
    )

# Step 2: Determine significant trends for each site and water year
significant_trends <- vwm_doc %>%
    group_by(site_code) %>%
    summarize(
        p_value = tryCatch(
            summary(lm(VWM_DOC ~ water_year))$coefficients[2, 4],
            error = function(e) NA  # If error, set p_value to NA
        )
    ) %>%
    mutate(significant = !is.na(p_value) & p_value < 0.05)  # Significance level of 0.05

# Merge trend significance back into the VWM DOC data
vwm_doc <- vwm_doc %>%
    left_join(significant_trends, by = "site_code")

# Step 3: Join the original dataset to include site_fullname and domain
vwm_doc <- vwm_doc %>%
    left_join(filtered_data_1_obs %>%
                  select(site_code, site_fullname, domain) %>%
                  distinct(), by = "site_code")

# Step 4: Identify the last points for each significant trendline (only significant ones)
last_points <- vwm_doc %>%
    filter(significant == TRUE) %>%
    group_by(site_code) %>%
    slice_tail(n = 1) %>%  # Get the last point of each trendline
    ungroup() %>%
    select(site_code, site_fullname, domain, water_year, VWM_DOC)  # Select relevant columns

# Step 5: Check that last_points contains the correct number of sites
print(nrow(last_points))  # Should be 12 significant sites

# Step 6: Generate the list of significant sites with their slopes and p-values, and include domain and site_fullname
significant_sites_list <- significant_trends %>%
    filter(significant == TRUE) %>%
    select(site_code, p_value) %>%
    left_join(
        vwm_doc %>%
            group_by(site_code) %>%
            summarize(
                slope = summary(lm(VWM_DOC ~ water_year))$coefficients[2, 1],  # Get the slope
                site_fullname = first(site_fullname),  # Get the site_fullname (first in the group)
                domain = first(domain)  # Get the domain (first in the group)
            ),
        by = "site_code"  # Join by site_code
    )  # Merge the slope, site_fullname, and domain into the list of significant sites

# Display the significant sites and their slopes, p-values, domain, and site_fullname
print(significant_sites_list)

# Step 7: Create the scatterplot with trendlines (no labels on the trendlines) and labels for significant trendlines at the end
sig_trend_plot <- ggplot(vwm_doc, aes(x = water_year, y = VWM_DOC, group = site_code)) +
    geom_point(aes(color = significant), alpha = 0.6, size = 2) +
    scale_y_log10(labels = scales::label_number()) +
    geom_smooth(
        data = subset(vwm_doc, significant == TRUE),
        method = "lm", se = FALSE, color = "blue", linewidth = 0.75
    ) +  # Trendline only for significant trends
    scale_color_manual(values = c("grey", "black")) +  # Grey for non-significant, black for significant
    labs(
        x = "Water Year",
        y = "Log VWM DOC (mg/L)",
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
    # Add a label in the bottom-left corner with the number of significant trends
    annotate("text", x = min(vwm_doc$water_year), y = 0.1,
             label = paste("60 sites, 12 significant"),
             hjust = 0, vjust = 0, size = 4, color = "blue")

# Display the plot without the labels on the trendlines
print(sig_trend_plot)


ggsave(here("figures/Q/sig_trend_plot.png"), plot = sig_trend_plot, width = 9, height = 7, dpi = 300)


#################### same thing but parse out experimental vs non

# Step 1: Calculate VWM DOC for each site and water year using filtered_data_1_obs
vwm_doc <- filtered_data_1_obs %>%
    group_by(site_code, site_fullname, water_year) %>%
    summarize(
        VWM_DOC = sum(val.x * val.y, na.rm = TRUE) / sum(val.y, na.rm = TRUE),  # Calculate VWM DOC
        Avg_Q = mean(val.y, na.rm = TRUE),  # Calculate average discharge for each site and water year
        .groups = "drop"
    )

# Step 2: Determine significant trends for each site and water year
significant_trends <- vwm_doc %>%
    group_by(site_code) %>%
    summarize(
        p_value = tryCatch(
            summary(lm(VWM_DOC ~ water_year))$coefficients[2, 4],
            error = function(e) NA  # If error, set p_value to NA
        )
    ) %>%
    mutate(significant = !is.na(p_value) & p_value < 0.05)  # Significance level of 0.05

# Merge trend significance back into the VWM DOC data
vwm_doc <- vwm_doc %>%
    left_join(significant_trends, by = "site_code")

# Step 3: Join the original dataset to include site_fullname, domain, and ws_status (experimental vs non-experimental)
vwm_doc <- vwm_doc %>%
    left_join(filtered_data_1_obs %>%
                  select(site_code, site_fullname, domain, ws_status) %>%
                  distinct(), by = "site_code")

# Step 4: Create the scatterplot with trendlines, differentiating by ws_status and significance
sig_trend_plot2 <- ggplot(vwm_doc, aes(x = water_year, y = VWM_DOC, group = site_code)) +
    # Points: grey for non-significant sites, black for significant sites
    geom_point(aes(color = ifelse(significant, "black", "grey")), alpha = 0.6, size = 2) +
    scale_y_log10(labels = scales::label_number()) +
    # Trendlines: green for experimental sites with significant trends, blue for non-experimental sites
    geom_smooth(
        data = subset(vwm_doc, significant == TRUE),
        method = "lm", se = FALSE,
        aes(color = ifelse(ws_status == "experimental", "darkolivegreen3", "blue")),
        linewidth = 1.0
    ) +  # Trendline only for significant trends
    scale_color_manual(values = c("black", "blue", "darkolivegreen3", "grey")) +  # Assign color for each condition
    labs(
        x = "Water Year",
        y = "Log VWM DOC (mg/L)",
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
    # Facet by experimental vs non-experimental status
    facet_wrap(~ ws_status, scales = "free_y")
# Add a label in the bottom-left corner with the number of significant trends
#annotate("text", x = min(vwm_doc$water_year), y = 0.1,
#         label = paste("12 significant trends"),
#        hjust = 0, vjust = 0, size = 4, color = "blue")

# Display the plot
print(sig_trend_plot2)

ggsave(here("figures/Q/sig_trend_plot_experimental.png"), plot = sig_trend_plot2, width = 9, height = 6, dpi = 300)



####### add pH data to dataset ####
# retrieve pH data
ph_chem <- ms_load_product(
    my_ms_dir,
    prodname = 'stream_chemistry',
    filter_vars = 'pH'
)
# Rename 'val' to 'ph_val' in ph_chem
ph_chem <- ph_chem %>%
    rename(ph_val = val)

######################################################

# Water year VWM DOC and pH across time, ordered by the number of DOC measurements

# Step 1: Add water year to the dataset
vwm_doc_data <- vwm_doc_data %>%
    mutate(water_year = water_year(date))

# Step 2: Count the number of VWM DOC measurements and the number of sites per domain
domain_stats_vwm <- vwm_doc_data %>%
    group_by(domain) %>%
    summarize(
        vwm_doc_count = sum(!is.na(vwm_doc)),  # Count of VWM DOC measurements
        site_count = n_distinct(site_code)  # Number of unique sites
    )

# Step 3: Add domain statistics to the dataset before averaging
vwm_doc_data <- vwm_doc_data %>%
    left_join(domain_stats_vwm, by = "domain")  # Join the domain statistics

# Step 4: Reorder the domains based on the number of VWM DOC measurements
vwm_doc_data <- vwm_doc_data %>%
    mutate(domain = forcats::fct_reorder(domain, vwm_doc_count, .desc = TRUE))  # Reorder by VWM DOC count

# Step 5: Group by domain and water_year, and calculate the average values for vwm_doc and ph_val
avg_data_vwm <- vwm_doc_data %>%
    group_by(domain, water_year) %>%
    summarize(
        avg_vwm_doc = mean(vwm_doc, na.rm = TRUE),
        avg_ph = mean(ph_val, na.rm = TRUE),
        site_count = first(site_count)  # Retain the number of sites in each domain
    )

# Step 6: Plot VWM DOC and pH by water year with "sites=__" in each facet
doc_with_ph_vwm_water_year <- ggplot(avg_data_vwm, aes(x = water_year)) +
    geom_line(aes(y = avg_vwm_doc, color = "VWM DOC"), linewidth = 0.75) +    # Line for VWM DOC (left y-axis)
    geom_line(aes(y = avg_ph, color = "pH"), linewidth = 0.75) +      # Line for pH (right y-axis)
    facet_wrap(~ domain, scales = "free_y") +                      # Facet by domain
    labs(x = "Water Year", y = "VWM DOC (mg/L)", title = "VWM DOC and pH by Water Year") +
    theme_minimal() +
    scale_color_manual(name = "Variable", values = c("VWM DOC" = "black", "pH" = "red")) +
    theme(legend.position = "bottom") +
    scale_y_continuous(                                     # Primary y-axis for VWM DOC
        sec.axis = sec_axis(~ ., name = "pH")                 # Secondary y-axis for pH with no scaling
    ) +
    theme(panel.grid = element_blank(),  # Remove gridlines
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.25)  # Add a black border around each facet
    ) +
    # Step 7: Add "sites=__" in the top left of each facet
    geom_text(aes(x = -Inf, y = Inf, label = paste("sites =", site_count)),
              hjust = -0.1, vjust = 1.5, size = 2.3, color = "black")  # Position text in top left

# Display the plot
doc_with_ph_vwm_water_year
# Save the plot using the here package
ggsave(here("figures/temporal_analyses/doc_with_pH_vwm_water_year.png"), plot = doc_with_ph_vwm_water_year, width = 8, height = 6, dpi = 300)


# Calculate VWM pH
vwm_ph <- combined_ph %>%
    group_by(site_code, water_year) %>%
    summarize(
        VWM_pH = sum(ph_val * val, na.rm = TRUE) / sum(val, na.rm = TRUE),  # VWM pH
        Avg_Q = mean(val, na.rm = TRUE),  # Average discharge
        .groups = "drop"
    )

# Step 3: Determine significant trends for pH
significant_trends <- vwm_ph %>%
    group_by(site_code) %>%
    summarize(
        p_value = tryCatch(
            summary(lm(VWM_pH ~ water_year))$coefficients[2, 4],  # p-value for the slope
            error = function(e) NA  # Handle errors gracefully
        ),
        slope = tryCatch(
            summary(lm(VWM_pH ~ water_year))$coefficients[2, 1],  # Slope of the trendline
            error = function(e) NA  # Handle errors gracefully
        )
    ) %>%
    mutate(significant = !is.na(p_value) & p_value < 0.05)  # Significance threshold of 0.05

# Merge trend significance back into the VWM pH data
vwm_ph <- vwm_ph %>%
    left_join(significant_trends, by = "site_code")

# Step 4: Add site metadata
vwm_ph <- vwm_ph %>%
    left_join(
        ph_chem %>%
            select(site_code, site_fullname, domain) %>%
            distinct(),
        by = "site_code"
    )

# Step 5: Identify the last points for each significant trendline
last_points <- vwm_ph %>%
    filter(significant == TRUE) %>%
    group_by(site_code) %>%
    slice_tail(n = 1) %>%  # Get the last point of each trendline
    ungroup() %>%
    select(site_code, site_fullname, domain, water_year, VWM_pH)  # Relevant columns

# Step 6: Generate a scatterplot with trendlines for significant trends
ph_trend_plot <- ggplot(vwm_ph, aes(x = water_year, y = VWM_pH, group = site_code)) +
    geom_point(aes(color = significant), alpha = 0.6, size = 2) +  # Points colored by significance
    geom_smooth(
        data = subset(vwm_ph, significant == TRUE),
        method = "lm", se = FALSE, color = "blue", linewidth = 0.75
    ) +  # Trendlines for significant trends
    scale_color_manual(values = c("grey", "black")) +  # Grey for non-significant, black for significant
    labs(
        x = "Water Year",
        y = "VWM pH",
        color = "Trend Significance",
        title = "Scatterplot of VWM pH Trends Over Time"
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
    # Add the number of significant trends in the bottom-left corner
    annotate("text", x = min(vwm_ph$water_year, na.rm = TRUE), y = min(vwm_ph$VWM_pH, na.rm = TRUE),
             label = paste0(nrow(last_points), " sites with significant trends"),
             hjust = 0, vjust = 0, size = 4, color = "blue")

# Step 7: Save and display the plot
ggsave(here("figures/Q/ph_trend_plot.png"), plot = ph_trend_plot, width = 9, height = 7, dpi = 300)
print(ph_trend_plot)





