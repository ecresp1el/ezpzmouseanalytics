# Load necessary packages
if (!requireNamespace("R6", quietly = TRUE)) {
  install.packages("R6")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
}

library(R6)
library(dplyr)
library(ggplot2)
library(tidyr)

# Define the R6 class
MouseDataHandler <- R6Class("MouseDataHandler",
                            public = list(
                              # Class variables
                              data = NULL,
                              color_mapping = list(
                                "Rbp4-LMO3" = "pink",
                                "wt-LMO3" = "grey",
                                "climbing" = "blue",
                                "gen sniff/poke" = "green",
                                "direct sniff" = "orange",
                                "none" = "grey"
                              ),
                              
                              # Constructor to initialize the class with the data
                              initialize = function(data) {
                                self$data <- data
                              },
                              
                              # Method to convert start and end columns to minutes and seconds
                              convert_times = function() {
                                # Convert start and end time columns assuming they represent minutes and seconds
                                self$data <- self$data %>%
                                  mutate(
                                    start = as.numeric(format(strptime(start, "%M:%S"), "%M")) * 60 +
                                      as.numeric(format(strptime(start, "%M:%S"), "%S")),
                                    end = as.numeric(format(strptime(end, "%M:%S"), "%M")) * 60 +
                                      as.numeric(format(strptime(end, "%M:%S"), "%S"))
                                  )
                                message("Converted times to seconds.")
                              },
                              
                              # Method to display data
                              show_data = function(n = 10) {
                                print(head(self$data, n))
                              },
                              
                              # Method to get data for plotting or further analysis
                              get_data = function() {
                                return(self$data)
                              },
                              
                              # Method to plot a 1x2 box plot for day 2 and day 3
                              plot_interaction_time = function() {
                                # Calculate total interaction time for each entry
                                self$data <- self$data %>%
                                  mutate(interaction_time = end - start)
                                
                                # Remove rows with N/A values in relevant columns
                                self$data <- self$data %>%
                                  filter(!is.na(genotype), !is.na(interaction_time), !is.na(object))
                                
                                # Filter data for Day 2 and Day 3
                                day_2_data <- self$data %>% filter(day == 2)
                                day_3_data <- self$data %>% filter(day == 3)
                                
                                # Combine Day 2 and Day 3 data with a new 'day' column for plotting
                                combined_data <- bind_rows(
                                  mutate(day_2_data, day_label = "Day 2"),
                                  mutate(day_3_data, day_label = "Day 3")
                                )
                                
                                # Generate box plot
                                p <- ggplot(combined_data, aes(x = interaction(factor(object), genotype), y = interaction_time, fill = genotype)) +
                                  geom_boxplot() +
                                  facet_wrap(~ day_label, nrow = 1) +
                                  scale_fill_manual(values = self$color_mapping) +
                                  labs(
                                    title = "Interaction Time by Genotype and Object",
                                    x = "Object and Genotype",
                                    y = "Total Interaction Time (seconds)"
                                  ) +
                                  theme_minimal()
                                
                                print(p)
                              }
                            )
)

# Example usage:
# Load the dataset
library(readr)
added_columns_tidy_format_novel_object_updated <- read_csv("Documents/github_project_folder/ezpzmouseanalytics/data/processed/added_columns_tidy_format_novel_object_updated.csv")

# Function to filter data by mouse tag
filter_mouse_data <- function(data, mouse_tag) {
  filtered_data <- data %>% filter(tag == mouse_tag)
  return(filtered_data)
}

# Function to separate data by day and object
separate_data_by_day_and_object <- function(filtered_data) {
  separated_data <- filtered_data %>%
    group_by(day, object) %>%
    nest()
  return(separated_data)
}

# Function to create an overlaid bar plot (timeline) from separated data
plot_timeline <- function(separated_data) {
  # Flatten the nested tibble to make it easier to plot
  expanded_data <- separated_data %>% unnest(cols = c(data))
  
  # Convert object and day to factors for categorical representation
  expanded_data <- expanded_data %>%
    mutate(
      object = factor(object),
      day = factor(day)
    )
  
  # Create a timeline from 0 to 600 seconds and fill in missing behaviors
  complete_data <- expanded_data %>%
    group_by(day, object) %>%
    complete(start = seq(0, 600, by = 1), fill = list(behavior = "not measured")) %>%
    fill(day, object, .direction = "downup") %>%
    arrange(day, object, start)
  
  # Generate explicit start and end times for "not measured" sections
  complete_data <- complete_data %>%
    group_by(day, object) %>%
    mutate(
      end = lead(start, default = 600),
      behavior = ifelse(is.na(behavior), "not measured", behavior)
    ) %>%
    filter(start != end) # Remove rows where start equals end
  
  # Set up the plot with a facet for each day (1x2 layout)
  p <- ggplot(complete_data) +
    geom_rect(aes(xmin = start, xmax = end, ymin = as.numeric(object) - 0.4, ymax = as.numeric(object) + 0.4, fill = behavior), alpha = 0.8) +
    facet_wrap(~ day, ncol = 2) +
    theme_minimal() +
    theme(panel.grid = element_blank()) +
    scale_fill_manual(values = list(
      "climbing" = "blue",
      "gen sniff/poke" = "green",
      "direct sniff" = "orange",
      "none" = "lightgrey",
      "not measured" = "lightgrey"
    )) +
    labs(
      title = "Behavioral Timeline by Day and Object",
      x = "Time (seconds)",
      y = "Object"
    )
  
  print(p)
}

# Function to plot total time spent interacting with Object 1 vs Object 2 on Day 2 and Day 3
plot_total_interaction_time <- function(data) {
  # Calculate total interaction time for each entry
  data <- data %>%
    mutate(interaction_time = end - start)
  
  # Summarize total interaction time for each object and day
  summarized_data <- data %>%
    group_by(day, object) %>%
    summarize(total_interaction_time = sum(interaction_time, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(day_label = factor(day, levels = c(2, 3), labels = c("Day 2", "Day 3")))
  
  # Generate bar plot
  p <- ggplot(summarized_data, aes(x = factor(object, labels = c("Object 1", "Object 2")), y = total_interaction_time, fill = factor(day_label))) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("Day 2" = "skyblue", "Day 3" = "lightcoral")) +
    labs(
      title = "Total Interaction Time with Object 1 vs Object 2",
      x = "Object",
      y = "Total Interaction Time (seconds)",
      fill = "Day"
    ) +
    theme_minimal() +
    theme(panel.grid = element_blank())
  
  print(p)
}

# Plot total time spent interacting with Object 1 vs Object 2 on Day 2 and Day 3
plot_total_interaction_time(mouse_data_handler$get_data())

# Create an instance of MouseDataHandler
mouse_data_handler <- MouseDataHandler$new(added_columns_tidy_format_novel_object_updated)

# Convert the start and end times to minutes and seconds
mouse_data_handler$convert_times()

# Display the updated data
mouse_data_handler$show_data()

# Plot interaction time for Day 2 and Day 3
mouse_data_handler$plot_interaction_time()

# Get the modified data (if you want to use it for further operations)
updated_data <- mouse_data_handler$get_data()

# Use the function to filter data for a specific mouse tag
filtered_data <- filter_mouse_data(updated_data, "3427")
print(filtered_data)

# Separate the filtered data by day and object
separated_data <- separate_data_by_day_and_object(filtered_data)
print(separated_data)

# Plot the timeline from separated data
plot_timeline(separated_data)





#######
# Filter out rows with negative interaction times
negative_times <- added_columns_tidy_format_novel_object_updated %>%
  mutate(
    start_seconds = as.numeric(format(strptime(start, "%H:%M:%S"), "%H")) * 3600 +
      as.numeric(format(strptime(start, "%H:%M:%S"), "%M")) * 60 +
      as.numeric(format(strptime(start, "%H:%M:%S"), "%S")),
    end_seconds = as.numeric(format(strptime(end, "%H:%M:%S"), "%H")) * 3600 +
      as.numeric(format(strptime(end, "%H:%M:%S"), "%M")) * 60 +
      as.numeric(format(strptime(end, "%H:%M:%S"), "%S")),
    interaction_time = end_seconds - start_seconds
  ) %>%
  filter(interaction_time < 0)

# View rows with negative interaction times
print(negative_times)


