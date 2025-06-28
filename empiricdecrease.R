# package install ---------------------------------------------------------
if(!require(pacman)){install.packages("pacman")}

p_load(
  tidyverse,
  ggraph,
  tidygraph,
  igraph,
  ggforce,
  concaveman,
  lubridate,
  zoo
)
# Split and clean associated actors
split_actors <- function(x) {
  if (is.na(x)) return(character(0))
  str_split(x, ",|;", simplify = FALSE) %>%
    unlist() %>%
    str_trim() %>%
    discard(~ .x == "")
}

# Merge actor names according to merge_rules and remove_patterns
merge_actor_name <- function(actor, merge_rules, remove_patterns) {
  if (is.na(actor) || actor == "") return(NA)
  
  if (any(str_detect(actor, regex(paste(remove_patterns, collapse = "|"), ignore_case = TRUE)))) {
    return(NA)
  }
  
  for (pat in names(merge_rules)) {
    if (str_detect(actor, regex(pat, ignore_case = TRUE))) {
      return(merge_rules[[pat]])
    }
  }
  
  return(actor)
}

# Merge associated actors (multiple per field)
merge_associated_actors <- function(assoc_actors, merge_rules, remove_patterns) {
  if (is.na(assoc_actors)) return(NA_character_)
  
  actors <- split_actors(assoc_actors)
  merged_actors <- sapply(actors, merge_actor_name, merge_rules = merge_rules, remove_patterns = remove_patterns)
  merged_actors <- merged_actors[!is.na(merged_actors)]
  
  if (length(merged_actors) == 0) {
    return(NA_character_)
  } else {
    return(paste(unique(merged_actors), collapse = "; "))
  }
}

# Get groups in network
groups_in_network <- rownames(empiric_network)

# Merge the actors in the event data
data <- data %>%
  mutate(
    merged_actor1 = sapply(actor1, merge_actor_name, merge_rules, remove_patterns),
    merged_actor2 = sapply(actor2, merge_actor_name, merge_rules, remove_patterns),
    merged_assoc_actor_1 = sapply(assoc_actor_1, merge_associated_actors, merge_rules, remove_patterns),
    merged_assoc_actor_2 = sapply(assoc_actor_2, merge_associated_actors, merge_rules, remove_patterns)
  )

# Identify if any group is involved in an event
event_has_merged_group <- function(actor1, actor2, assoc1, assoc2, groups) {
  actors <- c(actor1, actor2, split_actors(assoc1), split_actors(assoc2))
  actors <- actors[!is.na(actors)]
  any(actors %in% groups)
}

data$group_involved <- mapply(event_has_merged_group, 
                              data$merged_actor1, 
                              data$merged_actor2, 
                              data$merged_assoc_actor_1, 
                              data$merged_assoc_actor_2, 
                              MoreArgs = list(groups = groups_in_network))

# Filter events involving the groups
filtered_events <- filter(data, group_involved)

# Count number of events per day
event_counts <- filtered_events %>%
  group_by(event_date) %>%
  summarise(event_count = n()) %>%
  ungroup() %>%
  arrange(event_date)

# Calculate 7-day moving average
event_counts <- event_counts %>%
  mutate(moving_avg = zoo::rollmean(event_count, k = 7, fill = NA, align = "right"))

# Plot
ggplot(event_counts, aes(x = event_date)) +
  geom_bar(aes(y = event_count), stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_line(aes(y = moving_avg), color = "red", size = 1) +
  geom_vline(xintercept = as.numeric(as.Date("2024-09-17")), color = "grey40", linetype = "dashed", size = 1) +  #attack on hez
  geom_vline(xintercept = as.numeric(as.Date("2024-11-27")), color = "grey40", linetype = "dashed", size = 1) +  #cease fire with hez
  geom_vline(xintercept = as.numeric(as.Date("2024-12-08")), color = "grey40", linetype = "dashed", size = 1) +  #assad collapse
  geom_vline(xintercept = as.numeric(as.Date("2025-01-19")), color = "grey40", linetype = "dashed", size = 1) +  #cease-fire with hamas
  # geom_vline(xintercept = as.numeric(as.Date("2025-02-18")), color = "grey40", linetype = "dashed", size = 1) +  #Israel 'out of' lebanon
  geom_vline(xintercept = as.numeric(as.Date("2025-03-18")), color = "grey40", linetype = "dashed", size = 1) +  #cease-fires fall apart
  labs(title = "Number of Events per Day (Merged Groups) with 7-Day Moving Average",
       x = "Date",
       y = "Number of Events") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#check averages
# Ensure event_date is Date type
event_counts <- event_counts %>%
  mutate(event_date = as.Date(event_date))

# before IDF moved on hezbollah
period1_start <- as.Date("2024-06-17")
period1_end   <- as.Date("2024-09-17")

#after 
period2_start <- as.Date("2024-11-28")
period2_end   <- as.Date("2024-12-08")

# Filter and calculate averages
average_period1 <- event_counts %>%
  filter(event_date >= period1_start & event_date <= period1_end) %>%
  summarise(avg_events = mean(event_count, na.rm = TRUE)) %>%
  pull(avg_events)

average_period2 <- event_counts %>%
  filter(event_date >= period2_start & event_date <= period2_end) %>%
  summarise(avg_events = mean(event_count, na.rm = TRUE)) %>%
  pull(avg_events)

# Print the results
cat("Average number of events from", format(period1_start, "%Y-%B-%d"), 
    "to", format(period1_end, "%Y-%B-%d"), ":", average_period1, "\n")

cat("Average number of events from", format(period2_start, "%Y-%B-%d"), 
    "to", format(period2_end, "%Y-%B-%d"), ":", average_period2, "\n")
