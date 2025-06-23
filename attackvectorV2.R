# package install ---------------------------------------------------------
if(!require(pacman)){install.packages("pacman")}

p_load(
  tidyverse,
  ggraph,
  tidygraph,
  igraph,
  ggforce,
  concaveman,
  dplyr
)

# extract data ------------------------------------------------------------
data <- read.csv("2023-10-01-2025-04-20-Middle_East.csv")

# transform data ----------------------------------------------------------
data$event_date <- as.Date(data$event_date, format = "%d %B %Y")

# filter data -------------------------------------------------------------
data <- filter(data, event_date > as.Date("2023-10-06"))
data <- filter(data, event_date < as.Date("2024-09-17")) # Israel launches attack on Hezbollah
data <- filter(data, country == "Israel") # filter to attacks and deaths on/in Israel

data <- filter(data, !sub_event_type %in% c(
  "Agreement",
  "Change to group/activity",
  "Headquarters or base established",
  "Non-violent transfer of territory",
  "Other"
))
data <- filter(data, !interaction %in% c(
  "Civilians only",
  "Civilians-Civilians",
  "Protesters only",
  "Rioters only"
))


# separate associate actors -----------------------------------------------

# split and clean associated actors
split_actors <- function(x) {
  if (is.na(x)) return(character(0))
  str_split(x, ",|;", simplify = FALSE) %>%
    unlist() %>%
    str_trim() %>%
    discard(~ .x == "")
}

# Function to replace "(Palestine)" with "([admin1])" keeping the original actor name - #distinguish between gaza and west bank
replace_palestine <- function(actor_name, country, admin1) {
  if (country == "Palestine" && str_detect(actor_name, fixed("(Palestine)"))) {
    return(str_replace(actor_name, fixed("(Palestine)"), paste0("(", admin1, ")")))
  } else {
    return(actor_name)
  }
}

# Replace in actor1 and actor2
data <- data %>%
  rowwise() %>%
  mutate(
    actor1 = replace_palestine(actor1, country, admin1),
    actor2 = replace_palestine(actor2, country, admin1)
  ) %>%
  ungroup()

# Define exclusion categories - removing non-targeted actors caught in crossfire only
exclusion_patterns <- c(
  "^Aid Workers \\(",
  "Christian Group \\(",
  "^Civilians \\(",
  "Ethnic Group \\(",
  "^Farmers \\(",
  "^Fishers \\(",
  "^Health Workers \\(",
  "Jewish Group \\(",
  "^Journalists \\(",
  "^Labor Group \\(",
  "^Lawyers \\(",
  "^Migrants \\(",
  "Muslim Group \\(",
  "Police Forces",
  "^Prisoners \\(",
  "^Private Security Forces \\(",
  "^Refugees/IDPs \\(",
  "^Rioters \\(",
  "^Smugglers \\(",
  "^Students \\(",
  "^Teachers \\(",
  "^Women \\(",
  #General above - specific below
  "Communal Militia",
  "^Druze Goup \\(",
  "Government of Jordan \\(1999-\\) Jerusalem Islamic Waqf",
  "ICRC: International Committee of the Red Cross",
  "Islamic Group in Lebanon",
  "Islamic Health Society",
  "Islamic State",
  "Mine Action Foundation",
  "Military Forces of Egypt \\(2014-\\)",
  "Military Forces of Nepal \\(2022-\\)",
  "MSF: Doctors Without Borders",
  "Muslim Brotehrhood",
  "Norwegian People's Aid Organization",
  "Palestinian Committee of Prisoners' Affairs",
  "Palestinian National and Islamic Forces",
  "Palestinian Red Crescent Society",
  "Reconciliation Committee Members \\(Syria\\)",
  "SARC: Syrian Arab Red Crescent",
  "Settlement Emergency Squad",
  "Unidentified.*",
  "United Nations",
  "World Food Programme",
  "World Health Organization"
)

# Combine into a single regex pattern
exclusion_regex <- paste(exclusion_patterns, collapse = "|")

# Apply to assoc_actor_1 and assoc_actor_2
data <- data %>%
  rowwise() %>%
  mutate(
    assoc_actor_1 = {
      actors <- split_actors(assoc_actor_1)
      actors <- map_chr(actors, ~ replace_palestine(.x, country, admin1))
      actors <- discard(actors, ~ str_detect(.x, exclusion_regex))  
      if (length(actors) > 0) paste(actors, collapse = "; ") else NA_character_
    },
    assoc_actor_2 = {
      actors <- split_actors(assoc_actor_2)
      actors <- map_chr(actors, ~ replace_palestine(.x, country, admin1))
      actors <- discard(actors, ~ str_detect(.x, exclusion_regex))  
      if (length(actors) > 0) paste(actors, collapse = "; ") else NA_character_
    }
  ) %>%
  ungroup()

# # Result: edited data
# write.csv(data, "4/edited_data.csv", row.names = TRUE)

# Extract all unique actors 
all_actors <- unique(c(
  data$actor1,
  data$actor2,
  unlist(map(data$assoc_actor_1, split_actors)),
  unlist(map(data$assoc_actor_2, split_actors))
))
all_actors <- sort(na.omit(unique(all_actors)))

# conflict matrix ---------------------------------------------------------

# Create empty square matrix
conflict_matrix <- matrix(0, nrow = length(all_actors), ncol = length(all_actors),
                          dimnames = list(all_actors, all_actors))

# Loop through each event
for (i in 1:nrow(data)) {
  side1 <- c(data$actor1[i], split_actors(data$assoc_actor_1[i]))
  side2 <- c(data$actor2[i], split_actors(data$assoc_actor_2[i]))
  
  side1 <- side1[!is.na(side1) & side1 != ""]
  side2 <- side2[!is.na(side2) & side2 != ""]
  
  if (length(side1) > 0 & length(side2) > 0) {
    # Standard conflict between side1 and side2
    for (a1 in side1) {
      for (a2 in side2) {
        conflict_matrix[a1, a2] <- conflict_matrix[a1, a2] + 1
        conflict_matrix[a2, a1] <- conflict_matrix[a2, a1] + 1  # Symmetric
      }
    }
  } else if (length(side1) > 0 & length(side2) == 0 ) {
    # Conflict against blank actor2 -> conflict with Country
    country_actor <- paste0(data$country[i],data$admin1[i])
    
    # If country is not already in the matrix, add it
    if (!(country_actor %in% rownames(conflict_matrix))) {
      conflict_matrix <- rbind(conflict_matrix, setNames(rep(0, ncol(conflict_matrix)), colnames(conflict_matrix)))
      conflict_matrix <- cbind(conflict_matrix, setNames(rep(0, nrow(conflict_matrix)), rownames(conflict_matrix)))
      rownames(conflict_matrix)[nrow(conflict_matrix)] <- country_actor
      colnames(conflict_matrix)[ncol(conflict_matrix)] <- country_actor
    }
    
    for (a1 in side1) {
      conflict_matrix[a1, country_actor] <- conflict_matrix[a1, country_actor] + 1
      conflict_matrix[country_actor, a1] <- conflict_matrix[country_actor, a1] + 1
    }
  }
}

# Result: conflict_matrix is symmetric and square
write.csv(conflict_matrix, "4/av_conflict_matrix.csv", row.names = TRUE)

# narrow to target groups -------------------------------------------------

# Define partial match patterns
target_keywords <- c("Israel")

# Find all actors whose names contain any of the target keywords (case-insensitive)
target_actors <- rownames(conflict_matrix)[
  str_detect(rownames(conflict_matrix), regex(paste(target_keywords, collapse = "|"), ignore_case = TRUE))
]

# Find all actors with >0 conflicts with any of those target actors
connected_actors <- which(rowSums(conflict_matrix[, target_actors, drop = FALSE]) > 0)

# Include the target actors themselves
filtered_actor_names <- sort(unique(c(target_actors, rownames(conflict_matrix)[connected_actors])))

# Subset the matrix
sub_conflict_matrix <- conflict_matrix[filtered_actor_names, filtered_actor_names]

# Export
write.csv(sub_conflict_matrix, "4/av_sub_conflict_matrix", row.names = TRUE)


# casualty_matrix -----------------------------------------------------

# Initialize casualty_matrix similar to conflict_matrix
casualty_matrix <- matrix(0, nrow = nrow(conflict_matrix), ncol = ncol(conflict_matrix))
rownames(casualty_matrix) <- rownames(conflict_matrix)
colnames(casualty_matrix) <- colnames(conflict_matrix)

# Loop through each event
casualty_matrix <- matrix(0, nrow = nrow(conflict_matrix), ncol = ncol(conflict_matrix))
rownames(casualty_matrix) <- rownames(conflict_matrix)
colnames(casualty_matrix) <- colnames(conflict_matrix)

# Loop through each event  - only use primary actor_1 and actor_2 otherwise double counting across groups
for (i in 1:nrow(data)) {
  side1 <- data$actor1[i]
  side2 <- data$actor2[i]
  
  side1 <- side1[!is.na(side1) & side1 != ""]
  side2 <- side2[!is.na(side2) & side2 != ""]
  
  fatalities <- ifelse(is.na(data$fatalities[i]), 0, data$fatalities[i])
  
  if (length(side1) > 0 & length(side2) > 0) {
    casualty_matrix[side1, side2] <- casualty_matrix[side1, side2] + fatalities
    casualty_matrix[side2, side1] <- casualty_matrix[side2, side1] + fatalities
  } else if (length(side1) > 0 & length(side2) == 0) {
    country_actor <- paste0(data$country[i], data$admin1[i])
    
    casualty_matrix[side1, country_actor] <- casualty_matrix[side1, country_actor] + fatalities
    casualty_matrix[country_actor, side1] <- casualty_matrix[country_actor, side1] + fatalities
  }
}

write.csv(casualty_matrix, "4/av_casualty_matrix.csv", row.names = TRUE)

# Find all actors whose names contain any of the target keywords (case-insensitive)
target_actors <- rownames(casualty_matrix)[
  str_detect(rownames(casualty_matrix), regex(paste(target_keywords, collapse = "|"), ignore_case = TRUE))
]

# Find all actors with >0 conflicts with any of those target actors
connected_actors <- which(rowSums(casualty_matrix[, target_actors, drop = FALSE]) > 0)

# Include the target actors themselves
filtered_actor_names <- sort(unique(c(target_actors, rownames(casualty_matrix)[connected_actors])))

# Subset the matrix
sub_casualty_matrix <- casualty_matrix[filtered_actor_names, filtered_actor_names]

# Export
write.csv(sub_casualty_matrix, "4/av_sub_casualty_matrix.csv", row.names = TRUE)


# merge/remove actors ------------------------------------------------------------

# Define merge and remove rules
merge_rules <- list(
  #Abu Ali Mustafa Brigades
  "^PFLP" = "Abu Ali Mustafa Brigades (PJOR) (merged)",
  #Al-Aqsa Martyrs' Brigades - Fatah Movement
  "^AAMB" = "Al-Aqsa Martyrs' Brigades (PJOR) (merged)",
  "^Al Aqsa" = "Al-Aqsa Martyrs' Brigades (PJOR) (merged)",
  "Abu Ali Iyad Brigade" = "Al-Aqsa Martyrs' Brigades (PJOR) (merged)",
  "Abdul al-Qadir al-Husseini Brigades" = "Al-Aqsa Martyrs' Brigades (PJOR) (merged)",
  "Fatah Movement" = "Al-Aqsa Martyrs' Brigades (PJOR) (merged)",
  "Amjad al Fakhouri Brigades - Jaba" = "Al-Aqsa Martyrs' Brigades (PJOR) (merged)",
  #Ba'athist Syria
  "^Military Forces of Syria \\(2000-2024\\).*" = "Ba'athist Syria (merged)",
  "Ba'ath Party \\(Syria\\)" = "Ba'athist Syria (merged)",
  "Police Forces of Syria \\(2000-2024\\)" = "Ba'athist Syria (merged)",
  #pro-Assad militia - categorized with baathist due to significant cooperation with Assad but low overall conflict in network
  "Popular Resistance for the Liberation of the Golan \\(Syria\\)" = "Ba'athist Syria (merged)",
  "Qaterji Militia" = "Ba'athist Syria (merged)",
  "Sraya Al-Areen 313" = "Ba'athist Syria (merged)",
  "Syrian Social Nationalist Party" = "Ba'athist Syria (merged)",
  "The Galilee Forces - Lone Wolves" = "Ba'athist Syria (merged)",
  #Egypt
  "Military Forces of Egypt \\(2014-\\)" = "Egypt (merged)",
  #Gaza
  "Civilians \\(Gaza Strip\\)" = "Gaza (merged)",
  "PalestineGaza Strip" = "Gaza (merged)",
  #Government of Syria \\(2024-\\)
  "Communal Militia \\(Syria\\)" = "Government of Syria (2024) (merged)",
  "Government of Syria \\(2024-\\)" = "Government of Syria (2024) (merged)",
  "Military Forces of Syria \\(2024-\\)" = "Government of Syria (2024) (merged)",
  "^Syria" = "Government of Syria (2024) (merged)",
  #Hamas
  "Hamas" = "Hamas (PJOR) (merged)",
  "Police Forces of Palestine \\(2007-\\) Gaza Strip" = "Hamas (PJOR) (merged)",
  #Hezbollah
  "Amal Movement" = "Hezbollah (merged)",
  "Arab Socialist Baath Party in Lebanon" = "Hezbollah (merged)",
  "^Hezbollah" = "Hezbollah (merged)",
  "^Lebanon" = "Hezbollah (merged)",   #mostly israel shelling at Hezbollah positions
  "Imam al Mahdi Scouts" = "Hezbollah (merged)",
  "Islamic Risala Scout Association" = "Hezbollah (merged)",
  "Islamic Group in Lebanon" = "Hezbollah (merged)",
  "Lebanese Resistance Brigades" = "Hezbollah (merged)",
  #Houthi
  "Civilians \\(Yemen\\)" = "Houthi (merged)",
  "Military Forces of Yemen \\(2017-\\) Houthi" = "Houthi (merged)",
  "^Yemen" = "Houthi (merged)",
  #Islamic State
  "^Islamic State" = "Islamic State (merged)",
  #Iran
  "Civilians \\(Iran\\)" = "Iran (merged)",
  "^Military Forces of Iran" = "Iran (merged)",
  "^Government of Iran" = "Iran (merged)",
  "^Iran" = "Iran (merged)",
  #Lebanon
  "Lebanon \\(2021-\\)" = "Lebanon (merged)",
  "Civilians \\(Lebanon\\)" = "Lebanon (merged)",
  #Lions den
  "^Lions' Den" = "Lions' Den (merged)",
  #Mujahideen Brigades
  "Mujahideen Brigades" = "Mujahideen Brigades (PJOR) (merged)",
  #Nasser Salah al-Din Brigades
  "Al Nasser Salah al Deen Brigades" = "Nasser Salah al-Din Brigades (PJOR) (merged)",
  "Lewa al-Tawhid" = "Nasser Salah al-Din Brigades (PJOR) (merged)",            #a syrian group with the same name exists
  "Popular Resistance Movement" = "Nasser Salah al-Din Brigades (PJOR) (merged)",
  #National Resistance Brigades
  "DFLP: Democratic Front for the Liberation of Palestine" = "National Resistance Brigades (PJOR) (merged)",
  "National Resistance Brigades" = "National Resistance Brigades (PJOR) (merged)",
  #Other PJOR
  "Al Ahrar: Palestininian Freedom Movement" = "Other PJOR (merged)",
  ".*Jund Allah" = "Other PJOR (merged)",
  "Junud Allah - Dayr al Ghusun" = "Other PJOR (merged)",
  "Katibat Khalil ar Rahman" = "Other PJOR (merged)",
  "Katibat Mukhayyam al Farah" = "Other PJOR (merged)",
  "Katibat Mukhayyam Qalandia" = "Other PJOR (merged)",
  #Palestinian Islamic Jihad (PJOR)
  "^PIJ:" = "Palestinian Islamic Jihad (PJOR) (merged)",
  #Popular Mobilization Forces
  "Asaib Ahl Al Haq" = "Popular Mobilization Forces (merged)",
  "Imam Ali Brigades \\(Syria\\)" = "Popular Mobilization Forces (merged)",
  "Kataib Hezbollah \\(Iraq\\)" = "Popular Mobilization Forces (merged)",
  "Military Forces of Iraq \\(2022-\\) Popular Mobilization Forces" = "Popular Mobilization Forces (merged)",
  "Salah al-Din Brigade" = "Popular Mobilization Forces (merged)",
  "Saraya al-Ashura" = "Popular Mobilization Forces (merged)",
  #Pro-Iran Militia
  "Harakat Hezbollah Al Nujaba" = "Pro-Iran Militia (merged)",       #originally PMF now IRIQ
  "Islamic Resistance in Iraq" = "Pro-Iran Militia (merged)",
  "Islamic Resistance in the Land of Two Holy Mosques"= "Pro-Iran Militia (merged)",
  "LAFA: Abu Al Fadhal al Abbas Brigade" = "Pro-Iran Militia (merged)",
  "Militia \\(Pro-Iran\\)" = "Pro-Iran Militia (merged)",
  "SWAT Arabian Island" = "Pro-Iran Militia (merged)",
  #Russia
  "Military Forces of Russia \\(2000-\\)" = "Russia (merged)",
  #Turkey
  "Civilians \\(Turkey\\)" = "Turkey (merged)",
  "Police Forces of Turkey \\(2016-\\)" = "Turkey (merged)",
  #West Bank
  "Civilians \\(West Bank\\)" = "West Bank (merged)",
  "PalestineWest Bank" =  "West Bank (merged)",
  "Palestine \\(1994-\\)" = "West Bank (merged)",
  #Israel
  "^Government of Israel \\(2022" = "Israel (merged)",
  "^Hadash" = "Israel (merged)",
  "^Israel" = "Israel (merged)", 
  "Military Forces of Israel \\(2022" = "Israel (merged)",
  "Police Forces of Israel \\(2022" = "Israel (merged)",
  "Private Security Forces \\(Israel\\)" = "Israel (merged)",
  "Settlement Emergency Squad" = "Israel (merged)",
  "Settlers" = "Israel (merged)",
  ".*\\(Israel\\)" = "Israel (merged)"
)

remove_patterns <- c(
  "Civilians \\(Syria\\)",  # removes Syrian civilians who appear on both sides of syrian civil conflict and hard to classify
  "Civilians",       # removes civilians from countries not in conflict but caught in war (e.g. Bangledeshi in Lebanon)
  "Muslim Brotherhood",
  "Smugglers",
  "Unidentified.*",
  "United Nations",
  "Egypt \\(merged\\)",   #low activity in network
  "Islamic State \\(merged\\)",    #low actiivty -mostly defunct - arrested by  turkey mostly
  "Turkey \\(merged\\)" #low activity - mostly defunct - arrests islamic state and various international citizens crossing border mostly
)

# Function to merge and remove actor rows/columns in a symmetric matrix
merge_matrix_actors <- function(mat, merge_rules, remove_patterns = NULL) {
  original_names <- rownames(mat)
  merged_labels <- character(length(original_names))
  
  # First: Merge according to rules
  for (i in seq_along(original_names)) {
    label <- original_names[i]
    matched <- FALSE
    for (pat in names(merge_rules)) {
      if (str_detect(label, regex(pat, ignore_case = TRUE))) {
        merged_labels[i] <- merge_rules[[pat]]
        matched <- TRUE
        break
      }
    }
    # Keep original if not matched
    if (!matched) {
      merged_labels[i] <- label
    }
  }
  
  # Create new unique labels after merging
  unique_labels <- sort(unique(merged_labels))
  new_mat <- matrix(0, nrow = length(unique_labels), ncol = length(unique_labels),
                    dimnames = list(unique_labels, unique_labels))
  
  # Fill in the new matrix
  for (i in seq_along(original_names)) {
    for (j in seq_along(original_names)) {
      new_i <- merged_labels[i]
      new_j <- merged_labels[j]
      new_mat[new_i, new_j] <- new_mat[new_i, new_j] + mat[original_names[i], original_names[j]]
    }
  }
  
  # Then: Remove rows/columns based on remove_patterns
  if (!is.null(remove_patterns)) {
    remove_idx <- which(str_detect(rownames(new_mat), regex(paste(remove_patterns, collapse = "|"), ignore_case = TRUE)))
    if (length(remove_idx) > 0) {
      new_mat <- new_mat[-remove_idx, -remove_idx, drop = FALSE]
    }
  }
  
  return(new_mat)
}

# function to merge, clean, and export a matrix
process_and_export_matrix <- function(matrix, name, merge_rules, remove_patterns, output_dir = "4") {
  merged <- merge_matrix_actors(matrix, merge_rules, remove_patterns)
  nonzero_actors <- which(rowSums(merged) > 0 | colSums(merged) > 0)
  merged <- merged[nonzero_actors, nonzero_actors]
  write.csv(merged, file.path(output_dir, paste0("av_merged_", name, "_matrix.csv")), row.names = TRUE)
  return(merged)
}

# List of matrices to process
matrix_list <- list(
  conflict = sub_conflict_matrix,
  casualty = sub_casualty_matrix
)

# Process all matrices
for (name in names(matrix_list)) {
  merged_matrix <- process_and_export_matrix(matrix_list[[name]], name, merge_rules, remove_patterns)
  assign(paste0("av_merged_", name, "_matrix"), merged_matrix, envir = .GlobalEnv)
}

# Set diagonal to 0 (no self-loops)
diag(av_merged_conflict_matrix) <- 0
diag(av_merged_casualty_matrix) <- 0


# Clean row and column names
clean_names <- function(x) {
  str_replace_all(x, regex(" ?\\(merged\\)", ignore_case = TRUE), "")
}

# Clean names for both matrices
rownames(av_merged_conflict_matrix) <- clean_names(rownames(av_merged_conflict_matrix))
colnames(av_merged_conflict_matrix) <- clean_names(colnames(av_merged_conflict_matrix))
rownames(av_merged_casualty_matrix) <- clean_names(rownames(av_merged_casualty_matrix))
colnames(av_merged_casualty_matrix) <- clean_names(colnames(av_merged_casualty_matrix))

# Convert to dataframes with node name as a column
conflict_df <- av_merged_conflict_matrix %>%
  as.data.frame() %>%
  rownames_to_column(var = "Node")

casualty_df <- av_merged_casualty_matrix %>%
  as.data.frame() %>%
  rownames_to_column(var = "Node")

# Extract columns related to Israel
conflict_with_israel <- conflict_df %>%
  select(Node, Conflicts_with_Israel = Israel)

casualty_with_israel <- casualty_df %>%
  select(Node, Casualties_with_Israel = Israel)

# Merge the two
summary_table <- left_join(conflict_with_israel, casualty_with_israel, by = "Node") %>%
  mutate(
    Conflicts_with_Israel = replace_na(Conflicts_with_Israel, 0),
    Casualties_with_Israel = replace_na(Casualties_with_Israel, 0),
    log_conflicts_with_israel = log(Conflicts_with_Israel + 1),
    log_casualties_with_israel = log(Casualties_with_Israel + 1)
  ) %>%
  filter(Node != "Israel")

# View the result
print(summary_table)

write.csv(summary_table, "4/conflict_with_israel_summary.csv", row.names = TRUE)