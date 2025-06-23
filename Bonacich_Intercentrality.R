# package install ---------------------------------------------------------
if(!require(pacman)){install.packages("pacman")}

p_load(
  igraph,
  Matrix
)
#   Load Adjacency Matrix   #
file_path <- "4/Axis_of_Resistance_weighted.csv"    # Binary vs weighted - Axis_of_Resistance_Binary.csv vs Axis_of_Resistance_weighted.csv

# Load the matrix (assumes square matrix with row/col labels)
A_df <- read.csv(file_path, row.names = 1, check.names = FALSE)
A <- as.matrix(A_df)

# Extract node names
node_names <- rownames(A)


# Bonacich Centrality -----------------------------------------------------
compute_bonacich <- function(A, lambda_val, v = rep(1, nrow(A))) {
  n <- nrow(A)
  I <- diag(n)
  
  eig_max <- max(eigen(A)$values)
  if (abs(lambda_val) >= 1 / eig_max) {
    stop("Lambda is too high; must be less than 1 / max eigenvalue.")
  }
  
  inv_matrix <- solve(I - lambda_val * A)
  BC <- inv_matrix %*% v
  return(as.vector(BC))
}


# Intercentrality ---------------------------------------------------------
compute_intercentrality <- function(A, lambda_val, v = rep(1, nrow(A))) {
  n <- nrow(A)
  BC_full <- compute_bonacich(A, lambda_val, v)
  inter_centrality <- numeric(n)
  
  for (i in 1:n) {
    A_reduced <- A
    A_reduced[i, ] <- 0
    A_reduced[, i] <- 0
    BC_reduced <- compute_bonacich(A_reduced, lambda_val, v)
    inter_centrality[i] <- BC_full[i] + sum(BC_full[-i] - BC_reduced[-i])
  }
  
  return(inter_centrality)
}


# Key player --------------------------------------------------------------
find_optimal_target <- function(A, node_names, lambda_val, v = rep(1, nrow(A))) {
  n <- nrow(A)
  BC_full <- compute_bonacich(A, lambda_val, v)
  inter_centrality <- compute_intercentrality(A, lambda_val, v)
  total_activity_full <- sum(BC_full)
  impact_scores <- numeric(n)
  
  for (i in 1:n) {
    A_reduced <- A
    A_reduced[i, ] <- 0
    A_reduced[, i] <- 0
    BC_reduced <- compute_bonacich(A_reduced, lambda_val, v)
    total_activity_reduced <- sum(BC_reduced)
    impact_scores[i] <- total_activity_full - total_activity_reduced
  }
  
  # Compute standard centrality measures
  g <- graph_from_adjacency_matrix(A, mode = "undirected", diag = FALSE)
  degree_centrality      <- degree(g, normalized = TRUE)
  closeness_centrality   <- closeness(g, normalized = TRUE)
  betweenness_centrality <- betweenness(g, normalized = TRUE)
  eigenvector_centrality <- eigen_centrality(g)$vector
  
  # Combine into results table
  results_df <- data.frame(
    Node = node_names,
    Bonacich.Centrality = round(BC_full, 4),
    Intercentrality = round(inter_centrality, 4),
    Degree.Centrality = round(degree_centrality, 4),
    Closeness.Centrality = round(closeness_centrality, 4),
    Betweenness.Centrality = round(betweenness_centrality, 4),
    Eigenvector.Centrality = round(eigenvector_centrality, 4)
  )
  
  # Prevent row names from printing
  rownames(results_df) <- NULL
  
  return(list(
    "Optimal Target Node to Remove" = node_names[which.max(inter_centrality)],
    "Results Data Frame" = results_df
  ))
}


# Parameters --------------------------------------------------------------
# Automatically determine lambda
eig_max <- max(eigen(A)$values)
lambda_val <- 0.99 / eig_max  # for stability

cat("Lambda value used in Bonacich centrality:", round(lambda_val, 6), "\n\n")

# #unweighted
# v <- rep(1, nrow(A))

#-----------comment out above or below for weighted vector---------------------------------------------------------------------------------

#weight by attacks on Israel
# Load Israel conflict summary
conflict_summary <- read.csv("4/conflict_with_israel_summary.csv", stringsAsFactors = FALSE)

v <- rep(0, nrow(A))
names(v) <- rownames(A)

for (i in seq_along(v)) {
  actor_name <- names(v)[i]
  match_row <- conflict_summary[conflict_summary$Node == actor_name, ]

  if (nrow(match_row) == 1) {
    v[i] <- match_row$log_conflicts_with_israel        # <-swap log for not  log_conflicts_with_israel vs Conflicts_with_Israel
  } else {                                          #  or could do log_casualties_with_israel vs Casualties_with_Israel
    v[i] <- 0
  }
}

# Run analysis
results <- find_optimal_target(A, node_names, lambda_val, v)


# Outoput -----------------------------------------------------------------
cat("Optimal Target Node to Remove:\n", results$`Optimal Target Node to Remove`, "\n\n")

cat("Network Analysis Results:\n")
print(results$`Results Data Frame`)
