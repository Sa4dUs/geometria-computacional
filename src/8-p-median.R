# ============================================================
# File: 8-p-median.R
#
# Authors:
#   - Marcelo Domínguez (@sa4dus)
#   - Alejandro García Prada (@AlexGarciaPrada)
#
# Created: 2026-04-19
# Last modified: 2026-04-19
# License: MIT
# ============================================================

library(ggplot2)

read_ <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- trimws(lines)
  lines <- lines[lines != ""]
  
  metadata <- as.numeric(unlist(strsplit(lines[1], "\\s+")))
  
  n_total <- metadata[1]
  p_servers <- metadata[2]
  capacity <- metadata[3]
  
  node_values <- as.numeric(unlist(strsplit(lines[2:length(lines)], "\\s+")))
  nodes_matrix <- matrix(node_values, ncol = 4, byrow = TRUE)
  
  nodes_df <- data.frame(
    id = nodes_matrix[, 1],
    x = nodes_matrix[, 2],
    y = nodes_matrix[, 3],
    demand = nodes_matrix[, 4]
  )
  
  return(list(
    n = n_total,
    p = p_servers,
    c = capacity,
    nodes = nodes_df
  ))
}

plot_ <- function(instance) {
  df <- instance$nodes
  
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_point(aes(size = demand), alpha = 0.7) +
    theme_minimal() +
    coord_fixed() +
    theme(legend.position = "none")
  
  return(p)
}

greedy <- function(candidates, selectfn, feasfn, checkfn, ...) {
  candidates <- candidates
  solution   <- list()
  
  while (!checkfn(solution, ...) && length(candidates) > 0) {
    best_candidate <- selectfn(candidates, solution, ...)
    
    if (feasfn(best_candidate, solution, ...)) {
      solution <- append(solution, list(best_candidate))
    }
    
    candidates <- setdiff(candidates, best_candidate)
    
    if (is.null(best_candidate)) break
  }
  
  return(solution)
}

pmedian_calc <- function(selected, instance) {
  selected <- unlist(selected)
  if (length(selected) == 0) return(Inf)
  
  nodes <- instance$nodes
  all_coords <- as.matrix(nodes[, c("x", "y")])
  hubs_coords <- as.matrix(nodes[nodes$id %in% selected, c("x", "y"), drop = FALSE])
  
  if (nrow(hubs_coords) == 0) return(Inf)
  
  dist_matrix <- apply(hubs_coords, 1, function(h) {
    sqrt(colSums((t(all_coords) - h)^2))
  })
  
  if (is.null(dim(dist_matrix))) {
    min_dists <- dist_matrix
  } else {
    min_dists <- apply(dist_matrix, 1, min)
  }
  
  return(sum(min_dists))
}

pmedian_select <- function(candidates, currsol, instance) {
  alpha <- 0.05 
  
  costs <- sapply(candidates, function(cand) {
    pmedian_calc(c(unlist(currsol), cand), instance)
  })
  
  c_min <- min(costs)
  c_max <- max(costs)
  
  if (c_max == c_min) return(sample(candidates, 1))
  
  threshold <- c_min + alpha * (c_max - c_min)
  rcl <- candidates[costs <= threshold]
  
  if (length(rcl) > 1) {
    return(sample(rcl, 1))
  } else {
    return(rcl[1])
  }
}

pmedian_feas <- function(candidate, solution, instance) {
  return(length(solution) < instance$p)
}

pmedian_check <- function(solution, instance) {
  return(length(solution) >= instance$p)
}

pmedian <- function(instance) {
  n_iterations <- 100 
  best_total_ofv <- Inf
  
  for (i in 1:n_iterations) {
    sol <- greedy(
      candidates = instance$nodes$id,
      selectfn = pmedian_select,
      feasfn = pmedian_feas,
      checkfn = pmedian_check,
      instance = instance
    )
    
    current_ofv <- pmedian_calc(unlist(sol), instance)
    
    if (current_ofv < best_total_ofv) {
      best_total_ofv <- current_ofv
    }
  }
  
  return(best_total_ofv)
}

main <- function () {
  n <- 10
  dir <-"assets/practica8/" 
  files <- c("phub_50_5_1.txt", "phub_50_5_2.txt", "phub_50_5_3.txt", "phub_50_5_4.txt", "phub_50_5_5.txt")
  bkvf <- "best_values_pHub.txt"
  
  bkv_df <- read.table(paste0(dir, bkvf), col.names = c("name", "value"))
  for (file in files) {
    id = gsub(".txt", "", file)
    fpath <- paste0(dir, file)
    
    instance <- read_(fpath)
    print(plot_(instance))
    
    res <- numeric(n)
    
    for (i in 1:n) {
      res[i] <- pmedian(instance)
    }
    
    bkv <- bkv_df$value[bkv_df$name == id]
    
    best_ofv <- min(res)
    avg_ofv  <- mean(res)
    std_dev  <- sd(res)
    
    best_gap <- ((best_ofv - bkv) / bkv) * 100
    avg_gap  <- ((avg_ofv - bkv) / bkv) * 100
    
    cat(sprintf("\nINSTANCE ID: %s | BKV: %.2f\nBest OFV: %.2f (Gap: %.2f%%) Avg OFV: %.2f (Gap: %.2f%%)\nSD (Stability): %.4f\n", id, bkv, best_ofv, best_gap,avg_ofv, avg_gap, std_dev))
  }
}

main()