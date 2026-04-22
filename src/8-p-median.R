# ============================================================
# File: 8-p-median.R
#
# Authors:
#    - Marcelo Domínguez (@sa4dus)
#    - Alejandro García Prada (@AlexGarciaPrada)
#
# Created: 2026-04-19
# Last modified: 2026-04-22
# License: MIT
# ============================================================

if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

read_ <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- trimws(lines[lines != ""])
  metadata <- as.numeric(unlist(strsplit(lines[1], "\\s+")))
  node_values <- as.numeric(unlist(strsplit(lines[2:length(lines)], "\\s+")))
  nodes_matrix <- matrix(node_values, ncol = 4, byrow = TRUE)
  nodes_df <- data.frame(
    id = nodes_matrix[, 1], x = nodes_matrix[, 2], 
    y = nodes_matrix[, 3], demand = nodes_matrix[, 4]
  )
  dist_mtx <- as.matrix(dist(nodes_df[, c("x", "y")]))
  return(list(n = metadata[1], p = metadata[2], nodes = nodes_df, dist_mtx = dist_mtx))
}

calc_ofv <- function(selected, dist_mtx) {
  if (length(selected) == 0) return(Inf)
  if (length(selected) == 1) return(sum(dist_mtx[, selected]))
  return(sum(do.call(pmin.int, as.data.frame(dist_mtx[, selected]))))
}

pmedian_standard <- function(instance) {
  sol <- c(); candidates <- 1:instance$n; dist_mtx <- instance$dist_mtx
  while (length(sol) < instance$p) {
    costs <- sapply(candidates, function(cand) calc_ofv(c(sol, cand), dist_mtx))
    best_idx <- which.min(costs)
    sol <- c(sol, candidates[best_idx])
    candidates <- candidates[-best_idx]
  }
  return(sol)
}

pmedian_shadow_density <- function(instance) {
  n <- instance$n; p <- instance$p; dist_mtx <- instance$dist_mtx
  demands <- instance$nodes$demand
  R <- mean(dist_mtx) * 0.2
  sol <- c(); candidates <- 1:n
  potential <- sapply(1:n, function(i) {
    neighbors <- which(dist_mtx[i, ] <= R)
    sum(demands[neighbors])
  })
  while (length(sol) < p) {
    best_idx <- candidates[which.max(potential[candidates])]
    sol <- c(sol, best_idx)
    candidates <- setdiff(candidates, best_idx)
    shadow_nodes <- which(dist_mtx[best_idx, ] <= R * 2)
    potential[shadow_nodes] <- potential[shadow_nodes] * 0.1
  }
  return(sol)
}

get_avg_time <- function(FUN, inst, reps = 100) {
  invisible(FUN(inst))
  
  gc(verbose = FALSE)
  
  start_cpu <- proc.time()
  for (i in 1:reps) {
    invisible(FUN(inst))
  }
  end_cpu <- proc.time()
  
  total_cpu <- (end_cpu[1] + end_cpu[2]) - (start_cpu[1] + start_cpu[2])
  
  return(total_cpu / reps)
}

plot_ <- function(results) {
  results$Speedup <- results$Standard / results$ShadowDensity
  
  coeff <- max(results$Speedup) / (max(results$Gap) + 5)
  
  p <- ggplot(results, aes(x = Instance)) +
    geom_bar(aes(y = Speedup, fill = "Speedup (x Times Faster)"), 
             stat = "identity", alpha = 0.8, width = 0.7) +
    geom_text(aes(y = Speedup, label = paste0("x", round(Speedup, 1))), 
              vjust = -0.5, fontface = "bold", size = 3.5) +
    geom_hline(aes(yintercept = 1), linetype = "dashed", color = "black", alpha = 0.5) +
    geom_line(aes(y = Gap * coeff, group = 1, color = "Quality Gap (%)"), size = 1.2) +
    geom_point(aes(y = Gap * coeff, color = "Quality Gap (%)"), size = 3) +
    geom_label(aes(y = Gap * coeff, label = paste0(round(Gap, 1), "%")), 
               vjust = 0.5, color = "#e74c3c", fontface = "bold", size = 3,
               label.size = NA, fill = alpha("white", 0.7)) +
    scale_y_continuous(
      name = "Speedup Factor (Bars)",
      limits = c(0, max(results$Speedup) * 1.1),
      sec.axis = sec_axis(transform = ~ . / coeff, name = "Quality Gap % (Line)")
    ) +
    scale_fill_manual(name = NULL, values = c("Speedup (x Times Faster)" = "#16a085")) +
    scale_color_manual(name = NULL, values = c("Quality Gap (%)" = "#e74c3c")) +
    theme_minimal() +
    labs(x = "") +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold", size = 16),
          axis.title.y.right = element_text(color = "#e74c3c", face = "bold"),
          axis.text.y.right = element_text(color = "#e74c3c"),
          panel.grid.minor = element_blank())
  
  print(p)
}

main <- function() {
  files <- c("phub_50_5_1.txt", "phub_50_5_2.txt", "phub_50_5_3.txt", "phub_50_5_4.txt", "phub_50_5_5.txt")
  dir <- "assets/practica8/" 
  results <- data.frame()
  
  cat("Starting benchmark (1000 reps per instance)...\n\n")
  header <- sprintf("%-15s | %-20s | %-20s | %-6s", "INSTANCE", "STANDARD GREEDY", "SHADOW-DENSITY", "GAP %")
  cat(header, "\n", paste0(rep("-", nchar(header)), collapse = ""), "\n")
  
  for (f in files) {
    fpath <- paste0(dir, f)
    if(!file.exists(fpath)) next
    inst <- read_(fpath)
    
    ofv_s <- calc_ofv(pmedian_standard(inst), inst$dist_mtx)
    ofv_i <- calc_ofv(pmedian_shadow_density(inst), inst$dist_mtx)
    gap <- ((ofv_i - ofv_s) / ofv_s) * 100
    
    t_s <- get_avg_time(pmedian_standard, inst)
    t_i <- get_avg_time(pmedian_shadow_density, inst)
    
    results <- rbind(results, data.frame(
      Instance = gsub(".txt", "", f),
      Standard = t_s,
      ShadowDensity = t_i,
      Gap = gap
    ))
    
    cat(sprintf("%-15s | %8.2f (%8.6fs) | %8.2f (%8.6fs) | %5.2f%%\n", 
                gsub(".txt", "", f), ofv_s, t_s, ofv_i, t_i, gap))
  }
  
  plot_(results)
}

main()