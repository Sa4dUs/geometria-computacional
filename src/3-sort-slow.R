# ============================================================
# File: 3-sort-slow.R
#
# Authors:
#   - Marcelo Domínguez (@sa4dus)
#   - Alejandro García Prada (@AlexGarciaPrada)
#
# Created: 2026-03-08
# Last modified: 2026-03-08
# License: MIT
# ============================================================

iterativeBubbleSort <- function (v) {
  n <- length(v)
  
  for (i in 1:(n-1)) {
    swap = FALSE
    for (j in 1:(n-i)) {
      if (v[j] > v[j+1]) {
        tmp <- v[j]
        v[j] <- v[j+1]
        v[j+1] <- tmp
        swap = TRUE
      }
    } 
    
    if (!swap) {
      break
    }
  }
  
  v
}

recursiveBubbleSort <- function(v, n = length(v)) {
  if (n <= 1) return(v)
  
  for (i in 1:(n-1)) {
    if (v[i] > v[i+1]) {
      tmp <- v[i]
      v[i] <- v[i+1]
      v[i+1] <- tmp
    }
  }
  
  recursiveBubbleSort(v, n-1)
}

iterativeInsertionSort <- function (v) {
  n <- length(v)
  
  for (i in 2:n) {
    key = v[i]
    j = i-1
    
    while ((j > 0) && (v[j] > key)) {
      v[j+1] = v[j]
      j = j-1
    }
    v[j+1] = key
  }
  
  v
}

recursiveInsertionSort <- function(v, n = length(v)) {
  if (n <= 1) {
    return(v)
  }
  
  v <- recursiveInsertionSort(v, n - 1)
  
  key <- v[n]
  j <- n - 1
  
  while (j > 0 && v[j] > key) {
    v[j + 1] <- v[j]
    j <- j - 1
  }
  
  v[j + 1] <- key
  
  return(v)
}

iterativeSelectionSort <- function(v) {
  n <- length(v)
  for (i in 1:(n-1)) {
    min_idx <- i
    
    for (j in (i+1):n) {
      if (v[j] < v[min_idx]) {
        min_idx <- j
      }
    }
    
    v[c(i, min_idx)] <- v[c(min_idx, i)]
  }
  
  v
}

recursiveSelectionSort <- function(v, start = 1) {
  n <- length(v)
  
  if (start >= n) {
    return(v)
  }
  
  min_idx <- start
  
  for (j in (start + 1):n) {
    if (v[j] < v[min_idx]) {
      min_idx <- j
    }
  }
  
  v[c(start, min_idx)] <- v[c(min_idx, start)]
  
  v <- recursiveSelectionSort(v, start + 1)
  
  return(v)
}

benchmark <- function(v_type, sizes, iter_std, iter_rec, sort_fns, is_collision = FALSE) {
  results <- list()
  for (n_val in sizes) {
    for (name in names(sort_fns)) {
      is_rec <- grepl("_rec", name)
      n_iter <- if (is_rec) iter_rec else iter_std
      times <- numeric(n_iter)
      
      for (it in 1:n_iter) {
        v <- if (is_collision) {
          sample(1:n_val, 1000, replace = TRUE)
        } else {
          switch(v_type,
                 "random"  = runif(n_val, 0, n_val),
                 "sorted"  = seq(1, n_val),
                 "reverse" = seq(n_val, 1))
        }
        
        if (it == 1) invisible(tryCatch(sort_fns[[name]](v), error = function(e) NULL))
        
        t <- tryCatch(system.time(sort_fns[[name]](v))[1], error = function(e) NA)
        if (is.na(t)) break
        times[it] <- t
      }
      
      results[[length(results) + 1]] <- data.frame(
        x = n_val, 
        Algorithm = name, 
        Time = mean(times)
      )
    }
  }
  do.call(rbind, results)
}

plot_ <- function(data, algos, x_label) {
  df <- data[data$Algorithm %in% algos, ]
  if (nrow(df) == 0) return(NULL)
  
  u_algos <- unique(df$Algorithm)
  colors <- rainbow(length(u_algos))
  names(colors) <- u_algos
  
  plot(NULL, xlim = range(df$x), ylim = c(0, max(df$Time) * 1.1),
       xlab = x_label, ylab = "Mean Time (seconds)", main = "")
  grid(col = "gray", lty = "dotted")
  
  for (i in seq_along(u_algos)) {
    sub <- df[df$Algorithm == u_algos[i], ]
    sub <- sub[order(sub$x), ]
    lines(sub$x, sub$Time, type = "b", pch = 18 + i, col = colors[i], lwd = 2)
  }
  legend("bottomright", legend = u_algos, col = colors, lty = 1, pch = 19, bty = "n")
}

main <- function() {
  sizes <- seq(100, 500, by = 100)
  c_ranges <- c(2, 5, 10, 50, 100, 200, 300, 400, 500)
  
  fns <- list(
    "bubble_iter" = iterativeBubbleSort, "bubble_rec" = recursiveBubbleSort,
    "insertion_iter" = iterativeInsertionSort, "insertion_rec" = recursiveInsertionSort,
    "selection_iter" = iterativeSelectionSort, "selection_rec" = recursiveSelectionSort
  )
  
  iters <- names(fns)[grepl("iter", names(fns))]
  
  res_rand <- benchmark("random", sizes, 50, 2, fns)
  plot_(res_rand, c("bubble_iter", "bubble_rec"), "Problem Size (n)")
  plot_(res_rand, c("insertion_iter", "insertion_rec"), "Problem Size (n)")
  plot_(res_rand, c("selection_iter", "selection_rec"), "Problem Size (n)")
  
  for (case in c("random", "sorted", "reverse")) {
    res <- benchmark(case, sizes, 50, 0, fns[iters])
    plot_(res, iters, "Problem Size (n)")
  }
  
  res_coll <- benchmark(NULL, c_ranges, 50, 0, fns[iters], is_collision = TRUE)
  plot_(res_coll, iters, "Range (lower = more collisions)")
}

main()