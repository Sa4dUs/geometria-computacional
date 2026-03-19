# ============================================================
# File: sort-fast.R
#
# Authors:
#   - Marcelo Domínguez (@sa4dus)
#   - Alejandro García Prada (@AlexGarciaPrada)
#
# Created: 2026-03-17
# Last modified: 2026-03-19
# License: MIT
# ============================================================

quickSort <- function(v) {
  n <- length(v)
  if (n <= 1) return(v)
  
  stack <- matrix(NA, nrow = n, ncol = 2)
  top <- 1
  
  stack[top, ] <- c(1, n)
  
  while (top > 0) {
    range <- stack[top, ]
    top <- top - 1
    
    low <- range[1]
    high <- range[2]
    
    pivot <- v[high]
    i <- low - 1
    
    for (j in low:(high - 1)) {
      if (v[j] <= pivot) {
        i <- i + 1
        temp <- v[i]
        v[i] <- v[j]
        v[j] <- temp
      }
    }
    
    temp <- v[i + 1]
    v[i + 1] <- v[high]
    v[high] <- temp
    p <- i + 1
    
    if (p - 1 > low) {
      top <- top + 1
      stack[top, ] <- c(low, p - 1)
    }
    
    if (p + 1 < high) {
      top <- top + 1
      stack[top, ] <- c(p + 1, high)
    }
  }
  
  return(v)
}

mergeSort <- function(v) {
  n <- length(v)
  if (n <= 1) return(v)
  
  curr_size <- 1
  
  while (curr_size < n) {
    for (left in seq(1, n - 1, by = 2 * curr_size)) {
      
      mid <- min(left + curr_size - 1, n)
      right <- min(left + 2 * curr_size - 1, n)
      
      v <- merge(v, left, mid, right)
    }
    curr_size <- 2 * curr_size
  }
  
  return(v)
}

merge <- function(v, l, m, r) {
  if (m >= r) return(v)
  
  L <- v[l:m]
  R <- v[(m + 1):r]
  
  i <- 1
  j <- 1
  k <- l
  
  while (i <= length(L) && j <= length(R)) {
    if (L[i] <= R[j]) {
      v[k] <- L[i]
      i <- i + 1
    } else {
      v[k] <- R[j]
      j <- j + 1
    }
    k <- k + 1
  }
  
  if (i <= length(L)) {
    v[k:r] <- L[i:length(L)]
  } else if (j <= length(R)) {
    v[k:r] <- R[j:length(R)]
  }
  
  return(v)
}

heapSort <- function(v) {
  n <- length(v)
  if (n <= 1) return(v)
  
  for (i in seq(from = n %/% 2, to = 1, by = -1)) {
    v <- heapify(v, n, i)
  }
  
  for (i in seq(from = n, to = 2, by = -1)) {
    temp <- v[1]
    v[1] <- v[i]
    v[i] <- temp
    
    v <- heapify(v, i - 1, 1)
  }
  
  return(v)
}

heapify <- function(v, n, i) {
  parent <- i
  
  while (TRUE) {
    largest <- parent
    left <- 2 * parent
    right <- 2 * parent + 1
    
    if (left <= n && v[left] > v[largest]) {
      largest <- left
    }
    
    if (right <= n && v[right] > v[largest]) {
      largest <- right
    }
    
    if (largest == parent) {
      break
    }
    
    temp <- v[parent]
    v[parent] <- v[largest]
    v[largest] <- temp
    
    parent <- largest
  }
  
  return(v)
}

benchmark <- function(v_type, sizes, iter_std, iter_rec, sort_fns, is_collision = FALSE) {
  results <- list()
  for (n_val in sizes) {
    print(paste("[>] Running variant", v_type, "with size", n_val))
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
        
        if (it == 1) invisible(sort_fns[[name]](v))
        
        t <- system.time(sort_fns[[name]](v))[1]
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
  legend("topleft", legend = u_algos, col = colors, lty = 1, pch = 19, bty = "n")
}

main <- function() {
  sizes <- c(1000, 2500, 5000, 7500, 10000)
  c_ranges <- c(2, 5, 10, 50, 100, 200, 300, 400, 500)
  
  fns <- list(
    "merge_sort" = mergeSort,
    "heap_sort" = heapSort,
    "quick_sort" = quickSort
  )
  
  iters <- names(fns)
  
  # for (case in c("random", "sorted", "reverse")) {
  for (case in c("sorted", "reverse")) {
    res <- benchmark(case, sizes, 50, 0, fns[iters])
    plot_(res, iters, "Problem Size (n)")
  }
  
  res_coll <- benchmark(NULL, c_ranges, 50, 0, fns[iters], is_collision = TRUE)
  plot_(res_coll, iters, "Range (lower = more collisions)")
}

main()