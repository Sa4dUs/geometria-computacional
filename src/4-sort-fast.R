quickSort <- function(v) {
  if (length(v) <= 1) return(v)
  
  mid <- sample(v, 1)
  
  left  <- v[v <  mid]
  equal <- v[v == mid]
  right <- v[v >  mid]
  
  c(quickSort(left), equal, quickSort(right))
}

heapSort <- function(v) {
  n <- length(v)
  
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
  largest <- i
  l <- 2 * i
  r <- 2 * i + 1
  
  if (l <= n && v[l] > v[largest]) {
    largest <- l
  }
  
  if (r <= n && v[r] > v[largest]) {
    largest <- r
  }
  
  if (largest != i) {
    temp <- v[i]
    v[i] <- v[largest]
    v[largest] <- temp
    
    v <- heapify(v, n, largest)
  }
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
    "merge_sort" = mergeSort,
    "heap_sort" = heapSort,
    "quick_sort" = quickSort
  )
  
  iters <- names(fns)
  
  for (case in c("random", "sorted", "reverse")) {
    res <- benchmark(case, sizes, 50, 0, fns[iters])
    plot_(res, iters, "Problem Size (n)")
  }
  
  res_coll <- benchmark(NULL, c_ranges, 50, 0, fns[iters], is_collision = TRUE)
  plot_(res_coll, iters, "Range (lower = more collisions)")
}

main()