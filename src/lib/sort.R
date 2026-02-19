# ============================================================
# File: sort.R
#
# Authors:
#   - Marcelo Domínguez (@sa4dus)
#   - Alejandro García Prada (@AlexGarciaPrada)
#
# Created: 2026-02-09
# Last modified: 2026-02-19
# License: MIT
# ============================================================

bubbleSort <- function (v) {
  n = length(v)
  
  for (i in 1:(n-1)) {
    swap = FALSE
    for (j in 1:(n-i)) {
      if (v[j] > v[j+1]) {
        v[j:(j+1)] <- rev(v[j:(j+1)])
        swap = TRUE
      }
    } 
    
    if (!swap) {
      break
    }
  }
  
  v
}

insertionSort <- function (v) {
  n = length(v)
  
  for (i in 1:n) {
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

selectionSort <- function(v) {
  n = length(v)
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

mergeSort <- function(v) {
  n <- length(v)
  if (n <= 1) return(v)
  
  half <- ceiling(n/2)
  h1 <- mergeSort(v[1:half])
  h2 <- mergeSort(v[(half + 1):n])
  
  return(merge(h1, h2))
}

merge <- function(a, b) {
  out <- c()
  
  while (length(a) > 0 && length(b) > 0) {
    if (a[[1]] > b[[1]]) {
      out <- c(out, b[[1]])
      b <- b[-1]
    } else {
      out <- c(out, a[[1]])
      a <- a[-1]
    }
  }
  
  if (length(a) > 0) out <- c(out, a)
  if (length(b) > 0) out <- c(out, b)
  
  out
}

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

main <- function() {
  n = 1000
  v = round(runif(n, 0, n))
  sort_fns = c(insertionSort, selectionSort, mergeSort, quickSort, heapSort)
  
  for (sort_fn in sort_fns) {
    print(system.time(sort_fn(v)))
  }
}

main()