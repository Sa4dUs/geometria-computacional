# ============================================================
# File: sort.R
#
# Authors:
#   - Marcelo Domínguez (@sa4dus)
#   - Alejandro García Prada (@AlexGarciaPrada)
#
# Created: 2026-02-09
# Last modified: 2026-02-09
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

main <- function() {
  n = 100
  v = round(runif(n, 0, n))
  sort_fns = c(bubbleSort, insertionSort, selectionSort)
  
  for (sort_fn in sort_fns) {
    print(system.time(sort_fn(v)))
  }
}

main()