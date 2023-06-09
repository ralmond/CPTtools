#######
## Utility functions for checking to see if a series is increasing or
## decreasing.

isIncreasing <- function (vec) {
  if (length(vec) < 2L) return (NA)
  for (k in 2L:length(vec)) {
    if (vec[k-1L] >= vec[k]) return (FALSE)
  }
  return (TRUE)
}

isNondecreasing <- function (vec) {
  if (length(vec) < 2L) return (NA)
  for (k in 2L:length(vec)) {
    if (vec[k-1L] > vec[k]) return (FALSE)
  }
  return (TRUE)
}

isDecreasing <- function (vec) {
  if (length(vec) < 2L) return (NA)
  for (k in 2L:length(vec)) {
    if (vec[k-1L] <= vec[k]) return (FALSE)
  }
  return (TRUE)
}

isNonincreasing <- function (vec) {
  if (length(vec) < 2L) return (NA)
  for (k in 2L:length(vec)) {
    if (vec[k-1L] < vec[k]) return (FALSE)
  }
  return (TRUE)
}

isMonotonic <- function (vec, strict=TRUE) {
  if (length(vec) < 2L) return (NA)
  direction <- 0 ## 1 for increasing, -1 for decreasing
  result <- FALSE
  if (vec[1L] < vec[2L]) {
    direction <- 1                            #Increasing
    if (length(vec) == 2L) {
      result <- TRUE
    } else {
      ## Recurse
      if (strict) {
        result <- isIncreasing(vec[-1L])
      } else {
        result <- isNondecreasing(vec[-1L])
      }
    }
    
  } else if (vec[1L] > vec[2L]) {
    direction <- -1                            #Increasing
    if (length(vec) == 2L) {
      result <- TRUE
    } else {
      ## Recurse
      if (strict) {
        result <- isDecreasing(vec[-1L])
      } else {
        result <- isNonincreasing(vec[-1L])
      }
    }
    
  } else {
    ## Flat
    if (!strict) {
      if (length(vec)==2L) {
        result <- TRUE
      } else {
        result <- isMonotonic(vec[-1L],strict)
        direction <- attr(result,"direction")
      }
    }
  }
  attr(result,"direction") <- direction
  result
}
