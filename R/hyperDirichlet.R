rescaleTable <- function(table, scaleFactor) {
  pcols <- getTableStates(table)
  table[,pcols] <- sweep(table[,pcols],1,scaleFactor,"*")
  table
}

normalizeTable <- function(table) {
  pcols <- getTableStates(table)
  scaleFactor <- 1/rowSums(table[,pcols])
  table[,pcols] <- sweep(table[,pcols],1,scaleFactor,"*")
  table
}

getTableParents <- function (table) {
  names(table)[sapply(table,is.factor)]
}

getTableStates <- function (table) {
  names(table)[sapply(table,is.numeric)]
}

"scaleTable" <- function (table) {
  if (is.null(nrow(table))) {
    ## table is a vector
    scalecol <- length(table)
    sumcol <- scalecol-1
    if ("Scale" == names(table)[scalecol] &&
        "Sum" == names(table)[sumcol]) {
      Scale <- table[scalecol]
      Sum <- table[sumcol]
      return(table[-c(scalecol,sumcol)]/(Sum/Scale))
    } else {
      return(table)
    }
  }
  if (nrow(table) ==1) {
    ## R is far too eager to collapse single row matrixes into vectors
    ## so need special handling for this case.
    scalecol <- length(table)
    sumcol <- scalecol-1
    if ("Scale" == colnames(table)[scalecol] &&
        "Sum" == colnames(table)[sumcol]) {
      Scale <- table[,scalecol]
      Sum <- table[,sumcol]
      result <- table[,-c(scalecol,sumcol)]/(Sum/Scale)
      return(matrix(as.numeric(result),nrow=1,
                    dimnames=list(NULL,colnames(table)[-c(scalecol,sumcol)])))
    } else {
      return(table)
    }
  }
  states <- NULL
  if (is.data.frame(table)) {
    states <- factorPart(table)
    table <- numericPart(table)
  }
  scalecol <- ncol(table)
  sumcol <- scalecol-1
  if ("Scale" == colnames(table)[scalecol] &&
      "Sum" == colnames(table)[sumcol]) {
    Scale <- table[,scalecol]
    Sum <- table[,sumcol]
    table <- sweep(table[,-c(scalecol,sumcol)],1,Sum/Scale,"/")
  }
  if (is.null(states)) 
    return(table)
  data.frame(states,table)
}

"numericPart" <-
function(table) {
  if (is.matrix(table)) {
    if(is.numeric(table)) return(table)
    else stop("Table is a non-numeric matrix")
  }
  which <- sapply(table,is.numeric)
  as.matrix(table[,which,drop=FALSE])
}

"factorPart" <-
function(table) {
  which <- sapply(table,is.factor)
  table[,which,drop=FALSE]
}
