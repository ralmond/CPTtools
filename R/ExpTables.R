### ExpTables --- Building expected contingency tables.

pvecTable <- function(data,pvarName, regx = "<var>\\.<state>") {
  regx <- sub("<var>",pvarName,regx,fixed=TRUE) 
  regx <- sub("<state>","(\\w+)", regx,fixed=TRUE)
  var <- dplyr::select(data,dplyr::matches(regx))
  names(var) <- sub(regx,"\\1",names(var))
  as.matrix(var)
}

catTable <- function(data,fvarName,
                     cc=contrasts(as.factor(dplyr::pull(data,fvarName)),contrasts=FALSE)) {
  res <- cc[as.factor(dplyr::pull(data,fvarName)),]
  row.names(res) <- NULL
  res
}
  


expTable <- function(data, pvecVars, facVars, pvecregex = "<var>\\.<state>") {
  pvecsregex <- sub("<state>","(\\w+)", pvecregex,fixed=TRUE)
  allpvars <- sub("<var>",paste("(",paste(pvecVars,collapse="|"),")",sep=""),pvecsregex)
  subdata <- dplyr::select(data,dplyr::matches(allpvars),dplyr::all_of(facVars)) 
  subdata <- na.omit(subdata)
  pvars <- lapply(pvecVars,
    function (v) pvecTable(subdata,v,sub("<var>",v,pvecsregex)))
  names(pvars) <- pvecVars
  ## Spread these into multiple columns.
  fvars <- lapply(facVars, function(v) catTable(subdata,v))
  names(fvars) <- facVars
  vars <- c(pvars,fvars)
  Z <- length(vars)
  eqStr <- paste(paste(paste("z",letters[1:Z],sep=""),collapse = ","),
                 "->",paste(letters[1:Z],collapse=""))
  res <- do.call("einsum",c(eqStr,vars))
  dimnames(res) <- lapply(vars,colnames)
  res
}


