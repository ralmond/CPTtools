### ExpTables --- Building expected contingency tables.

pvecTable <- function(data,pvarName, 
                      regx = sub("<var>",pvarName,"P\\.<var>\\.\\.(\\w+)")) {
  var <- select(data,matches(regx))
  names(var) <- sub(regx,"\\1",names(var))
  as.matrix(var)
}
catTable <- function(data,fvarName,
                     cc=contrasts(as.factor(pull(data,fvarName)),contrast=FALSE)) {
  res <- cc[pull(data,fvarName),]
  row.names(res) <- NULL
  res
}
  


expTable <- function(data, pvecVars, facVars, pvecregex = "P\\.<var>\\.\\.<state>") {
  pvecsregex <- sub("<state>","(\\w+)", pvecregex,fixed=TRUE)
  allpvars <- sub("<var>",paste("(",paste(pvecVars,collapse="|"),")",sep=""),pvecsregex)
  subdata <- select(data,matches(allpvars),all_of(facVars)) 
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


