# cmip6 to cmip5 converter
# Establishes which models in cmip5 are most similar to those in cmip6 for use in pulse factors
cmip6tocmip5converter <- function(){
  source("./modules/cmip5.R")
  ctemp5 = ctemp
  rm(ctemp)
  source("./modules/cmip6.R")
  ctemp = ctemp[ctemp$year > 2005, ]
  ctemp[, ssp := substr(rcp, 1, 1)]
  ctemp[, rcp := paste0("rcp", substr(rcp, 2, 3))]
  # This is to fix a line repeat bug and should not be in the committed version
  ctemp = aggregate(temp ~ model + rcp + ISO3 + year + ssp, ctemp, mean)
  
  modelmapping = data.frame()
  for (model_i in unique(ctemp$model)){
    ctempdif = merge(x=ctemp5, y=ctemp[ctemp$model==model_i,], by=c("ISO3", "year", "rcp"), 
                     all.x=TRUE)
    ctempdif[,difsq := (temp.x-temp.y) ** 2]
    ctempdif = ctempdif[!is.na(difsq)]
    if (nrow(ctempdif) == 0) {
      drow = data.frame(model_i, "NA")
    } else {
      groupmean = aggregate(ctempdif, by=list(ctempdif$model.x), mean)
      drow = data.frame(model_i, groupmean[groupmean$difsq==min(groupmean$difsq), "Group.1"])
    }
    names(drow) <- c("cmip6", "cmip5")
    modelmapping = rbind(modelmapping, drow)
  }
  # A few models do not overlap the scenarios with previous work. We assign them the values 
  # of the first most similarly named model
  toinfillmm = modelmapping[modelmapping$cmip5=="NA", "cmip6"]
  infilledmm = modelmapping[modelmapping$cmip5!="NA",]
  for (model_i in toinfillmm){
    print(model_i)
    matchstr = substr(model_i, 1, 5)
    ind0 = grep(matchstr, infilledmm$cmip6)
    ind0 = ind0[1]
    modelmapping[modelmapping$cmip6==model_i, "cmip5"] = infilledmm$cmip5[ind0]
  }
  return(modelmapping)
}
