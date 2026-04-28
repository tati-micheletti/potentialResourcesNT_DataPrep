makePotentialSeismicLines <- function(disturbanceList, 
                                whatToCombine){
  # We use the same potential layers for seismic lines than for oilGas. Here we have an extra
  # function only for organizational purposes!
  laysToWork <- disturbanceList[["oilGas"]][names(disturbanceList[["oilGas"]]) %in% 
                                              whatToCombine[datasetName == "oilGas", 
                                                            dataClasses]]
  if (is.null(laysToWork)){
    message("No potential for seismic lines in the study area. Returning NULL")
    return(NULL)
  } 
  
  whichPotential <- disturbanceList[["oilGas"]][["potentialOilGas"]]
  whichClaim <- disturbanceList[["oilGas"]][["claims"]]
  
  # Validate required fields exist
  if (!"Band_1" %in% names(whichPotential)) {
    stop("The potentialOilGas layer must have a field named 'Band_1'")
  }
  
  # This used to be the layer C2H4_BCR6_NT1. I have no idea why terra/reproducible changed 
  # the name to Band_1. No time to find out... 
  band_vals <- as.numeric(whichPotential$Band_1)  # Explicit vector extraction
  whichPotential$Potential <- band_vals + 1
  whichPotential <- whichPotential[, "Potential"] #remove all names except potential
  
  max_val <- max(whichPotential$Potential, na.rm = TRUE)
  whichClaim$Potential <- max_val + 1 # Maximum potential is in already claimed areas
  whichClaim <- whichClaim[, "Potential"] #remove all names except potential
  
  newPotentialOil <- rbind(whichPotential, whichClaim)
  return(newPotentialOil)
}