makePotentialOilGas <- function(disturbanceList, 
                                whatToCombine){
  
  # whatToCombine selects no oilGas layers (e.g., it only combines mining layers): there is
  # nothing to build, whether or not the study area has oil and gas disturbances
  if (!"oilGas" %in% whatToCombine[["datasetName"]]){
    message("No potential for oil and gas in the study area: whatToCombine selects no oilGas layers. Returning NULL")
    return(NULL)
  }

  laysToWork <- disturbanceList[["oilGas"]][names(disturbanceList[["oilGas"]]) %in% 
                                              whatToCombine[datasetName == "oilGas", 
                                                            dataClasses]]
  if (is.null(laysToWork)){
    message("No potential for oil and gas in the study area. Returning NULL")
    return(NULL)
  } 
  
  whichClaim <- laysToWork[unlist(lapply(laysToWork, function(L){
    return(any(grepl(pattern = "OBJECTID", x = names(L))))
  }))]
  if (length(whichClaim) > 0){
    whichClaim <- whichClaim[[1]]
    useClaim <- TRUE
  } else {
    useClaim <- FALSE
  }

  whichPotential <- laysToWork[unlist(lapply(laysToWork, function(L){
    return(any(grepl(pattern = "Band_1", x = names(L))))
  }))]# This used to be the layer C2H4_BCR6_NT1. I have no idea why terra/reproducible changed 
  # the name to Band_1. No time to find out... 
  if (length(whichPotential) > 0){
    whichPotential <- whichPotential[[1]]
    whichPotential[["Potential"]] <- as.numeric(whichPotential[["Band_1"]][[1]])+1
    whichPotential <- whichPotential[, "Potential"] #remove all names except potential
    usePotential <- TRUE
  } else {
    usePotential <- FALSE
  }
  
  if (all(usePotential,
          useClaim)){
    # pot claim
    whichClaim[["Potential"]] <- as.numeric(max(whichPotential))+1 # Maximum potential is in already claimed areas
    whichClaim <- whichClaim[, "Potential"] #remove all names except potential
    newPotentialOil <- rbind(whichPotential, whichClaim)
  } else {
    if (all(usePotential,
            !useClaim)){
      # pot !claim
      newPotentialOil <- whichPotential
    } else {
      if (all(!usePotential,
              useClaim)){
        # !pot claim
        stop(paste0("No potential in the study area. At least one potential ",
                    "layer is necessary for the analysis. If you are supplying ",
                    " potential layers, please keep this in mind. If you are ",
                    "the deafult in the NT, your study area is outside of",
                    "currently supported region for potential resources",
                    "creation but this was missed before. Please debug!"))
      } else {
        # !pot !claim
        stop(paste0("No existing oil and gas disturbance NEITHER ",
                    "potential in the study area. At least one potential ",
                    "layer is necessary for the analysis. If you are supplying ",
                    "potential layers, please keep this in mind. If you are using ",
                    "the default in the NT, your study area is outside of the ",
                    "currently supported region for potential resources ",
                    "creation but this was missed before. Please debug!"))
      }
    }
  }
  
  return(newPotentialOil)
}