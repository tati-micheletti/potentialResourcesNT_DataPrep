## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "potentialResourcesNT_DataPrep",
  description =  paste0("This is a data preparation module to harmonize different",
                        " anthropogenic disturbance datasets, more specifically, ",
                        "mining and oil/gas. It's intended for the Northwest ",
                        "Territories region (default) and is idyosyncratic.",
                        "This means this module is NOT generalizable, but can ",
                        "be used as basis for other types of development. The ",
                        "objective is to create one standardized layer for each",
                        " of the potential resources that has increasing values ",
                        "for most prioritized places (i.e., higher values, more",
                        " likely structures will appear)."),
  keywords = "",
  authors = structure(list(list(given = "Tati", 
                                family = "Micheletti", role = c("aut", "cre"), 
                                email = "tati.micheletti@gmail.com", 
                                comment = NULL)), 
                      class = "person"),  
  childModules = character(0),
  version = list(potentialResourcesNT_DataPrep = "1.0.0"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "potentialResourcesNT_DataPrep.Rmd"), ## same file
  reqdPkgs = list("SpaDES.core (>=1.0.10)", "ggplot2", 
                  "PredictiveEcology/reproducible",
                  "raster", "terra", "crayon", "data.table", "RCurl",
                  "tictoc"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("whatToCombine", "data.table", 
                    data.table(datasetName = c("oilGas", "oilGas", "mining", "mining"),
                               dataClasses = c("potentialOilGas", "potentialOilGas", 
                                               "potentialMining", "potentialMining"),
                               toDifferentiate = c(NA, "C2H4_BCR6_NT1", "CLAIM_STAT", "PERMIT_STA"),
                               activeProcess = c(NA, NA, "CLAIM_STAT", "PERMIT_STA")), 
                    NA, NA,
                    paste0("Here the user should specify a data.table with the ",
                           "dataName and dataClasses from the object `disturbanceList` ",
                           "anthroDisturbance_DataPrep, first and (input from ",
                           "second levels) to be combined. The table also contains a ",
                           "column identifying which to be used to filter",
                           " active processes for mining (CLAIM_STAT and PERMIT_STA) ",
                           "For Oil/Gas (it needs to identify which layer ",
                           "is the potential one (C2H4_BCR6_NT1) and which is used ",
                           " to constrain where oil and gas will be added. For oil ",
                           "and gas, the other potential layer (exploration permits)",
                           " is used as a starting point to add structures, followed",
                           " by randomly placing them in the highest values of C2H4_BCR6_NT1",
                           "and going down until the total amount is reached. For mining, ",
                           "CLAIM_STAT is the potential exploration, while PERMIT_STA ",
                           "are the ones that might become CLAIMS. The most likely ",
                           "values are CLAIMS and followed by PERMITS. ")),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?"),
    defineParameter("allowPre2011", "logical", FALSE, NA, NA,
                    paste0("If TRUE, allows simulations whose start time is ",
                           "before 2011. Intended for specialised validation ",
                           "runs where pre-2011 baselines are available."))
  ),
  inputObjects = bindrows(
    expectsInput(objectName = "disturbanceList", objectClass = "list",
                 desc = paste0("List (general category) of lists (specific ",
                               "class) needed for generating ",
                               "disturbances. This last list contains: ",
                               "Outter list names: dataName from disturbanceDT",
                               "Inner list names: dataClass from disturbanceDT, ",
                               "which is a unique class after harmozining, except ",
                               "for any potential resources that need idiosyncratic",
                               " processing. This  means that each combination ",
                               "of dataName and dataClass (except for 'potential')",
                               " will only have only one element."), 
                 sourceURL = "https://drive.google.com/file/d/1v7MpENdhspkWxHPZMlmx9UPCGFYGbbYm/view?usp=sharing"),
    expectsInput(objectName = "studyArea", 
                 objectClass = "SpatialPolygonDataFrame|vect", 
                 desc = paste0("Study area to which the module should be ",
                               "constrained to. Defaults to NT1+BCR6. Object ",
                               "can be of class 'vect' from terra package"), 
                 sourceURL = "https://drive.google.com/file/d/1RPfDeHujm-rUHGjmVs6oYjLKOKDF0x09/view?usp=sharing"),
    expectsInput(objectName = "rasterToMatch", 
                 objectClass = "RasterLayer|rast", 
                 desc = paste0("All spatial outputs will be reprojected and ",
                               "resampled to it. Defaults to NT1+BCR6. Object ",
                               "can be of class 'rast' from terra package"), 
                 sourceURL = "https://drive.google.com/file/d/11yCDc2_Wia2iw_kz0f0jOXrLpL8of2oM/view?usp=sharing")
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "disturbanceList", objectClass = "list",
                  desc = paste0("List (general category) of lists (specific ",
                                "class) needed for generating ",
                                "disturbances. This is a modified input, where we ",
                                "replace multiple potential layers (i.e., mining",
                                " and oilGas) by only one layer with the highest",
                                "values being the ones that need to be filled ",
                                "with new developments first, or prepare potential layers",
                                " (i.e., potentialCutblocks).")),
    createsOutput(objectName = "potentialOilGas", objectClass = "list",
                  desc = paste0("List (general category) of lists (specific "))
  )
))

## event types
#   - type `init` is required for initialization

doEvent.potentialResourcesNT_DataPrep = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {

      # If the simulations start before 2011, it shouldn't work because of the data
      if (start(sim) < 2011 && !isTRUE(P(sim)$allowPre2011)) {
        stop(paste0("Please revisit your starting year for",
                    " the simulations. Simulations shouldn't ",
                    "start before 2011 (unless allowPre2011 = TRUE)."))
      }
      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "potentialResourcesNT_DataPrep", "createPotentialMining", eventPriority = 3)
      sim <- scheduleEvent(sim, start(sim), "potentialResourcesNT_DataPrep", "createPotentialOilGas", eventPriority = 3)
      sim <- scheduleEvent(sim, start(sim), "potentialResourcesNT_DataPrep", "createPotentialSeismicLines", eventPriority = 3)
      sim <- scheduleEvent(sim, start(sim), "potentialResourcesNT_DataPrep", "createPotentialCutblocks", eventPriority = 3)
      sim <- scheduleEvent(sim, start(sim), "potentialResourcesNT_DataPrep", "replaceInDisturbanceList", eventPriority = 3)
    },
    createPotentialMining = {
      sim$potentialMining <- makePotentialMining(disturbanceList = sim$disturbanceList, 
                                                 whatToCombine = P(sim)$whatToCombine)
    },
    createPotentialOilGas = {
      sim$potentialOilGas <- makePotentialOilGas(disturbanceList = sim$disturbanceList, 
                                                 whatToCombine = P(sim)$whatToCombine)
    },
    createPotentialSeismicLines = {
      # We use the same potential layers for seismic lines than for oilGas. Here we have an extra
      # function only for organizational purposes! As we don't use the existing seismicLines to 
      # determine where the oil potential is (but the oil facilities and C2H4 layer), 
      # we do not need to create an extra layer. Therefore, we use the same as potential Oil
      sim$potentialSeismicLines <- copy(sim$potentialOilGas)
    },
    createPotentialCutblocks = {
      sim$potentialCutblocks <- makePotentialCutblocks(disturbanceList = sim$disturbanceList)
    },
    replaceInDisturbanceList = {
      sim$disturbanceList <- replaceList(disturbanceList = sim$disturbanceList,
                                         potentialOil = sim$potentialOilGas,
                                         potentialMining = sim$potentialMining,
                                         potentialCutblocks = sim$potentialCutblocks,
                                         potentialSeismicLines = sim$potentialSeismicLines)
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  if (!suppliedElsewhere(object = "studyArea", sim = sim)) {
    sim$studyArea <- prepInputs(url = extractURL("studyArea"),
                                targetFile = "NT1_BCR6.shp",
                                alsoExtract = "similar",
                                destinationPath = dPath)
    
    warning(paste0("studyArea was not supplied. Defaulting to BCR6+NT1 in the",
                   " Northwest Territories"), immediate. = TRUE)
  }
  
  if (!suppliedElsewhere(object = "rasterToMatch", sim = sim)) {
    sim$rasterToMatch <- prepInputs(url = extractURL("rasterToMatch"),
                                    targetFile = "RTM.tif",
                                    destinationPath = dPath)
    
    warning(paste0("rasterToMatch was not supplied. Defaulting to BCR6+NT1 in the",
                   " Northwest Territories"), immediate. = TRUE)
  }
  
  if (!suppliedElsewhere(object = "disturbanceList", sim = sim)) {
    sim$disturbanceList <- unwrapTerraList(terraList = extractURL("disturbanceList"), 
                                           generalPath = dPath)
    
    warning(paste0("disturbanceList was not supplied. The current should only ",
                   " be used for module testing purposes! Please run the module ",
                   "`anthroDisturbance_DataPrep`"), 
            immediate. = TRUE)
  }
  
  return(invisible(sim))
}
