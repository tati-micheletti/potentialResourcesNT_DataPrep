# tests/testthat/test-makePotentialOilGas.R

testthat::test_that("makePotentialOilGas: setup sanity", {
  testthat::skip_on_cran()
  
  # small helper to draw a 1x1 square polygon
  mk_sq <- function(x0, y0, size = 1, crs = "EPSG:3857") {
    terra::vect(
      matrix(c(
        x0, y0,
        x0 + size, y0,
        x0 + size, y0 + size,
        x0, y0 + size,
        x0, y0
      ), ncol = 2, byrow = TRUE),
      type = "polygons", crs = crs
    )
  }
  
  # Build a tiny "potential" layer with a 'Band_1' column
  pot1 <- mk_sq(0, 0); pot2 <- mk_sq(2, 0)
  pot  <- rbind(pot1, pot2)
  pot$Band_1 <- 5   # constant on purpose (documents current function behavior)
  # Build a tiny "claims" layer (detected via presence of 'OBJECTID')
  clm <- mk_sq(1, 1)
  clm$OBJECTID <- 1L
  
  # Minimal disturbanceList + whatToCombine
  dl <- list(oilGas = list(
    potentialOilGas = pot,
    claims          = clm
  ))
  wtc <- data.table(
    datasetName = "oilGas",
    dataClasses = c("potentialOilGas", "claims")
  )
  
  # Sanity
  testthat::expect_true("oilGas" %in% names(dl))
  testthat::expect_true(all(c("potentialOilGas","claims") %in% names(dl$oilGas)))
  testthat::expect_true(all(c("Band_1") %in% names(dl$oilGas$potentialOilGas)))
  testthat::expect_true(all(c("OBJECTID") %in% names(dl$oilGas$claims)))
})

testthat::test_that("Both potential and claims present: claims get max(potential)+1 and layers are combined", {
  testthat::skip_on_cran()
  
  # Fixtures (identical to previous block; kept local for test independence)
  mk_sq <- function(x0, y0, size = 1, crs = "EPSG:3857") {
    terra::vect(
      matrix(c(x0,y0, x0+size,y0, x0+size,y0+size, x0,y0+size, x0,y0),
             ncol = 2, byrow = TRUE),
      type = "polygons", crs = crs
    )
  }
  pot <- rbind(mk_sq(0,0), mk_sq(2,0)); pot$Band_1 <- 5
  clm <- mk_sq(1,1); clm$OBJECTID <- 1L
  dl  <- list(oilGas = list(potentialOilGas = pot, claims = clm))
  wtc <- data.table(datasetName = "oilGas",
                    dataClasses = c("potentialOilGas","claims"))
  
  out <- makePotentialOilGas(dl, wtc)
  
  # Structure
  testthat::expect_s4_class(out, "SpatVector")
  testthat::expect_true("Potential" %in% names(out))
  testthat::expect_equal(sort(names(out)), "Potential") # only Potential retained
  
  # Current behavior: Potential from 'Band_1' is (first value + 1) for *all* potential features
  # Band_1 was 5 -> Potential = 6 for potential polygons; claims get 7
  vals <- out$Potential
  testthat::expect_true(all(vals %in% c(6, 7)))
  testthat::expect_equal(sum(vals == 6), 2) # two potential polys
  testthat::expect_equal(sum(vals == 7), 1) # one claim poly
})

testthat::test_that("Only potential present: returned layer mirrors potential with Potential = Band_1 + 1", {
  testthat::skip_on_cran()

  mk_sq <- function(x0, y0, size = 1, crs = "EPSG:3857") {
    terra::vect(
      matrix(c(x0,y0, x0+size,y0, x0+size,y0+size, x0,y0+size, x0,y0),
             ncol = 2, byrow = TRUE),
      type = "polygons", crs = crs
    )
  }
  pot <- rbind(mk_sq(0,0), mk_sq(2,0)); pot$Band_1 <- 5
  dl  <- list(oilGas = list(potentialOilGas = pot))
  wtc <- data.table(datasetName = "oilGas", dataClasses = "potentialOilGas")
  
  out <- makePotentialOilGas(dl, wtc)
  
  testthat::expect_s4_class(out, "SpatVector")
  testthat::expect_true("Potential" %in% names(out))
  testthat::expect_equal(nrow(out), nrow(pot))
  testthat::expect_true(all(out$Potential == 6)) # 5 + 1, documented current behavior
})

testthat::test_that("Only claims present: function errors with 'No potential in the study area'", {
  testthat::skip_on_cran()

  mk_sq <- function(x0, y0, size = 1, crs = "EPSG:3857") {
    terra::vect(
      matrix(c(x0,y0, x0+size,y0, x0+size,y0+size, x0,y0+size, x0,y0),
             ncol = 2, byrow = TRUE),
      type = "polygons", crs = crs
    )
  }
  clm <- mk_sq(1,1); clm$OBJECTID <- 1L
  dl  <- list(oilGas = list(claims = clm))
  wtc <- data.table(datasetName = "oilGas", dataClasses = "claims")
  
  testthat::expect_error(
    makePotentialOilGas(dl, wtc),
    regexp = "No potential in the study area",
    ignore.case = TRUE
  )
})

testthat::test_that("No matching layers selected by whatToCombine: function errors documenting current behavior", {
  testthat::skip_on_cran()

  # A list that *has* layers, but whatToCombine filters them all away
  mk_sq <- function(x0, y0, size = 1, crs = "EPSG:3857") {
    terra::vect(
      matrix(c(x0,y0, x0+size,y0, x0+size,y0+size, x0,y0+size, x0,y0),
             ncol = 2, byrow = TRUE),
      type = "polygons", crs = crs
    )
  }
  pot <- mk_sq(0,0); pot$Band_1 <- 5
  clm <- mk_sq(1,1); clm$OBJECTID <- 1L
  dl  <- list(oilGas = list(potentialOilGas = pot, claims = clm))
  
  # Choose a dataClass name that doesn't exist so 'laysToWork' ends up empty
  wtc <- data.table(datasetName = "oilGas", dataClasses = "doesNotExist")
  
  testthat::expect_error(
    makePotentialOilGas(dl, wtc),
    regexp = "potential.*necessary|No existing.*NEITHER.*potential",
    ignore.case = TRUE
  )
})

test_that("Missing oilGas sector -> returns NULL with message", {
  skip_on_cran()
  dl  <- list()  # no oilGas at all
  wtc <- data.table(datasetName = "oilGas",
                    dataClasses = c("potentialOilGas","claims"))
  
  expect_message(
    out <- makePotentialOilGas(dl, wtc),
    regexp = "No potential for oil and gas in the study area",
    ignore.case = TRUE
  )
  expect_null(out)
})

test_that("Multiple claim layers: only the first claims layer is used", {
  skip_on_cran()

  mk_sq <- function(x0, y0, size = 1, crs = "EPSG:3857") {
    vect(matrix(c(x0,y0, x0+size,y0, x0+size,y0+size, x0,y0+size, x0,y0),
                ncol = 2, byrow = TRUE), type = "polygons", crs = crs)
  }
  
  pot <- rbind(mk_sq(0,0), mk_sq(2,0)); pot$Band_1 <- 5
  clmA <- mk_sq(1,1);  clmA$OBJECTID <- 101L   # first in list -> should be chosen
  clmB <- mk_sq(10,10); clmB$OBJECTID <- 202L
  
  dl  <- list(oilGas = list(potentialOilGas = pot, claimsA = clmA, claimsB = clmB))
  wtc <- data.table(datasetName = "oilGas",
                    dataClasses = c("potentialOilGas","claimsA","claimsB"))
  
  out <- makePotentialOilGas(dl, wtc)
  expect_s4_class(out, "SpatVector")
  # If both claims were used, there would be 2 features with the top priority
  expect_equal(sum(out$Potential == max(out$Potential, na.rm = TRUE)), 1)
})

test_that("Non-constant Band_1: Potential is computed per feature as Band_1 + 1", {
  skip_on_cran()
  
  mk_sq <- function(x0, y0, size = 1, crs = "EPSG:3857") {
    terra::vect(matrix(c(x0,y0, x0+size,y0, x0+size,y0+size, x0,y0+size, x0,y0),
                       ncol = 2, byrow = TRUE), type = "polygons", crs = crs)
  }
  
  pot <- rbind(mk_sq(0,0), mk_sq(2,0))
  pot$Band_1 <- c(1, 100)
  dl  <- list(oilGas = list(potentialOilGas = pot))
  wtc <- data.table::data.table(datasetName = "oilGas", dataClasses = "potentialOilGas")
  
  out <- makePotentialOilGas(dl, wtc)
  
  testthat::expect_s4_class(out, "SpatVector")
  testthat::expect_identical(sort(names(out)), "Potential")
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_equal(out$Potential, pot$Band_1 + 1)
})


