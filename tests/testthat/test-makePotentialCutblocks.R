# tests/testthat/test-makePotentialCutblocks.R
skip_on_cran()

# ---- helpers ---------------------------------------------------------------

mk_sq <- function(xmin, ymin, size = 10, crs = "EPSG:3857",
                  SI_50 = 8, CROWNCL = 30) {
  xmax <- xmin + size
  ymax <- ymin + size
  m <- matrix(c(
    xmin, ymin,
    xmax, ymin,
    xmax, ymax,
    xmin, ymax,
    xmin, ymin
  ), ncol = 2, byrow = TRUE)
  v <- terra::vect(m, type = "polygons", crs = crs)
  v$SI_50   <- SI_50
  v$CROWNCL <- CROWNCL
  v
}

mk_layer <- function(polys) {
  # rbind SpatVectors, reset rownames
  do.call(rbind, polys)
}

# Build a small, reproducible potentialCutblocks layer
good1 <- mk_sq( 0,  0, SI_50 = 12, CROWNCL = 40)  # qualifies
good2 <- mk_sq(20,  0, SI_50 =  8, CROWNCL = 30)  # on the threshold, qualifies
bad1  <- mk_sq( 0, 20, SI_50 =  7, CROWNCL = 35)  # Site index too small
bad2  <- mk_sq(20, 20, SI_50 = 15, CROWNCL = 10)  # Crown closure too small

pot_cutblocks <- mk_layer(list(good1, good2, bad1, bad2))

# Standard nested structure
baseDL <- list(
  forestry = list(
    potentialCutblocks = pot_cutblocks
  )
)

# ---- tests -----------------------------------------------------------------

test_that("filters to SI_50 >= 8 and CROWNCL >= 30 (happy path)", {
  out <- makePotentialCutblocks(baseDL)
  expect_s4_class(out, "SpatVector")
  expect_true(terra::geomtype(out) == "polygons")
  
  # exactly the two qualifying features remain
  expect_equal(nrow(out), 2)
  
  # attributes preserved and thresholds inclusive
  df <- as.data.frame(out)
  expect_true(all(df$SI_50 >= 8))
  expect_true(all(df$CROWNCL >= 30))
  
  # CRS preserved
  expect_equal(terra::crs(out), terra::crs(pot_cutblocks))
})

test_that("returns empty SpatVector when nothing qualifies", {
  none <- mk_layer(list(
    mk_sq(0, 0, SI_50 = 5, CROWNCL = 10),
    mk_sq(15, 0, SI_50 = 3, CROWNCL = 25)
  ))
  dl <- list(forestry = list(potentialCutblocks = none))
  out <- makePotentialCutblocks(dl)
  expect_s4_class(out, "SpatVector")
  expect_equal(nrow(out), 0L)
})

test_that("returns NULL (with message) when potential layer is NULL", {
  dl <- list(forestry = list(potentialCutblocks = NULL))
  expect_message(
    out <- makePotentialCutblocks(dl),
    regexp = "No potential for forestry", ignore.case = TRUE
  )
  expect_null(out)
})

test_that("missing 'forestry' sector returns NULL with message", {
  dl <- list() # no 'forestry'
  expect_message(
    out <- makePotentialCutblocks(dl),
    regexp = "No potential for forestry.*Returning NULL", ignore.case = TRUE
  )
  expect_null(out)
})

test_that("does not mutate the input object", {
  # shallow copy for comparison
  before_geom <- terra::geom(baseDL$forestry$potentialCutblocks)
  before_df   <- as.data.frame(baseDL$forestry$potentialCutblocks)
  
  invisible(makePotentialCutblocks(baseDL))
  
  after_geom <- terra::geom(baseDL$forestry$potentialCutblocks)
  after_df   <- as.data.frame(baseDL$forestry$potentialCutblocks)
  
  expect_identical(before_geom, after_geom)
  expect_identical(before_df,   after_df)
})
