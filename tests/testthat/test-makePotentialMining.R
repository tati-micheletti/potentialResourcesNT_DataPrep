# tests/testthat/test-makePotentialMining.R

# --- small helpers ---------------------------------------------------------
mk_sq <- function(xmin, ymin, size = 10, crs = "EPSG:3857", atts = list()) {
  coords <- matrix(
    c(xmin, ymin,
      xmin + size, ymin,
      xmin + size, ymin + size,
      xmin, ymin + size,
      xmin, ymin),
    ncol = 2, byrow = TRUE
  )
  v <- terra::vect(coords, type = "polygons", crs = crs)
  if (length(atts)) for (nm in names(atts)) v[[nm]] <- atts[[nm]]
  v
}

# Build minimal claims & permits layers with ACTIVE/CANCELLED and an extra status
claims <- rbind(
  mk_sq(0, 0,  atts = list(CLAIM_STAT = "ACTIVE")),
  mk_sq(20, 0, atts = list(CLAIM_STAT = "CANCELLED")),
  mk_sq(40, 0, atts = list(CLAIM_STAT = "EXPIRED"))   # should be dropped
)
permits <- rbind(
  mk_sq(0, 20,  atts = list(PERMIT_STA = "ACTIVE")),
  mk_sq(20, 20, atts = list(PERMIT_STA = "CANCELLED")),
  mk_sq(40, 20, atts = list(PERMIT_STA = "SUSPENDED")) # should be dropped
)
crs_claims <- crs(claims) # for CRS check

# disturbanceList: order matches whatToCombine$dataClasses to satisfy current indexing
dl_ok <- list(
  mining = list(
    miningClaims  = claims,
    miningPermits = permits
  )
)

whatToCombine_ok <- data.table(
  datasetName   = "mining",
  dataClasses   = c("miningClaims", "miningPermits"),
  activeProcess = c("CLAIM_STAT", "PERMIT_STA")
)

test_that("Returns a SpatVector with correct Potential mapping and only 'Potential' attribute", {
  skip_on_cran()
  
  res <- makePotentialMining(dl_ok, whatToCombine_ok)
  
  expect_s4_class(res, "SpatVector")
  # only the Potential attribute should remain
  expect_equal(names(res), "Potential")
  
  # mapping per current implementation:
  # ACTIVE CLAIM  -> 4
  # ACTIVE PERMIT -> 3
  # CANCELLED CLAIM  -> 2
  # CANCELLED PERMIT -> 1
  expect_true(all(res$Potential %in% c(1, 2, 3, 4)))
  
  # we created exactly one of each ACTIVE/CANCELLED for claims and permits
  expect_equal(nrow(res), 4)
  expect_setequal(res$Potential, c(1, 2, 3, 4))
  
  # unknown statuses (EXPIRED/SUSPENDED) are excluded
  expect_equal(nrow(res), 4)
  
  # CRS preserved from inputs
  expect_equal(terra::crs(res), crs_claims)
})

test_that("Mis-specified activeProcess (order/field mismatch) errors (documents current behaviour)", {
  skip_on_cran()
  
  # Swap activeProcess (now wrong: claims will be filtered by PERMIT_STA, etc.)
  whatToCombine_bad <- copy(whatToCombine_ok)
  whatToCombine_bad$activeProcess <- rev(whatToCombine_bad$activeProcess)
  
  expect_error(
    makePotentialMining(dl_ok, whatToCombine_bad),
    regexp = "No potential|PERMIT|CLAIM|field|column|not.*found|subscript",
    ignore.case = TRUE
  )
})

test_that("Missing mining sector returns NULL with message", {
  skip_on_cran()
  
  dl_null <- list(mining = NULL)
  expect_message(
    out <- makePotentialMining(dl_null, whatToCombine_ok),
    regexp = "No potential mining.*Returning NULL",
    ignore.case = TRUE
  )
  expect_null(out)
})

test_that("If no requested mining classes exist, function returns NULL with message", {
  skip_on_cran()
  
  # mining list exists but with different names -> laysToWork becomes length-0 list
  dl_empty_pick <- list(mining = list(otherLayer = claims))
  expect_message(
    out <- makePotentialMining(dl_empty_pick, whatToCombine_ok),
    regexp = "No potential mining.*Returning NULL",
    ignore.case = TRUE
  )
  expect_null(out)
})

test_that("Claims-only and Permits-only produce expected Potential sets", {
  skip_on_cran()
  cl <- rbind(mk_sq(0,0, atts=list(CLAIM_STAT="ACTIVE")),
              mk_sq(20,0,atts=list(CLAIM_STAT="CANCELLED")))
  dl_claims <- list(mining=list(miningClaims=cl))
  wtc_claims <- data.table(datasetName="mining",
                           dataClasses="miningClaims",
                           activeProcess="CLAIM_STAT")
  res_c <- makePotentialMining(dl_claims, wtc_claims)
  expect_setequal(res_c$Potential, c(4,2))
  
  pe <- rbind(mk_sq(0,0, atts=list(PERMIT_STA="ACTIVE")),
              mk_sq(20,0,atts=list(PERMIT_STA="CANCELLED")))
  dl_perms <- list(mining=list(miningPermits=pe))
  wtc_perms <- data.table(datasetName="mining",
                          dataClasses="miningPermits",
                          activeProcess="PERMIT_STA")
  res_p <- makePotentialMining(dl_perms, wtc_perms)
  expect_setequal(res_p$Potential, c(3,1))
})

test_that("CRS mismatch across selected layers: output inherits CRS of first layer (documents current behaviour)", {
  skip_on_cran()
  a <- mk_sq(0,0, crs="EPSG:3857", atts=list(CLAIM_STAT="ACTIVE"))
  b <- mk_sq(10,0, crs="EPSG:3005", atts=list(PERMIT_STA="ACTIVE"))
  dl <- list(mining=list(miningClaims=a, miningPermits=b))
  wtc <- data.table(datasetName="mining",
                    dataClasses=c("miningClaims","miningPermits"),
                    activeProcess=c("CLAIM_STAT","PERMIT_STA"))
  
  res <- makePotentialMining(dl, wtc)
  
  expect_s4_class(res, "SpatVector")
  expect_equal(terra::crs(res), terra::crs(a))  # inherits first layer's CRS
})

test_that("Missing activeProcess field in a selected layer errors", {
  skip_on_cran()
  cl <- mk_sq(0,0, atts=list(CLAIM_STAT="ACTIVE"))
  pe <- mk_sq(10,0, atts=list(WRONG_FIELD="ACTIVE")) # missing PERMIT_STA
  dl <- list(mining=list(miningClaims=cl, miningPermits=pe))
  wtc <- data.table(datasetName="mining",
                    dataClasses=c("miningClaims","miningPermits"),
                    activeProcess=c("CLAIM_STAT","PERMIT_STA"))
  expect_error(
    makePotentialMining(dl, wtc),
    regexp = "RHS of ==|length 0|%in%|PERMIT|field|column",
    ignore.case = TRUE
  )
})

test_that("NA or mixed-case statuses are excluded under current strict matching", {
  skip_on_cran()
  cl <- rbind(
    mk_sq(0,0,  atts=list(CLAIM_STAT="ACTIVE")),
    mk_sq(10,0, atts=list(CLAIM_STAT=NA_character_)),
    mk_sq(20,0, atts=list(CLAIM_STAT="active")) # not matched
  )
  dl <- list(mining=list(miningClaims=cl))
  wtc <- data.table(datasetName="mining", dataClasses="miningClaims", activeProcess="CLAIM_STAT")
  res <- makePotentialMining(dl, wtc)
  expect_equal(nrow(res), 1)
  expect_equal(unique(res$Potential), 4)
})

test_that("Non-polygon mining layers are accepted and geometry is preserved (documents current behaviour)", {
  skip_on_cran()
  ln <- terra::vect(matrix(c(0,0, 10,0), ncol=2, byrow=TRUE), type="lines", crs="EPSG:3857")
  ln$CLAIM_STAT <- "ACTIVE"
  dl <- list(mining=list(miningClaims=ln))
  wtc <- data.table(datasetName="mining", dataClasses="miningClaims", activeProcess="CLAIM_STAT")
  
  res <- makePotentialMining(dl, wtc)
  
  expect_s4_class(res, "SpatVector")
  expect_true(grepl("line", terra::geomtype(res), ignore.case = TRUE))
  expect_equal(unique(res$Potential), 4)
  expect_equal(nrow(res), 1)
})

test_that("Order dependence: misaligned whatToCombine vs list order fails", {
  skip_on_cran()
  cl <- mk_sq(0,0,  atts=list(CLAIM_STAT="ACTIVE"))
  pe <- mk_sq(10,0, atts=list(PERMIT_STA="CANCELLED"))
  dl <- list(mining=list(miningClaims=cl, miningPermits=pe)) # claims first
  wtc <- data.table(datasetName="mining",
                    dataClasses=c("miningPermits","miningClaims"),  # reversed
                    activeProcess=c("PERMIT_STA","CLAIM_STAT"))
  expect_error(
    makePotentialMining(dl, wtc),
    regexp = "No potential|subscript|RHS of ==|activeProcess",
    ignore.case = TRUE
  )
})

test_that("Order dependence: reordering both list and whatToCombine aligns and succeeds", {
  skip_on_cran()
  cl <- mk_sq(0,0,  atts=list(CLAIM_STAT="ACTIVE"))
  pe <- mk_sq(10,0, atts=list(PERMIT_STA="CANCELLED"))
  dl2 <- list(mining=list(miningPermits=pe, miningClaims=cl)) # now permits first
  wtc2 <- data.table(datasetName="mining",
                     dataClasses=c("miningPermits","miningClaims"),
                     activeProcess=c("PERMIT_STA","CLAIM_STAT"))
  
  res <- makePotentialMining(dl2, wtc2)
  expect_s4_class(res, "SpatVector")
  expect_setequal(res$Potential, c(1,4))  # CANCELLED permit -> 1; ACTIVE claim -> 4
})

test_that("Unreferenced mining layers are ignored", {
  skip_on_cran()
  cl <- mk_sq(0,0, atts=list(CLAIM_STAT="ACTIVE"))
  extra <- mk_sq(20,0, atts=list(DUMMY="X"))
  dl <- list(mining=list(miningClaims=cl, miningLegacy=extra))
  wtc <- data.table(datasetName="mining", dataClasses="miningClaims", activeProcess="CLAIM_STAT")
  res <- makePotentialMining(dl, wtc)
  expect_equal(nrow(res), 1)
  expect_equal(unique(res$Potential), 4)
})
