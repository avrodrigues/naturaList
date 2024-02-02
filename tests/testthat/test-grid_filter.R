## Create test data specific for the test in this file

occ_xy_test <- data.frame(
  decimalLongitude = c(0.25, 0.12, 0.13, 0.32, 0.41, 0.52, 0.77, 0.27, 0.60, 0.78, 0.00, 0.25, 0.28, 0.48, 0.52, 0.53, 0.54, 0.30, 0.68),
  decimalLatitude = c(0.00, 0.13, 0.21, 0.25, 0.18, 0.12, 0.23, 0.27, 0.28, 0.26, 0.75, 0.52, 0.53, 0.72, 0.52, 0.62, 0.60, 0.78, 0.95)
)

crit_levels <- c(
  "1_det_by_spec",
  "2_not_spec_name",
  "3_image",
  "4_sci_collection",
  "5_field_obs",
  "6_no_criteria_met"
)


occ_xy_test$institutionCode <- "Int. Test"
occ_xy_test$collectionCode  <- "0001"
occ_xy_test$catalogNumber <- "990"

set.seed(130623)
occ_xy_test$year <- sample(2010:2012,  19, replace = T)

set.seed(130623)
occ_xy_test$dateIdentified <- sample(
  as.Date(c("2023/05/22", "2015/06/01")),
  19, replace = T
  )

occ_xy_test$species <- "Teste testtii"
occ_xy_test$identifiedBy <- "ID_001"
occ_xy_test$basisOfRecord <- ""
occ_xy_test$mediaType <- ""
occ_xy_test$occurrenceID <- ""

set.seed(130623)
occ_xy_test$naturaList_levels <- sample(crit_levels, 19, replace = T)


res_05 <- c(0.5, 0.5)
res_025 <- c(0.25, 0.25)

# reference for resolution tests ----

grid_filter_05 <- grid_filter(occ_xy_test, grid.resolution = res_05)
grid_filter_025 <- grid_filter(occ_xy_test, grid.resolution = res_025)

test_that("Number of selected occurrences", {
  expect_equal(nrow(grid_filter_05), 5)
  expect_equal(nrow(grid_filter_025), 13)
  })


test_that("Corrected levels of occurrence selected in each grid cell", {
  expect_equal(
    grid_filter_05$naturaList_levels,
    crit_levels[c(1,2,2,1,1)]
  )

  expect_equal(
    grid_filter_025$naturaList_levels,
    crit_levels[c(3,5,1,1,2,6,4,1,4,2,5,3,1)]
  )
})




