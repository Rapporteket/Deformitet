# Load required libraries
library(testthat)
library(dplyr)
library(tidyr)
library(forcats)


# Test for `finn_variabler`
test_that("finn_variabler returns correct column names", {
  result <- finn_variabler("Funksjon")
  expected <- c(
    "SRS22_FUNCTION_SCORE",
    "SRS22_FUNCTION_SCORE_patient3mths",
    "SRS22_FUNCTION_SCORE_patient12mths",
    "SRS22_FUNCTION_SCORE_patient60mths"
  )
  expect_equal(result, expected)
})

# Test for `lag_sam_tabell`
test_that("lag_sam_tabell returns data in long format", {
  data <- dplyr::tibble(
    ShNavn = c("A", "B"),
    SRS22_FUNCTION_SCORE = c(1, 2),
    SRS22_FUNCTION_SCORE_patient3mths = c(3, 4),
    SRS22_FUNCTION_SCORE_patient12mths = c(4, 4),
    SRS22_FUNCTION_SCORE_patient60mths = c(5, 4)
  )
  result <- lag_sam_tabell(data, "Funksjon")
  expect_equal(nrow(result), 8)
  expect_equal(colnames(result), c("ShNavn", "Punkt", "Score"))
})

# Test for `nye_navn`
test_that("nye_navn assigns correct labels to Punkt", {
  data <- tibble(
    Punkt = c("SRS22_FUNCTION_SCORE", "SRS22_FUNCTION_SCORE_patient3mths"),
    Score = c(1, 2)
  )
  result <- nye_navn(data)
  expect_equal(unique(result$Punkt), c("Pre-operativt", "3 mnd"))
})

# Test for `vask_sam_tabell`
test_that("vask_sam_tabell filters and relevels data correctly", {
  data <- tibble(
    Punkt = c("Pre-operativt", "3 mnd", "12 mnd", "5 aar"),
    ShNavn = c("A", "A", "B", "C"),
    Score = c(1, 2, 3, 5)
  )
  result <- vask_sam_tabell(data, "Funksjon")
  expect_true(all(!is.na(result$Score)))
  expect_true(all(result$n > 5))
})

# Test for `ggdata_sam_plot`
test_that("ggdata_sam_plot returns correct explanation", {
  result <- ggdata_sam_plot("Funksjon")
  expect_equal(result$forklaring, "SRS22 funksjon (1: dårlig - 5: bra)")
})

# Test for `finn_sam_variabler`
test_that("finn_sam_variabler filters data for two points", {
  data <- tibble(
    Punkt = c("Pre-operativt", "3 mnd", "12 mnd"),
    Score = c(1, 2, 3)
  )
  result <- finn_sam_variabler(data, "Før operasjon - 3 mnd")
  expect_equal(unique(result$Punkt), c("Pre-operativt", "3 mnd"))
})

# Test for `boxplot_sam`
test_that("boxplot_sam creates a ggplot object", {
  data <- tibble(
    Punkt = c("Pre-operativt", "3 mnd"),
    Score = c(1, 2)
  )
  gg_data <- tibble(forklaring = "SRS22 funksjon (1: dårlig - 5: bra)")
  input_data <- tibble(stuff = "Funksjon", "kvinne", "10/01/23", "10/01/24", "10", "15")
  plot <- boxplot_sam(data, gg_data, input_data)
  expect_s3_class(plot, "ggplot")
})

# Test for `density_sam`
test_that("density_sam creates a ggplot object", {
  data <- tibble(
    ShNavn = c("A", "B"),
    Punkt = c("Pre-operativt", "3 mnd"),
    Score = c(1, 2)
  )
  gg_data <- tibble(forklaring = "SRS22 funksjon (1: dårlig - 5: bra)")
  input_data <- tibble(stuff = "Funksjon", "kvinne", "10/01/23", "10/01/24", "10", "15")
  plot <- density_sam(data, gg_data, input_data)
  expect_s3_class(plot, "ggplot")
})
