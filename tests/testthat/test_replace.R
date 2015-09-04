################################################################################
context("testing replace-replace_na()")
################################################################################

test_that("Correct error message is generated with replacement of length > 1", {
   expected <- "Vector 'example' must contain exactly one element, but contains 2."
   example <- c("a", "b")
   expect_that(replace_na(c(1:3, NA), replacement = example), 
      throws_error(expected))
})

test_that("Correct error message is generated with replacement of length 0", {
   expected <- "Vector 'example' must contain exactly one element, but contains 0."
   example <- c()
   expect_that(replace_na(c(1:3, NA), replacement = example), 
      throws_error(expected))
})

test_that("Correct warning is produced with no NAs", {
   expected <- "Vector contains no NAs, so nothing will be replaced."
   expect_that(replace_na(1:3, "x"), gives_warning(expected))
})

test_that("No warning is produced with no NAs but warn = FALSE", {
   expect_that(replace_na(1:3, "x", warn = FALSE), equals(1:3))
})


test_that("Correct output with 1 NA", {
   expect_that(replace_na(c(1:3, NA), replacement = "x"), 
      equals(c(1:3, "x")))
})

test_that("Correct output with multiple NAs", {
   expect_that(replace_na(c(NA, 1:3, NA, "y", NA), replacement = "x"), 
      equals(c("x", 1:3, "x", "y", "x")))
})

test_that("Correct output with factor and multiple NAs", {
   vec <- c(NA, 1:3, NA, "y", NA)
   vec <- factor(vec)
   expect_that(replace_na(vec, replacement = "x"), 
      equals(factor(c("x", 1:3, "x", "y", "x"))))
})

################################################################################
context("testing replace-replace_value()")
################################################################################

test_that("Correct error message is generated with replacement of length > 1", {
   expected <- "Vector 'example' must contain exactly one element, but contains 2."
   example <- c("a", "b")
   expect_that(replace_value(1:3, value = "x", replacement = example), 
      throws_error(expected))
})

test_that("Correct error message is generated with replacement of length 0", {
   expected <- "Vector 'example' must contain exactly one element, but contains 0."
   example <- c()
   expect_that(replace_value(c(1:3, NA), value = "x", replacement = example), 
      throws_error(expected))
})

test_that("Correct error message is generated with value of length > 1", {
   expected <- "Vector 'example' must contain exactly one element, but contains 2."
   example <- c("a", "b")
   expect_that(replace_value(1:3, value = example, replacement = "a"), 
      throws_error(expected))
})

test_that("Correct error message is generated with value of length 0", {
   expected <- "Vector 'example' must contain exactly one element, but contains 0."
   example <- c()
   expect_that(replace_value(c(1:3, NA), value = example, replacement = "x"), 
      throws_error(expected))
})

test_that("Correct warning is produced with no matches", {
   expected <- "Vector contains no matching values, so nothing will be replaced."
   expect_that(replace_value(1:3, "a", "x"), gives_warning(expected))
})

test_that("No warning is produced with no matches but warn = FALSE", {
   expect_that(replace_value(1:3, "a", "x", warn = FALSE), equals(1:3))
})

test_that("Correct output with 1 match", {
   expect_that(replace_value(c(1:3, "a"), "a", replacement = "x"), 
      equals(c(1:3, "x")))
})

test_that("Correct output with multiple matches", {
   expect_that(replace_value(c("a", 1:3, "a", "y", "a"), "a", 
      replacement = "x"), 
      equals(c("x", 1:3, "x", "y", "x")))
})

test_that("Correct output with factor and multiple matches", {
   vec <- c("a", 1:3, "a", "y", "a")
   vec <- factor(vec)
   expect_that(replace_value(vec, "a", replacement = "x"), 
      equals(factor(c("x", 1:3, "x", "y", "x"))))
})

################################################################################
context("testing replace-replace_regex()")
################################################################################

test_that("Correct error message is generated with replacement of length > 1", {
   expected <- "Vector 'example' must contain exactly one element, but contains 2."
   example <- c("a", "b")
   expect_that(replace_regex(1:3, regex = "x", replacement = example), 
      throws_error(expected))
})

test_that("Correct error message is generated with replacement of length 0", {
   expected <- "Vector 'example' must contain exactly one element, but contains 0."
   example <- c()
   expect_that(replace_regex(c(1:3, NA), regex = "x", replacement = example), 
      throws_error(expected))
})

test_that("Correct error message is generated with value of length > 1", {
   expected <- "Vector 'example' must contain exactly one element, but contains 2."
   example <- c("a", "b")
   expect_that(replace_regex(1:3, regex = example, replacement = "a"), 
      throws_error(expected))
})

test_that("Correct error message is generated with value of length 0", {
   expected <- "Vector 'example' must contain exactly one element, but contains 0."
   example <- c()
   expect_that(replace_regex(c(1:3, NA), regex = example, replacement = "x"), 
      throws_error(expected))
})

test_that("Correct warning is produced with no matches", {
   expected <- "Vector contains no matching values, so nothing will be replaced."
   expect_that(replace_regex(1:3, "a", "x"), gives_warning(expected))
})

test_that("No warning is produced with no matches but warn = FALSE", {
   expect_that(replace_regex(1:3, "a", "x", warn = FALSE), equals(1:3))
})

test_that("Correct output with 1 match", {
   expect_that(replace_regex(c(1:3, "absolutely"), "a.", replacement = "x"), 
      equals(c(1:3, "x")))
})

test_that("Correct output with multiple matches", {
   expect_that(replace_regex(c("a", 1:3, "alabama", "y", "aardvark"), 
      "a.", replacement = "x"), 
      equals(c("a", 1:3, "x", "y", "x")))
})

test_that("Correct output with factor and multiple matches", {
   vec <- c("a", 1:3, "alabama", "y", "aardvark")
   vec <- factor(vec)
   expect_that(replace_regex(vec, "a.", replacement = "x"), 
      equals(factor(c("a", 1:3, "x", "y", "x"))))
})

################################################################################
context("testing replace-replace_values()")
################################################################################

test_that("Correct error message is generated with unequal length vectors", {
   expected <- "Vectors 'rep' and 'val' must contain equal numbers of elements."
   rep <- c("a", "b")
   val <- 1:3
   expect_that(replace_values(1:100, values = val, replacements = rep), 
      throws_error(expected))
})

test_that("Correct warning is produced with no matches", {
   expected <- "Vector 'x' contains no values from 'y', so nothing will be replaced."
   x <- 1:3
   y <- "a"
   
   expect_that(replace_values(x, y, "zzz"), gives_warning(expected))
})

test_that("No warning is produced with no matches but warn = FALSE", {
   expected <- "Vector 'x' contains no values from 'y', so nothing will be replaced."
   x <- 1:3
   y <- "a"
   
   expect_that(replace_values(x, y, "zzz", warn = FALSE), equals(1:3))
})

test_that("Correct output with 1 match", {
   expect_that(replace_values(10:30, 30:31, c("a", "b")), 
      equals(c(10:29, "a")))
})

test_that("Correct output with 2 matches", {
   expect_that(replace_values(1:3, 1:2, c("a", "b")), 
      equals(c("a", "b", 3)))
})

test_that("Correct output with 2 matches and factor", {
   input <- factor(1:3)
   expect_that(replace_values(input, 1:2, c("a", "b")), 
      equals(factor(c("a", "b", 3))))
})