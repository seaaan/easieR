################################################################################
context("testing collapse-collapse_to_sentence()")
################################################################################

test_that("Correct error message is generated with vector of length 1", {
   expected <- "Vector 'one' must contain more than one element, but contains 1."
   one <- "test"
   expect_that(collapse_to_sentence(one), throws_error(expected))
})

test_that("Correct error message is generated with vector of length 0", {
   expected <- "Vector 'none' must contain more than one element, but contains 0."
   none <- c()
   expect_that(collapse_to_sentence(none), throws_error(expected))
})

test_that("Correct output, two elements", {
   expected <- "a and b"
   expect_that(collapse_to_sentence(c("a", "b")), 
      is_identical_to(expected))
})

test_that("Correct output, defaults", {
   expected <- "a, b, and c"
   expect_that(collapse_to_sentence(c("a", "b", "c")), 
      is_identical_to(expected))
})

test_that("Correct output, no oxford comma", {
   expected <- "a, b and c"
   expect_that(collapse_to_sentence(c("a", "b", "c"), oxford_comma = FALSE), 
      is_identical_to(expected))
})

test_that("Correct output, terminal period", {
   expected <- "a, b, and c."
   expect_that(collapse_to_sentence(c("a", "b", "c"), terminal_period = TRUE), 
      is_identical_to(expected))
})

test_that("Correct output, terminal period no oxford comma", {
   expected <- "a, b and c."
   expect_that(collapse_to_sentence(c("a", "b", "c"), oxford_comma = FALSE, 
         terminal_period = TRUE), 
      is_identical_to(expected))
})

test_that("Correct output, 'or' as conjunction", {
   expected <- "a, b, or c"
   expect_that(collapse_to_sentence(c("a", "b", "c"), conjunction = "or"), 
      is_identical_to(expected))
})

################################################################################
context("testing collapse-collapse_slash()")
################################################################################

test_that("Correct error message is generated with vector of length 1", {
   expected <- "Vector 'one' must contain more than one element, but contains 1."
   one <- "test"
   expect_that(collapse_slash(one), throws_error(expected))
})

test_that("Correct error message is generated with vector of length 0", {
   expected <- "Vector 'none' must contain more than one element, but contains 0."
   none <- c()
   expect_that(collapse_slash(none), throws_error(expected))
})

test_that("Correct output, two elements", {
   expected <- "a/b/"
   expect_that(collapse_slash(c("a", "b")), is_identical_to(expected))
})

test_that("Correct output, two elements no terminal", {
   expected <- "a/b"
   expect_that(collapse_slash(c("a", "b"), terminal_slash = FALSE), 
      is_identical_to(expected))
})

test_that("Correct output, two elements back", {
   expected <- "a\\b\\"
   expect_that(collapse_slash(c("a", "b"), forward = FALSE), 
      is_identical_to(expected))
})

test_that("Correct output, two elements back no terminal", {
   expected <- "a\\b"
   expect_that(collapse_slash(c("a", "b"), forward = FALSE, terminal_slash = FALSE),
      is_identical_to(expected))
})

test_that("Correct output, defaults three", {
   expected <- "a/b/c/"
   expect_that(collapse_slash(c("a", "b", "c")), is_identical_to(expected))
})