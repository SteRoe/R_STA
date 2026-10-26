testthat::test_that("fixed-width numeric data use their metadata descriptors", {
  path <- tempfile(fileext = ".sta")
  withr::defer(unlink(path))
  make_fixed_numeric_sta_fixture(path)

  result <- RSTA::decode_sta(path)

  testthat::expect_identical(dim(result), c(3L, 2L))
  testthat::expect_identical(names(result), c("AGE", "STATUS"))
  testthat::expect_equal(result$AGE, c(58, 42, 35))
  testthat::expect_equal(result$STATUS, c(1, 2, NA_real_))
  testthat::expect_identical(
    attr(result, "variable_metadata")$text,
    c("age", "status")
  )
  testthat::expect_identical(
    attr(result, "chr_values")$chr,
    c("Active", "Inactive")
  )
})

testthat::test_that("mixed records preserve case names, notes, labels, and missings", {
  path <- tempfile(fileext = ".sta")
  withr::defer(unlink(path))
  make_mixed_sta_fixture(path)

  result <- RSTA::decode_sta(path)
  metadata <- attr(result, "variable_metadata")

  testthat::expect_identical(dim(result), c(3L, 12L))
  testthat::expect_identical(result$CASE, c("C001", "C002", "C003"))
  testthat::expect_identical(attr(result, "rownames"), result$CASE)
  testthat::expect_identical(attr(result, "file_notes"), "Header")
  testthat::expect_identical(attr(result, "file_notes2"), "Top")
  testthat::expect_equal(
    metadata$missing_value[match(c("SCORE", "FLAG", "GROUP"), metadata$name)],
    c(-1000, 222, 255)
  )
  testthat::expect_identical(result$SCORE, c(10L, NA_integer_, 30L))
  testthat::expect_identical(result$FLAG, c(1L, NA_integer_, 0L))
  testthat::expect_identical(result$GROUP, c(1L, 0L, NA_integer_))
  testthat::expect_identical(
    attr(result, "chr_values")$chr,
    c("No", "Yes")
  )
})

testthat::test_that("duplicate truncated names preserve variables and file notes", {
  path <- tempfile(fileext = ".sta")
  withr::defer(unlink(path))
  make_duplicate_name_sta_fixture(path)

  result <- RSTA::decode_sta(path)

  testthat::expect_identical(
    names(result),
    c("TEXTVAR1", "TEXTVAR1_v7_1")
  )
  testthat::expect_equal(result[[1L]], 1)
  testthat::expect_equal(result[[2L]], 2)
  testthat::expect_identical(attr(result, "file_notes"), "Header")
  testthat::expect_identical(attr(result, "file_notes2"), "Top")
})

testthat::test_that("missing input paths fail explicitly", {
  testthat::expect_error(
    RSTA::decode_sta(tempfile(fileext = ".sta")),
    "STA-file not found"
  )
})

testthat::test_that("custom byte, integer, and double missing codes become NA", {
  data <- c(
    as.raw(222L),
    write_i32(-1000L),
    write_f64(123.5)
  )
  layout <- data.frame(
    name = c("byte", "integer", "double"),
    offset = c(0L, 1L, 5L),
    width = c(1L, 4L, 8L),
    missing_value = c(222, -1000, 123.5),
    type = c("byte_integer", "integer", "double")
  )

  result <- RSTA:::decode_fixed_width_records(data, 1L, length(data), layout)

  testthat::expect_true(all(vapply(result, is.na, logical(1))))
})
