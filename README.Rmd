# RSTA

Reading data stored by version 7 of Statistica software package.

RSTA decodes supported binary `.sta` files to R data frames and preserves
variable metadata, value labels, missing-value codes, file notes, and case
names when they are present.

## Installation

This project is currently a source package. To install it from the package
root, run:

```sh
R CMD INSTALL .
```

The automated tests require `testthat` and `withr`:

```r
install.packages(c("testthat", "withr"))
```

## Usage

```r
library(RSTA)

data <- decode_sta("path/to/file.sta")
head(data)
attr(data, "variable_metadata")
attr(x, "value_labels")
attr(x, "chr_values")
attr(x, "rownames")
attr(x, "file_notes")
attr(x, "file_notes2")

```

`decode_sta()` identifies Version 7 files from their
headers and dispatches them to the corresponding decoders.

Numeric category codes remain numeric; code-to-text mappings are exposed in
`value_labels` and `chr_values`. Missing-value codes are converted to `NA` and
recorded in `variable_metadata`.

## Tests

From the package root, run:

```r
testthat::test_local(".", reporter = "summary")
```

## License

RSTA is distributed under the MIT License. The copyright holder and author
metadata in `DESCRIPTION` and `LICENSE` are provisional and must be reviewed
before release.
