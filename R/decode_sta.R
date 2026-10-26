
#' Decode a binary STA file
#'
#' Reads observations and variable metadata from a supported binary STA file.
#' Numeric category codes are retained in the returned data frame; their
#' textual labels are available in attributes. Version 5 CSS and Version 6 OLE
#' headers are routed to their corresponding decoders automatically.
#' @param path Path to a binary STA file.
#'
#' @return A data frame with one row per observation and one column per
#'   variable. Attributes may include `variable_metadata`, `value_labels`,
#'   `chr_values`, `file_notes`, `file_notes2`, and `rownames`, depending on
#'   which structures are present in the file.
#'
#' @examples
#' \dontrun{
#' x <- decode_sta("data/training-v7.sta")
#' attr(x, "variable_metadata")
#' attr(x, "chr_values")
#' }
#' @export
decode_sta <- function(path) {
  if (length(path) != 1L || is.na(path) || !file.exists(path)) {
    stop("STA-file not found: ", path)
  }

  header <- local({
    header_con <- file(path, open = "rb")
    on.exit(close(header_con))
    readBin(header_con, raw(), n = 8L)
  })
  # V5 check
  if (length(header) >= 4L &&
      identical(header[seq_len(4L)], charToRaw("CSS "))) {
    if (!exists("decode_sta5", mode = "function", inherits = TRUE)) {
      stop(
        "Version-5-CSS file detected. Load decode_sta5.R before decode_sta.R."
      )
    }
    return(get("decode_sta5", mode = "function", inherits = TRUE)(path))
  }
  # V6 check
  cfb_signature <- as.raw(c(
    208L, 207L, 17L, 224L, 161L, 177L, 26L, 225L
  ))
  if (length(header) == length(cfb_signature) &&
      identical(header, cfb_signature)) {
    if (!exists("decode_sta6", mode = "function", inherits = TRUE)) {
      stop(
        "Version-6-OLE file detected. Load decode_sta6.R before decode_sta.R."
      )
    }
    return(get("decode_sta6", mode = "function", inherits = TRUE)(path))
  }
  # V7 check
  con <- file(path, open = "rb")
  on.exit(close(con))
  
  read_i32_at <- function(offset) {
    seek(con, where = offset, origin = "start")
    value <- readBin(con, integer(), n = 1L, size = 4L, endian = "little")
    if (length(value) != 1L) {
      stop("STA-header is incomplete.")
    }
    value
  }
  
  file_size <- file.info(path)$size
  n_vars <- read_i32_at(0xEC)
  n_obs <- read_i32_at(0xF0)
  row_stride <- read_i32_at(0xFC)
  data_start <- 0x100
  has_variable_width_fields <- row_stride != n_vars * 8L + 4L
  if (is.na(file_size) || n_vars < 1L || n_obs < 0L || row_stride < 1L) {
    stop("Invalid or not supported STA dimensions inside header.")
  }
  
  data_end <- data_start + n_obs * row_stride
  if (data_end > file_size) {
    stop("STA file is shorther than mentioned inside header.")
  }
  
  seek(con, where = data_start, origin = "start")
  data <- readBin(con, raw(), n = n_obs * row_stride)
  if (length(data) != n_obs * row_stride) {
    stop("STA file contains incomplete records.")
  }
  metadata_start <- data_start + n_obs * row_stride
  seek(con, where = metadata_start, origin = "start")
  metadata <- readBin(con, raw(), n = file_size - metadata_start)
  descriptor_marker <- as.raw(c(1L, 0L, 0L, 0L, 2L))
  has_metadata_descriptors <- length(metadata) >= length(descriptor_marker) &&
    sum(vapply(
      seq_len(length(metadata) - length(descriptor_marker) + 1L),
      function(i) identical(
        metadata[i:(i + length(descriptor_marker) - 1L)],
        descriptor_marker
      ),
      logical(1)
    )) >= n_vars
  v7 <- FALSE
  if (has_variable_width_fields || has_metadata_descriptors) {
    # message(path, ": Version-7-STA file with metadata descriptors detected.")
    v7 <- TRUE
  } else {
    # message(path, ": Version-7-STA file without metadata descriptors detected.")
    v7 <- TRUE
  }
  if (v7 == TRUE) {
    message(path, ": Version-7-STA file detected.")
    if (!exists("decode_sta7", mode = "function", inherits = TRUE)) {
      stop(
        "Version-7 file detected. Load decode_sta7.R before decode_sta.R."
      )
    }
    return(get("decode_sta7", mode = "function", inherits = TRUE)(path))
  }
}
