decode_fixed_width_records <- function(data, n_obs, row_stride, layout) {
  if (!all(c("name", "offset", "width", "type") %in% names(layout)) ||
      anyNA(layout$offset) || anyNA(layout$width) ||
      any(layout$offset < 0L) || any(layout$width < 1L) ||
      any(layout$offset + layout$width > row_stride)) {
    stop("Invalid layout for mixed STA records.")
  }

  if (!"missing_value" %in% names(layout)) {
    layout$missing_value <- ifelse(
      layout$type == "byte_integer",
      255,
      ifelse(layout$type %in% c("integer", "double"), -999999998, NA_real_)
    )
  }

  row_starts <- (seq_len(n_obs) - 1L) * row_stride
  columns <- lapply(seq_len(nrow(layout)), function(col) {
    offset <- layout$offset[col]
    width <- layout$width[col]
    type <- layout$type[col]

    if (type %in% c("character", "integer_text")) {
      values <- vapply(seq_len(n_obs), function(row) {
        start <- row_starts[row] + offset + 1L
        field <- data[seq.int(start, length.out = width)]
        terminator <- match(as.raw(0L), field)
        if (!is.na(terminator)) {
          field <- field[seq_len(terminator - 1L)]
        }
        if (!length(field)) {
          return(NA_character_)
        }
        value <- iconv(
          rawToChar(field),
          from = "CP1252",
          to = "UTF-8",
          sub = "byte"
        )
        value <- trimws(value)
        if (nzchar(value)) value else NA_character_
      }, character(1))
      if (type == "integer_text") {
        return(as.integer(values))
      }
      return(values)
    }

    byte_starts <- row_starts + offset + 1L
    if (type == "byte_integer") {
      values <- as.integer(data[byte_starts])
      missing_value <- layout$missing_value[col]
      if (!is.na(missing_value)) {
        values[values == missing_value] <- NA_integer_
      }
      return(values)
    }

    if (!type %in% c("integer", "double")) {
      stop("Not supported field type inside mixed STA layout: ", type)
    }

    byte_indices <- unlist(lapply(
      byte_starts,
      seq.int,
      length.out = width
    ), use.names = FALSE)
    con <- rawConnection(data[byte_indices], open = "rb")
    on.exit(close(con))
    values <- if (type == "integer") {
      readBin(con, integer(), n = n_obs, size = width, endian = "little")
    } else {
      readBin(con, numeric(), n = n_obs, size = width, endian = "little")
    }
    values[values %in% c(-9999, -999999998)] <- NA
    missing_value <- layout$missing_value[col]
    if (!is.na(missing_value)) {
      values[values == missing_value] <- NA
    }
    values
  })

  result <- as.data.frame(columns, check.names = FALSE, stringsAsFactors = FALSE)
  names(result) <- layout$name
  result
}

#' Reads meta data from a binary Version 7 STA file
#'
#' Reads variable metadata from a supported binary STA file.
#' @param data data from a binary STA file
#' @param metadata data from a binary STA file
#' @param n_obs number of observations from a binary STA file
#' @param n_vars number of variables from a binary STA file
#' @param row_stride dataset length from a binary STA file
#'
#' @return
#' attr(x, "variable_metadata")
#' attr(x, "value_labels")
#' attr(x, "chr_values")
#' attr(x, "rownames")
#' attr(x, "file_notes")
#' attr(x, "file_notes2")
#'
#' @examples
#' \dontrun{
#' x <- decode_sta7("data/training-v7.sta")
#' attr(x, "variable_metadata")
#' attr(x, "value_labels")
#' attr(x, "chr_values")
#' attr(x, "rownames")
#' attr(x, "file_notes")
#' attr(x, "file_notes2")
#' }
#' @export
decode_metadata_records <- function(
  data,
  metadata,
  n_obs,
  n_vars,
  row_stride
) {
  if (length(metadata) < 5L) {
    stop("STA file contains no complete variable meta data.")
  }

  marker <- as.raw(c(1L, 0L, 0L, 0L, 2L))
  marker_starts <- which(vapply(
    seq_len(length(metadata) - length(marker) + 1L),
    function(i) identical(metadata[i:(i + length(marker) - 1L)], marker),
    logical(1)
  ))
  notes_signature <- as.raw(c(
    255L, 64L, 1L, 33L, 126L, 92L, 70L, 84L, 44L, 44L, 44L
  ))
  metadata_notes <- character()
  metadata_notes2 <- character()
  first_descriptor_start <- if (length(marker_starts)) {
    min(marker_starts)
  } else {
    length(metadata) + 1L
  }
  preamble <- metadata[seq_len(first_descriptor_start - 1L)]
  if (length(preamble) > length(notes_signature) &&
      identical(preamble[seq_along(notes_signature)], notes_signature)) {
    note_bytes <- preamble[-seq_along(notes_signature)]
    note_bytes <- note_bytes[seq_len(match(as.raw(0L), c(note_bytes, as.raw(0L))) - 1L)]
    note_text <- iconv(
      rawToChar(note_bytes),
      from = "CP1252",
      to = "UTF-8",
      sub = "byte"
    )
    metadata_notes <- trimws(strsplit(note_text, "\n", fixed = TRUE)[[1L]])
    metadata_notes <- metadata_notes[nzchar(metadata_notes)]
  } else if (length(preamble) > 1L &&
             identical(preamble[1L], as.raw(13L))) {
    file_note_bytes <- preamble[-1L]
    second_note_candidates <- if (
      identical(preamble[length(preamble)], as.raw(0L))
    ) {
      candidates <- seq.int(2L, length(preamble) - 2L)
      candidates[
        as.integer(preamble[candidates]) ==
          length(preamble) - candidates - 1L
      ]
    } else {
      integer()
    }
    has_second_note <- length(second_note_candidates) > 0L
    if (has_second_note) {
      second_note_start <- max(second_note_candidates)
      note_length <- length(preamble) - second_note_start - 1L
      file_note_bytes <- preamble[seq.int(2L, length.out = second_note_start - 2L)]
      second_note_bytes <- preamble[seq.int(
        second_note_start + 1L,
        length.out = note_length
      )]
      second_note <- iconv(
        rawToChar(second_note_bytes),
        from = "CP1252",
        to = "UTF-8",
        sub = "byte"
      )
      metadata_notes2 <- trimws(strsplit(second_note, "\n", fixed = TRUE)[[1L]])
      metadata_notes2 <- metadata_notes2[nzchar(metadata_notes2)]
    } else {
      null_position <- match(as.raw(0L), file_note_bytes)
      if (!is.na(null_position)) {
        file_note_bytes <- file_note_bytes[seq_len(null_position - 1L)]
      }
    }
    if (length(file_note_bytes)) {
      note_text <- iconv(
        rawToChar(file_note_bytes),
        from = "CP1252",
        to = "UTF-8",
        sub = "byte"
      )
      metadata_notes <- trimws(strsplit(note_text, "\n", fixed = TRUE)[[1L]])
      metadata_notes <- metadata_notes[nzchar(metadata_notes)]
    }
  }

  read_i32 <- function(position) {
    bytes <- metadata[position:(position + 3L)]
    readBin(bytes, integer(), n = 1L, size = 4L, endian = "little")
  }

  read_value_labels <- function(type_position) {
    empty_labels <- function() {
      data.frame(code = numeric(), label = character())
    }
    count_position <- type_position + 27L
    if (count_position + 3L > length(metadata)) {
      return(empty_labels())
    }
    count <- read_i32(count_position)
    if (is.na(count) || count < 0L || count > 256L) {
      return(empty_labels())
    }

    position <- count_position + 4L
    if (count == 0L) {
      labels <- empty_labels()
      attr(labels, "end_position") <- position
      return(labels)
    }
    labels <- vector("list", count)
    chr_values <- vector("list", count)
    has_chr_values <- FALSE
    for (i in seq_len(count)) {
      if (position + 8L > length(metadata)) {
        return(empty_labels())
      }
      code <- readBin(
        metadata[position:(position + 7L)],
        numeric(),
        n = 1L,
        size = 8L,
        endian = "little"
      )
      label_length <- as.integer(metadata[position + 8L])
      label_start <- position + 9L
      label_end <- label_start + label_length - 1L
      if (label_length < 1L || label_end > length(metadata)) {
        return(empty_labels())
      }
      label_bytes <- metadata[label_start:label_end]
      if (any(label_bytes == as.raw(0L))) {
        return(empty_labels())
      }
      label <- iconv(
        rawToChar(label_bytes),
        from = "CP1252",
        to = "UTF-8",
        sub = "byte"
      )
      labels[[i]] <- data.frame(code = code, label = label)

      chr_length_position <- label_end + 1L
      if (chr_length_position > length(metadata)) {
        return(empty_labels())
      }
      chr_length <- as.integer(metadata[chr_length_position])
      if (!is.na(chr_length) && chr_length > 0L) {
        if (chr_length > 254L ||
            label_end + 1L + chr_length > length(metadata)) {
          return(empty_labels())
        }
        chr_start <- label_end + 2L
        chr_end <- chr_start + chr_length - 1L
        chr_bytes <- metadata[chr_start:chr_end]
        if (any(chr_bytes == as.raw(0L))) {
          return(empty_labels())
        }
        chr_values[[i]] <- data.frame(
          code = code,
          chr = iconv(
            rawToChar(chr_bytes),
            from = "CP1252",
            to = "UTF-8",
            sub = "byte"
          ),
          stringsAsFactors = FALSE
        )
        has_chr_values <- TRUE
        position <- chr_end + 1L
      } else {
        chr_values[[i]] <- data.frame(
          code = code,
          chr = "",
          stringsAsFactors = FALSE
        )
        position <- label_end + 2L
      }
    }
    result <- do.call(rbind, labels)
    if (has_chr_values) {
      attr(result, "chr_values") <- do.call(rbind, chr_values)
    }
    attr(result, "end_position") <- position
    result
  }

  descriptors <- lapply(marker_starts, function(start) {
    if (start + 5L > length(metadata)) {
      return(NULL)
    }
    name_length <- as.integer(metadata[start + 5L])
    name_start <- start + 6L
    description_length_position <- name_start + name_length
    if (name_length < 1L || description_length_position > length(metadata)) {
      return(NULL)
    }

    description_length <- as.integer(metadata[description_length_position])
    if (description_length == 255L) {
      if (description_length_position + 2L > length(metadata)) {
        return(NULL)
      }
      description_length <- sum(as.integer(metadata[
        (description_length_position + 1L):(description_length_position + 2L)
      ]) * c(1, 256))
      description_start <- description_length_position + 3L
      type_position <- description_length_position + 3L + description_length
    } else {
      description_start <- description_length_position + 1L
      type_position <- description_length_position + 1L + description_length
    }
    if (type_position + 7L > length(metadata)) {
      return(NULL)
    }
    fin = FALSE
    tryCatch({
      name <- rawToChar(metadata[name_start:(name_start + name_length - 1L)])
      fin = TRUE
    }, error = function(e) {
    })
    if (fin == FALSE) {
      name <- metadata[name_start:(name_start + name_length - 1L)]
      name <- name[name != as.raw(0)]
      name <- rawToChar(name)
      # name <- rawToCharNull(metadata[name_start:(name_start + name_length - 1L)])
    }
    # name <- metadata[name_start:(name_start + name_length - 1L)]
    # name <- name[name != as.raw(0)]
    # name <- rawToChar(name)
    name <- iconv(name, from = "CP1252", to = "UTF-8", sub = "byte")
    
    if (description_length > 0L) {
      fin <- FALSE
      tryCatch({
        description <- rawToChar(metadata[seq.int(
          description_start,
          length.out = description_length
        )])
      fin <- TRUE
      },
      error = function(e) {
      })
      if (fin == FALSE) {
        description <- metadata[seq.int(description_start, length.out = description_length)]
        description <- description[description != as.raw(0)]
        description <- rawToChar(description)
      }
      description <- iconv(
        description,
        from = "CP1252",
        to = "UTF-8",
        sub = "byte"
      )
    } else {
      description <- ""
    }
    
    type <- read_i32(type_position)
    width <- read_i32(type_position + 4L)
    if (!((type == 0L && width == 8L) ||
          (type == 1L && width > 0L) ||
          (type == 2L && width == 4L) ||
          (type == 3L && width == 1L))) {
      return(NULL)
    }

    labels <- read_value_labels(type_position)
    end_position <- attr(labels, "end_position")
    if (is.null(end_position)) {
      end_position <- type_position + 31L
    }
    list(
      name = name,
      text = description,
      type = type,
      width = width,
      value_labels = labels,
      end_position = end_position
    )
  })
  descriptors <- Filter(Negate(is.null), descriptors)
  if (!length(descriptors)) {
    stop("No variable meta data inside STA file.")
  }
  descriptor_names <- vapply(descriptors, `[[`, character(1), "name")
  descriptor_names <- make.unique(descriptor_names, sep = "_v7_")
  descriptor_labels <- lapply(descriptors, `[[`, "value_labels")
  descriptor_ends <- vapply(descriptors, `[[`, numeric(1), "end_position")
  descriptors <- data.frame(
    name = descriptor_names,
    text = vapply(descriptors, `[[`, character(1), "text"),
    type = vapply(descriptors, `[[`, integer(1), "type"),
    width = vapply(descriptors, `[[`, integer(1), "width"),
    stringsAsFactors = FALSE
  )

  default_missing_bytes <- function(type, width) {
    if (type == 1L) {
      return(as.raw(rep(0L, width)))
    }
    if (type == 3L) {
      return(as.raw(255L))
    }
    if (type == 2L) {
      return(writeBin(-999999998L, raw(), size = width, endian = "little"))
    }
    writeBin(-999999998, raw(), size = width, endian = "little")
  }
  read_missing_values <- function(metadata, descriptors, minimum_start) {
    defaults <- vapply(seq_len(nrow(descriptors)), function(i) {
      if (descriptors$type[i] == 1L) {
        return(NA_real_)
      }
      if (descriptors$type[i] == 3L) {
        return(255)
      }
      -999999998
    }, numeric(1))
    field_widths <- descriptors$width
    field_offsets <- c(0L, utils::head(cumsum(field_widths), -1L))
    vector_width <- sum(field_widths)
    if (vector_width > length(metadata)) {
      return(defaults)
    }

    expected_first <- default_missing_bytes(
      descriptors$type[1L],
      descriptors$width[1L]
    )
    possible_starts <- which(metadata == expected_first[1L])
    possible_starts <- possible_starts[
      possible_starts >= minimum_start &
        possible_starts <= length(metadata) - vector_width + 1L
    ]
    for (byte in seq_along(expected_first)) {
      possible_starts <- possible_starts[
        metadata[possible_starts + byte - 1L] == expected_first[byte]
      ]
    }
    if (!length(possible_starts)) {
      return(defaults)
    }

    numeric_fields <- which(descriptors$type != 1L)
    scores <- integer(length(possible_starts))
    for (i in numeric_fields) {
      expected <- default_missing_bytes(
        descriptors$type[i],
        descriptors$width[i]
      )
      matches <- rep(TRUE, length(possible_starts))
      for (byte in seq_along(expected)) {
        matches <- matches &
          metadata[possible_starts + field_offsets[i] + byte - 1L] ==
            expected[byte]
      }
      scores <- scores + as.integer(matches)
    }
    best <- which.max(scores)
    if (scores[best] < ceiling(0.75 * length(numeric_fields))) {
      return(defaults)
    }

    start <- possible_starts[best]
    vapply(seq_len(nrow(descriptors)), function(i) {
      type <- descriptors$type[i]
      if (type == 1L) {
        return(NA_real_)
      }
      position <- start + field_offsets[i]
      bytes <- metadata[seq.int(position, length.out = descriptors$width[i])]
      if (type == 3L) {
        return(as.numeric(bytes[1L]))
      }
      if (type == 2L) {
        return(as.numeric(readBin(
          bytes, integer(), n = 1L, size = descriptors$width[i],
          endian = "little"
        )))
      }
      readBin(bytes, numeric(), n = 1L, size = descriptors$width[i],
              endian = "little")
    }, numeric(1))
  }
  descriptor_missing_values <- read_missing_values(
    metadata,
    descriptors,
    max(descriptor_ends)
  )

  first_descriptor <- descriptors[1L, ]
  record_prefix <- 0L
  if (first_descriptor$type == 1L &&
      first_descriptor$width * 2L <= row_stride &&
      n_obs > 0L) {
    width <- first_descriptor$width
    first_field <- data[seq_len(width)]
    repeated_field <- data[width + seq_len(width)]
    if (any(first_field != as.raw(0L)) &&
        identical(first_field, repeated_field)) {
      record_prefix <- width
    }
  }

  if (nrow(descriptors) != n_vars) {
    stop("The number of STA variable meta data does not match header information.")
  }
  names <- descriptors$name
  text <- descriptors$text
  types <- descriptors$type
  widths <- descriptors$width

  value_labels <- descriptor_labels[match(names, descriptors$name)]
  missing_values <- descriptor_missing_values[match(names, descriptors$name)]
  value_labels <- lapply(value_labels, function(labels) {
    if (is.null(labels)) {
      data.frame(code = numeric(), label = character())
    } else {
      labels
    }
  })
  if (record_prefix == 0L) {
    record_prefix <- row_stride - 4L - sum(widths)
  }
  if (length(widths) != n_vars) {
    stop("The recognized variable definitions do not match variable number.")
  }
  if (record_prefix < 0L) {
    stop("STA meta data variable widths do not match record length.")
  }
  if (record_prefix + sum(widths) + 4L != row_stride) {
    stop("STA meta data variable widths do not match record length.")
  }

  layout <- data.frame(
    name = names,
    offset =     record_prefix + c(0L, utils::head(cumsum(widths), -1L)),
    width = widths,
    missing_value = missing_values,
    type = ifelse(types == 1L, "character",
                  ifelse(types == 2L, "integer",
                         ifelse(types == 3L, "byte_integer", "double")))
  )
  storage_types <- c("Double", "String", "Integer", "Byte")
  variable_metadata <- data.frame(
    name = names,
    text = text,
    type = storage_types[types + 1L],
    width = widths,
    storage_code = types,
    missing_value = missing_values,
    stringsAsFactors = FALSE
  )
  result <- decode_fixed_width_records(data, n_obs, row_stride, layout)

  if (record_prefix > 0L) {
    row_starts <- (seq_len(n_obs) - 1L) * row_stride
    case_names <- vapply(seq_len(n_obs), function(row) {
      field <- data[seq.int(
        row_starts[row] + 1L,
        length.out = record_prefix
      )]
      terminator <- match(as.raw(0L), field)
      if (!is.na(terminator)) {
        field <- field[seq_len(terminator - 1L)]
      }
      if (!length(field)) {
        return(NA_character_)
      }
      value <- trimws(iconv(
        rawToChar(field),
        from = "CP1252",
        to = "UTF-8",
        sub = "byte"
      ))
      if (nzchar(value)) value else NA_character_
    }, character(1))
    attr(result, "rownames") <- case_names
  }

  attr(result, "variable_metadata") <- variable_metadata
  attr(result, "value_labels") <- stats::setNames(value_labels, names)
  if (length(metadata_notes)) {
    attr(result, "file_notes") <- metadata_notes
  }
  if (length(metadata_notes2)) {
    attr(result, "file_notes2") <- metadata_notes2
  }
  result
}


#' Decode a binary Version 7 STA file
#'
#' Reads observations and variable metadata from a supported binary STA file.
#' Numeric category codes are retained in the returned data frame; their
#' textual labels are available in attributes.
#' @param path Path to a binary STA file.
#'
#' @return A data frame with one row per observation and one column per
#'   variable. Attributes may include `variable_metadata`, `value_labels`,
#'   `chr_values`, `file_notes`, `file_notes2`, and `rownames`, depending on
#'   which structures are present in the file.
#'
#' @examples
#' \dontrun{
#' x <- decode_sta7("data/training-v7.sta")
#' attr(x, "variable_metadata")
#' attr(x, "value_labels")
#' attr(x, "chr_values")
#' attr(x, "rownames")
#' attr(x, "file_notes")
#' attr(x, "file_notes2")
#' }
#' @export

decode_sta7 <- function(path) {
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

  if (has_variable_width_fields || has_metadata_descriptors) {
    message(path, ": Version-7-STA file with metadata descriptors detected.")
    result <- decode_metadata_records(
      data, metadata, n_obs, n_vars, row_stride
    )
  } else {
    message(path, ": Version-7-STA file without metadata descriptors detected.")
    values <- matrix(NA_real_, nrow = n_obs, ncol = n_vars)
    for (row in seq_len(n_obs)) {
      row_start <- data_start + (row - 1L) * row_stride
      for (col in seq_len(n_vars)) {
        seek(con, where = row_start + (col - 1L) * 8L, origin = "start")
        value <- readBin(con, numeric(), n = 1L, size = 8L, endian = "little")
        if (length(value) != 1L) {
          stop("STA file contains incomplete numerical data.")
        }
        values[row, col] <- value
      }
    }

    values[values %in% c(-9999, -999999998)] <- NA_real_
    result <- as.data.frame(values)
    names(result) <- paste0("Var", seq_len(n_vars))
    attr(result, "variable_metadata") <- data.frame(
      name = names(result),
      text = "",
      type = "Double",
      width = 8L,
      storage_code = 0L,
      missing_value = -999999998,
      stringsAsFactors = FALSE
    )
  }

  value_labels <- attr(result, "value_labels")
  if (is.null(value_labels)) {
    value_labels <- stats::setNames(
      rep(list(data.frame(code = numeric(), label = character())), ncol(result)),
      names(result)
    )
  }
  attr(result, "value_labels") <- value_labels
  chr_values <- do.call(rbind, lapply(seq_along(value_labels), function(col) {
    labels <- value_labels[[col]]
    character_values <- attr(labels, "chr_values")
    if (!is.null(character_values) && nrow(character_values)) {
      return(data.frame(
        variable = names(value_labels)[col],
        code = character_values$code,
        chr = character_values$chr,
        stringsAsFactors = FALSE
      ))
    }
    if (!nrow(labels)) {
      return(NULL)
    }
    data.frame(
      variable = names(value_labels)[col],
      code = labels$code,
      chr = labels$label,
      stringsAsFactors = FALSE
    )
  }))
  if (is.null(chr_values)) {
    chr_values <- data.frame(
      variable = character(),
      code = numeric(),
      chr = character(),
      stringsAsFactors = FALSE
    )
  }
  rownames(chr_values) <- NULL
  attr(result, "chr_values") <- chr_values
  message("n = ", n_vars, " variables and n = ", n_obs, " observations loaded.")

  return(result)
}
