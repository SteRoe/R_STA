write_i32 <- function(value) {
  writeBin(as.integer(value), raw(), size = 4L, endian = "little")
}

write_f64 <- function(value) {
  writeBin(as.numeric(value), raw(), size = 8L, endian = "little")
}

set_header_i32 <- function(header, offset, value) {
  header[seq.int(offset + 1L, length.out = 4L)] <- write_i32(value)
  header
}

make_descriptor <- function(
  name,
  type,
  width,
  text = "",
  labels = NULL,
  chr_values = NULL
) {
  name_bytes <- charToRaw(name)
  text_bytes <- charToRaw(text)
  if (length(name_bytes) > 255L || length(text_bytes) > 254L) {
    stop("Synthetic fixture descriptor text exceeds supported short lengths.")
  }

  descriptor <- c(
    as.raw(c(1L, 0L, 0L, 0L, 2L)),
    as.raw(length(name_bytes)),
    name_bytes,
    as.raw(length(text_bytes)),
    text_bytes,
    write_i32(type),
    write_i32(width),
    as.raw(rep(0L, 19L))
  )
  if (is.null(labels)) {
    labels <- data.frame(code = numeric(), label = character())
  }
  if (!is.null(chr_values) && length(chr_values) != nrow(labels)) {
    stop("Synthetic chr_values must match the number of value labels.")
  }
  descriptor <- c(descriptor, write_i32(nrow(labels)))
  for (i in seq_len(nrow(labels))) {
    label_bytes <- charToRaw(labels$label[i])
    if (length(label_bytes) > 254L) {
      stop("Synthetic value label exceeds the supported short length.")
    }
    entry <- c(
      write_f64(labels$code[i]),
      as.raw(length(label_bytes)),
      label_bytes
    )
    if (is.null(chr_values)) {
      entry <- c(entry, as.raw(0L))
    } else {
      chr_bytes <- charToRaw(chr_values[i])
      if (length(chr_bytes) > 254L) {
        stop("Synthetic character value exceeds the supported short length.")
      }
      entry <- c(entry, as.raw(length(chr_bytes)), chr_bytes)
    }
    descriptor <- c(descriptor, entry)
  }
  descriptor
}

make_mixed_sta_fixture <- function(path) {
  variables <- list(
    list(name = "CASE", type = 1L, width = 4L, text = ""),
    list(name = "SCORE", type = 2L, width = 4L, text = "integer score"),
    list(name = "FLAG", type = 3L, width = 1L, text = "byte flag"),
    list(
      name = "GROUP",
      type = 3L,
      width = 1L,
      text = "group code",
      labels = data.frame(code = c(0, 1), label = c("No", "Yes"))
    ),
    lapply(seq_len(8L), function(i) {
      list(
        name = paste0("AUX", i),
        type = 3L,
        width = 1L,
        text = ""
      )
    })
  )
  aux_variables <- variables[[5L]]
  variables <- append(variables[1:3], list(variables[[4L]]))
  variables <- c(variables, aux_variables)
  field_widths <- vapply(variables, `[[`, integer(1), "width")
  missing_values <- c(list(
    as.raw(rep(0L, 4L)),
    write_i32(-1000L),
    as.raw(222L)
  ), rep(list(as.raw(255L)), 9L))
  rows <- list(
    list(case = "C001", score = 10L, flag = 1L, group = 1L),
    list(case = "C002", score = -1000L, flag = 222L, group = 0L),
    list(case = "C003", score = 30L, flag = 0L, group = 255L)
  )
  records <- lapply(rows, function(row) {
    c(
      charToRaw(row$case),
      charToRaw(row$case),
      write_i32(row$score),
      as.raw(row$flag),
      as.raw(row$group),
      as.raw(rep(0L, 8L)),
      as.raw(rep(0L, 4L))
    )
  })
  data <- do.call(c, records)
  row_stride <- 4L + sum(field_widths) + 4L
  note_preamble <- c(
    as.raw(13L),
    charToRaw("Header"),
    as.raw(3L),
    charToRaw("Top"),
    as.raw(0L)
  )
  descriptors <- lapply(variables, function(variable) {
    make_descriptor(
      variable$name,
      variable$type,
      variable$width,
      variable$text,
      variable$labels
    )
  })
  metadata <- c(note_preamble, do.call(c, descriptors), do.call(c, missing_values))
  header <- raw(256L)
  header <- set_header_i32(header, 0xEC, length(variables))
  header <- set_header_i32(header, 0xF0, length(rows))
  header <- set_header_i32(header, 0xFC, row_stride)
  writeBin(c(header, data, metadata), path)
  invisible(path)
}

make_duplicate_name_sta_fixture <- function(path) {
  variables <- list(
    list(name = "TEXTVAR1", type = 0L, width = 8L, text = ""),
    list(name = "TEXTVAR1", type = 0L, width = 8L, text = "")
  )
  note_preamble <- c(
    as.raw(13L),
    charToRaw("Header"),
    as.raw(3L),
    charToRaw("Top"),
    as.raw(0L)
  )
  descriptors <- lapply(variables, function(variable) {
    make_descriptor(
      variable$name,
      variable$type,
      variable$width,
      variable$text
    )
  })
  missing_values <- rep(list(write_f64(-999999998)), length(variables))
  metadata <- c(note_preamble, do.call(c, descriptors),
                do.call(c, missing_values))
  header <- raw(256L)
  header <- set_header_i32(header, 0xEC, length(variables))
  header <- set_header_i32(header, 0xF0, 1L)
  header <- set_header_i32(header, 0xFC, 20L)
  data <- c(write_f64(1), write_f64(2), as.raw(rep(0L, 4L)))
  writeBin(c(header, data, metadata), path)
  invisible(path)
}

make_fixed_numeric_sta_fixture <- function(path) {
  variables <- list(
    list(name = "AGE", type = 0L, width = 8L, text = "age"),
    list(
      name = "STATUS",
      type = 0L,
      width = 8L,
      text = "status",
      labels = data.frame(code = c(1, 2), label = c("Active", "Inactive"))
    )
  )
  rows <- rbind(c(58, 1), c(42, 2), c(35, -999999998))
  data <- raw()
  for (row in seq_len(nrow(rows))) {
    data <- c(
      data,
      write_f64(rows[row, 1L]),
      write_f64(rows[row, 2L]),
      as.raw(rep(0L, 4L))
    )
  }
  descriptors <- lapply(variables, function(variable) {
    make_descriptor(
      variable$name,
      variable$type,
      variable$width,
      variable$text,
      variable$labels
    )
  })
  missing_values <- rep(list(write_f64(-999999998)), length(variables))
  metadata <- c(do.call(c, descriptors), do.call(c, missing_values))
  header <- raw(256L)
  header <- set_header_i32(header, 0xEC, length(variables))
  header <- set_header_i32(header, 0xF0, nrow(rows))
  header <- set_header_i32(header, 0xFC, 20L)
  writeBin(c(header, data, metadata), path)
  invisible(path)
}

write_u16 <- function(value) {
  as.raw(c(value %% 256, floor(value / 256) %% 256))
}

write_u32_unsigned <- function(value) {
  as.raw(c(
    value %% 256,
    floor(value / 256) %% 256,
    floor(value / 256^2) %% 256,
    floor(value / 256^3) %% 256
  ))
}

write_u64_unsigned <- function(value) {
  c(
    write_u32_unsigned(value %% 2^32),
    write_u32_unsigned(floor(value / 2^32))
  )
}

set_bytes_at <- function(bytes, offset, value) {
  bytes[seq.int(offset + 1L, length.out = length(value))] <- value
  bytes
}
