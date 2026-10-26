\name{decode_sta7}
\alias{decode_sta7}
\title{Decode a Version 7 STA file}
\usage{
decode_sta7(path)
}
\arguments{
\item{path}{Path to a Version 7 STA file.}
}
\value{
A data frame with one row per observation and one column per variable.
Attributes may include \code{variable_metadata}, \code{value_labels},
\code{chr_values}, \code{file_notes}, \code{file_notes2}, \code{rownames},
and \code{decode_notes}.
}
\description{
Reads Version 7 STA files stored in teh legacy format. The
decoded data frame includes variable metadata, missing-value codes, value
labels and their character values, file notes, and case names when present.
Files both with and without case-record prefixes are supported.
}
\examples{
\dontrun{
x <- decode_sta7("example.sta")
attr(x, "variable_metadata")
attr(x, "rownames")
}
}
