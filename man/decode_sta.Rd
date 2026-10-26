\name{decode_sta}
\alias{decode_sta}
\title{Decode a binary STA file}
\usage{
decode_sta(path)
}
\arguments{
\item{path}{Path to a binary STA file.}
}
\value{
A data frame with one row per observation and one column per variable.
Attributes may include \code{variable_metadata}, \code{value_labels},
\code{chr_values}, \code{file_notes}, \code{file_notes2}, and \code{rownames},
depending on which structures are present in the file.
}
\description{
Reads observations and variable metadata from a supported binary STA file.
Numeric category codes are retained in the returned data frame; their
textual labels are available in attributes.

}
\examples{
\dontrun{
x <- decode_sta("data/example.sta")
attr(x, "variable_metadata")
attr(x, "chr_values")
}
}
