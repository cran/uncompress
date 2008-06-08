\name{uncompress}
\alias{uncompress}
\title{Uncompress .Z file data}
\description{Uncompresses data compressed with the "compress" utility. Data pass in and returned are both raw binary strings.}
\usage{uncompress(data)}
\arguments{\item{data}{The raw binary data to decompress.}}
\examples{
library("uncompress")
\dontrun{
handle <- file("file.Z", "rb")
data <- readBin(handle, "raw", 9999999)
close(handle)
uncomp_data <- uncompress(data)
}
}
\keyword{file}
