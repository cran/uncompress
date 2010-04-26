\name{uncompress}
\alias{uncompress}
\title{Uncompress .Z file data}
\description{Uncompresses data compressed with the "compress" utility. Data pass in and returned are both raw binary strings. Invalid compressed data results in an error being returned.}
\usage{uncompress(data)}
\arguments{\item{data}{The raw binary data to decompress.}}
\examples{
library("uncompress")
\dontrun{

## Example 1 - extracting text data from a .Z file.
handle <- file("file.Z", "rb")
# The size here is arbitrary, it should be large enough for most files,
#  adjust as necessary.
data <- readBin(handle, "raw", 99999999)
close(handle)
uncomp_data <- uncompress(data)
# At this point you might want to check if uncomp_data is NULL in case the
#  file is corrupt.
# This is assuming it's Unix-style text (i.e. \n separates lines).
# If it's Windows-style text you may need to use "\r\n".
lines <- strsplit(rawToChar(uncomp_data), "\n")
print(lines)

## Example 2 - extracting text data from a .Z file at an HTTP host.
handle <- url("http://host/path/file.Z", "rb")
# The size here is arbitrary, it should be large enough for most files,
#  adjust as necessary.
data <- readBin(handle, "raw", 99999999)
close(handle)
uncomp_data <- uncompress(data)
# At this point you might want to check if uncomp_data is NULL in case the
#  file is corrupt.
# This is assuming it's Unix-style text (i.e. \n separates lines).
# If it's Windows-style text you may need to use "\r\n".
lines <- strsplit(rawToChar(uncomp_data), "\n")
print(lines)

## Example 3 - downloading a .Z file, then uncompressing it.
# Mode is important, in Windows if you don't use binary mode the compressed
#  data will be corrupted.
download.file("http://host/path/file.Z", "file.Z", mode="wb")
handle <- file("file.Z", "rb")
# The size here is arbitrary, it should be large enough for most files,
#  adjust as necessary.
data <- readBin(handle, "raw", 99999999)
close(handle)
uncomp_data <- uncompress(data)
# At this point you might want to check if uncomp_data is NULL in case the
#  file is corrupt.
# This is assuming it's Unix-style text (i.e. \n separates lines).
# If it's Windows-style text you may need to use "\r\n".
lines <- strsplit(rawToChar(uncomp_data), "\n")
print(lines)

## Example 4 - downloading a .Z file, writing the uncompressed data to a
##  local file.
handle <- url("http://host/path/file.Z", "rb")
# The size here is arbitrary, it should be large enough for most files,
#  adjust as necessary.
data <- readBin(handle, "raw", 99999999)
close(handle)
uncomp_data <- uncompress(data)
handle <- file("file", "wb")
writeBin(uncomp_data, handle)
close(handle)

}
}
\keyword{file}
