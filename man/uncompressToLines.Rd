\name{uncompressToLines}
\alias{uncompressToLines}
\title{Uncompress data and return it as a string vector}
\description{This function is a wrapper for uncompress() and rawToLines(). First it uncompresses the raw binary vector passed as the first argument, then it splits it according to line breaks (LF [Unix] or CR/LF [Windows] style), and returns a string vector with one line per entry. If there is a blank line at the end of the file, it is included. Optionally, you can skip a number of lines from the start of the file by passing an integer as the second argument. You can also, optionally, limit the number of lines that will be returned by passing an integer as the third argument.}
\usage{uncompressToLines(data, start_line = 0, max_line_count = 999999999)}
\arguments{\item{data}{The raw compressed binary data.}
           \item{start_line}{The index of the first line to return. Defaults to 0. In effect, processing of the data begins after skipping this many lines from the start. If it is greater than the number of lines in the file, an error will be returned.}
           \item{max_line_count}{If more than this many lines would be returned, the rest are ignored. Otherwise it has no effect. The default is 999999999.}}
\examples{
library("uncompress")
\dontrun{

## Example 1 - uncompress a file, then split it into strings by line.
handle <- file("file.Z", "rb")
# The size here is arbitrary, it should be large enough for most files,
#  adjust as necessary.
data <- readBin(handle, "raw", 99999999)
close(handle)
text_lines <- uncompressToLines(data)
print(text_lines)

## Example 2 - the same, except does the conversion 1000 lines at a time.
##             Note that this will uncompress all the data for each call!
##             Example 3 is a much better way to do it.
handle <- file("file.Z", "rb")
# The size here is arbitrary, it should be large enough for most files,
#  adjust as necessary.
data <- readBin(handle, "raw", 99999999)
close(handle)
start <- 0
while( 1 ) {
	text_lines <- uncompressToLines(data, start, 1000)
	if( length(text_lines) == 0 )
		break;
	print(text_lines);
	if( length(text_lines) < 1000 )
		break;
	start <- start + 1000
}

## Example 3 - Smarter version of Example 2
handle <- file("file.Z", "rb")
# The size here is arbitrary, it should be large enough for most files,
#  adjust as necessary.
data <- readBin(handle, "raw", 99999999)
close(handle)
data <- uncompress(data);
start <- 0
while( 1 ) {
	text_lines <- rawToLines(data, start, 1000)
	if( length(text_lines) == 0 )
		break;
	print(text_lines);
	if( length(text_lines) < 1000 )
		break;
	start <- start + 1000
}

}
}
\keyword{file}
