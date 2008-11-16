.First.lib <- function(lib_loc, package) {
	library.dynam("uncompress", package, lib_loc);
}

uncompress <- function(data) {
	return(.Call("uncompress", data, PACKAGE = "uncompress"));
}
rawToLines <- function(data, start_line=0, max_line_count=999999999) {
	return(.Call("rawToLines", data, start_line, max_line_count, PACKAGE = "uncompress"));
}
uncompressToLines <- function(data, start_line=0, max_line_count=999999999) {
	return(.Call("rawToLines", uncompress(data), start_line, max_line_count, PACKAGE = "uncompress"));
}
