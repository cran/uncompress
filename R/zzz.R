.First.lib <- function(lib_loc, package) {
	library.dynam("uncompress", package, lib_loc);
}
uncompress <- function(data) {
	return(.Call("uncompress", data, PACKAGE = "uncompress"));
}
