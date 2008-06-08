library("uncompress")
handle <- file("test.Z", "rb")
data <- readBin(handle, "raw", 9999999)
close(handle)
uncomp_data <- uncompress(data)
print(uncomp_data);
