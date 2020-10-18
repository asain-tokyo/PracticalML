# Download and install ElemStatLearn

url <- "http://cran.r-project.org/src/contrib/Archive/ElemStatLearn/ElemStatLearn_2015.6.26.tar.gz"
pkgFile <- "ElemStatLearn_2015.6.26.tar.gz"
download.file(url = url, destfile = pkgFile)

install.packages(pkgs = pkgFile, type = "source", repos = NULL)
library(ElemStatLearn)