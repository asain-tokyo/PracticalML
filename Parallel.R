# Parallel processing
# install.packages("doParallel")
library(doParallel)
cl <- makePSOCKcluster(10)
registerDoParallel(cl)
