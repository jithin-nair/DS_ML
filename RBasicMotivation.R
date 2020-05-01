library(dslabs)
data(murders)

library(dslabs)
data(na_example)
na_example
ind <- is.na(na_example)

# Compute the average, for entries of na_example that are not NA 
mean(!ind)
mean(na_example[!ind])
na_example[!ind]