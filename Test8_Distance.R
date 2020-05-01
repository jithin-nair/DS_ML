library(dslabs)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)
d <- dist(tissue_gene_expression$x)
d
image(as.matrix(d))


#Question 2
# Distances
#x_1 <- as.matrix(d)[1:2]
#x_2 <- as.matrix(d)[39:40]
#x_3 <- as.matrix(d)[73:74]
#dist(x_1)
#dist(x_2)
#dist(x_3)
#x_a <- c(x_1, x_2, x_3)
d <- dist(tissue_gene_expression$x)
ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]
image(as.matrix(d))