#Cross Validation and Bootstrap
n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, color = I("black"))

m <- median(income)
m

#set.seed(1995)
set.seed(1995, sample.kind="Rounding") #instead if using R 3.6 or later
N <- 250
X <- sample(income, N)
M<- median(X)
M

library(gridExtra)
B <- 10^4
M <- replicate(B, {
  X <- sample(income, N)
  median(X)
})
p1 <- qplot(M, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(M)) + geom_abline()
grid.arrange(p1, p2, ncol = 2)

mean(M)
sd(M)

B <- 10^4
M_star <- replicate(B, {
  X_star <- sample(X, N, replace = TRUE)
  median(X_star)
})

tibble(monte_carlo = sort(M), bootstrap = sort(M_star)) %>%
  qplot(monte_carlo, bootstrap, data = .) + 
  geom_abline()

quantile(M, c(0.05, 0.95))
quantile(M_star, c(0.05, 0.95))

median(X) + 1.96 * sd(X) / sqrt(N) * c(-1, 1)

mean(M) + 1.96 * sd(M) * c(-1,1)

mean(M_star) + 1.96 * sd(M_star) * c(-1, 1)


#Fail_Q345
set.seed(1, sample.kind = "Rounding")
y <- rnorm(100, 0, 1)

indexes <- createResample(y, 10)

ks = seq(1,10,1)

B <- 10^4
M <- replicate(B, {
  X <- sample(y,25)
})

A = seq(1,10000,1)
anw3 = sapply(A, function(k){
  qt = quantile(M[,k], 0.75)
})

mean(anw3)
sd(anw3)