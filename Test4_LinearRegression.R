### Comprehension Check: Linear Regression
library(caret)
library(tidyverse)

# Question 1

y_rmse <- c(1:100)

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
dat

set.seed(1)
for(i in 1:100) # For Loop (Should Use Replicate)
{    
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  
  # Train linear model
  fit <- lm(y ~ x, data = train_set)
  fit
  
  # Loss Function
  y_hat <- predict(fit, test_set)
  y_rmse[i] <- sqrt(mean((y_hat - test_set$y)^2))
  print(i)
}

y_rmse
mean(y_rmse)
sd(y_rmse)

# Question 2 

set.seed(1)

myRMSE <- function(size)
{
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = size, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  
  RMSE <- replicate(n = size, {
    
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    
    # Train linear model
    fit <- lm(y ~ x, data = train_set)
    
    # Loss Function
    y_hat <- predict(fit, test_set)
    sqrt(mean((y_hat - test_set$y)^2))
  })
  list(mean(RMSE),sd(RMSE))
}

n <- c(100, 500, 1000, 5000, 10000)
set.seed(1)
f<-sapply(n, myRMSE)
f

# Question 4

myRMSE <- function(size)
{
  set.seed(1)
  Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = size, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  RMSE <- replicate(n = size, {
    
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    
    # Train linear model
    fit <- lm(y ~ x, data = train_set)
    
    # Loss Function
    y_hat <- predict(fit, test_set)
    sqrt(mean((y_hat - test_set$y)^2))
  })
  list(mean(RMSE),sd(RMSE))
}

n <- c(100)
set.seed(1)
f<-sapply(n, myRMSE)
f

# Question 6
set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)

train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

# Train linear model
fit_1 <- lm(y ~ x_1, data = train_set)
fit_2 <- lm(y ~ x_2, data = train_set)
fit_12 <- lm(y ~ x_1 + x_2, data = train_set)

# Loss Functions
y_hat_1 <- predict(fit_1, test_set)
RMSE_1 <- sqrt(mean((y_hat_1 - test_set$y)^2))

y_hat_2 <- predict(fit_2, test_set)
RMSE_2 <- sqrt(mean((y_hat_2 - test_set$y)^2))

y_hat_12 <- predict(fit_12, test_set)
RMSE_12 <- sqrt(mean((y_hat_12 - test_set$y)^2))  



# Comprehension Check: Logistic Regression

library(tidyverse)
library(caret)

set.seed(2)

make_data <-
  function(n = 1000,
           p = 0.5,
           mu_0 = 0,
           mu_1 = 2,
           sigma_0 = 1,
           sigma_1 = 1) {
    y <-  rbinom(n, 1, p)
    f_0 <- rnorm(n, mu_0, sigma_0)
    f_1 <- rnorm(n, mu_1, sigma_1)
    x <- ifelse(y == 1, f_1, f_0)
    
    test_index <- createDataPartition(y, list = FALSE)
    list(
      train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
      test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index)
    )
  }

dat <- make_data()

dat$train %>%
  ggplot(aes(x, color = y)) +
  geom_density()

# Plot 25 different datasets with difference ranging from 0 to 3
# and plot accuracy vs mu_1
delta <- seq(0, 3, len = 25)

## First we compute accuracy using our initial dataset to
## develop a prototype computation. Since we have a binary
## outcome, we will use a logistic regression model (at
## this stage of the course that's all we know to use)
## Now, we start by training the model
fit <- glm(y ~ x, data = dat$train, family = 'binomial')
p_hat <- predict(fit, dat$test)
y_hat <- factor(ifelse(p_hat > 0.5, '1', '0'))
cfm <- confusionMatrix(y_hat, reference = dat$test$y)
cfm$overall['Accuracy']

## Extending this to the problem
acc <- map_dbl(delta, function(x) {
  dat <- make_data(mu_1 = x)
  fit <- glm(y ~ x, data = dat$train, family = 'binomial')
  p_hat <- predict(fit, dat$test, type = 'response')
  y_hat <- factor(ifelse(p_hat > 0.5, '1', '0'))
  cfm <- confusionMatrix(y_hat, reference = dat$test$y)
  cfm$overall['Accuracy']
})
qplot(delta, acc)