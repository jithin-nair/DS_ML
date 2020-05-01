library(randomForest)
fit <- fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25) 
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")