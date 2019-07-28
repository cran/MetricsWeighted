## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)

## ----setup---------------------------------------------------------------
library(MetricsWeighted)

## ------------------------------------------------------------------------
# The data
y_num <- iris[["Sepal.Length"]]
fit_num <- lm(Sepal.Length ~ ., data = iris)
pred_num <- fit_num$fitted
weights <- seq_len(nrow(iris))

# Performance metrics
mae(y_num, pred_num)  # unweighted
mae(y_num, pred_num, w = rep(1, length(y_num)))  # same
mae(y_num, pred_num, w = weights)  # different

rmse(y_num, pred_num)


## ------------------------------------------------------------------------
# The data
y_cat <- iris[["Species"]] == "setosa"
fit_cat <- glm(y_cat ~ Sepal.Length, data = iris, family = binomial())
pred_cat <- predict(fit_cat, type = "response")

# Performance metrics
AUC(y_cat, pred_cat)  # unweighted
AUC(y_cat, pred_cat, w = weights)  # weighted
logLoss(y_cat, pred_cat)  # Logloss
deviance_bernoulli(y_cat, pred_cat)  # LogLoss * 2


## ------------------------------------------------------------------------
summary(fit_num)$r.squared

# same
r_squared(y_num, pred_num)
r_squared(y_num, pred_num, deviance_function = deviance_tweedie, tweedie_p = 0)

# weighted
r_squared(y_num, pred_num, w = weights)
r_squared(y_num, pred_num, w = weights, deviance_function = deviance_gamma) 
r_squared(y_num, pred_num, w = weights, deviance_function = deviance_tweedie, tweedie_p = 2)


## ------------------------------------------------------------------------
require(dplyr)

# Regression with `Sepal.Length` as response
iris %>% 
  mutate(pred = predict(fit_num, data = .)) %>% 
  performance("Sepal.Length", "pred")

# Same
iris %>% 
  mutate(pred = predict(fit_num, data = .)) %>% 
  performance("Sepal.Length", "pred", metrics = rmse)

# Grouped by Species
iris %>% 
  mutate(pred = predict(fit_num, data = .)) %>% 
  group_by(Species) %>% 
  do(performance(data = ., actual = "Sepal.Length", predicted = "pred"))

# Customized output
iris %>% 
  mutate(pred = predict(fit_num, data = .)) %>% 
  performance("Sepal.Length", "pred", value = "performance",
              metrics = list(`root-mean-squared error` = rmse))

# Multiple measures
iris %>% 
  mutate(pred = predict(fit_num, data = .)) %>% 
  performance("Sepal.Length", "pred",
              metrics = list(rmse = rmse, mae = mae, `R-squared` = r_squared))

# Grouped by Species
iris %>% 
  mutate(pred = predict(fit_num, data = .)) %>% 
  group_by(Species) %>% 
  do(performance(., "Sepal.Length", "pred",
                 metrics = list(rmse = rmse, mae = mae, `R-squared` = r_squared)))

# Passing extra argument (Tweedie p)
iris %>% 
  mutate(pred = predict(fit_num, data = .)) %>% 
  performance("Sepal.Length", "pred",
              metrics = list(`normal deviance` = deviance_normal, 
                             `Tweedie with p = 0` = deviance_tweedie),
              tweedie_p = 0)


