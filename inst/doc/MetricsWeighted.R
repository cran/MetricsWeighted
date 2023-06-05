## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  fig.width = 5.5,
  fig.height = 4.5
)

## -----------------------------------------------------------------------------
library(MetricsWeighted)

# The data
y_num <- iris[["Sepal.Length"]]
fit_num <- lm(Sepal.Length ~ ., data = iris)
pred_num <- fit_num$fitted
weights <- seq_len(nrow(iris))

# Performance metrics
rmse(y_num, pred_num)
rmse(y_num, pred_num, w = rep(1, length(y_num)))  # same
rmse(y_num, pred_num, w = weights)                # different
mae(y_num, pred_num)
medae(y_num, pred_num, w = weights)

# MSE = mean normal deviance = mean Tweedie deviance with p = 0
mse(y_num, pred_num)
deviance_normal(y_num, pred_num)
deviance_tweedie(y_num, pred_num, tweedie_p = 0)

# Mean Poisson deviance equals mean Tweedie deviance with parameter 1
deviance_poisson(y_num, pred_num)
deviance_tweedie(y_num, pred_num, tweedie_p = 1)

# Mean Gamma deviance equals mean Tweedie deviance with parameter 2
deviance_gamma(y_num, pred_num)
deviance_tweedie(y_num, pred_num, tweedie_p = 2)

## -----------------------------------------------------------------------------
# The data
y_cat <- iris[["Species"]] == "setosa"
fit_cat <- glm(y_cat ~ Sepal.Length, data = iris, family = binomial())
pred_cat <- predict(fit_cat, type = "response")

# Performance metrics
AUC(y_cat, pred_cat)                 # unweighted
AUC(y_cat, pred_cat, w = weights)    # weighted
logLoss(y_cat, pred_cat)             # Log loss = binary cross-entropy
deviance_bernoulli(y_cat, pred_cat)  # Log Loss * 2

## -----------------------------------------------------------------------------
summary(fit_num)$r.squared

# Same
r_squared(y_num, pred_num)
r_squared(y_num, pred_num, deviance_function = deviance_tweedie, tweedie_p = 0)

## -----------------------------------------------------------------------------
ir <- iris
ir$pred <- predict(fit_num, data = ir)

# Create multiple Tweedie deviance functions
multi_Tweedie <- multi_metric(deviance_tweedie, tweedie_p = c(0, seq(1, 3, by = 0.2)))
perf <- performance(
  ir, 
  actual = "Sepal.Length", 
  predicted = "pred",
  metrics = multi_Tweedie, 
  key = "Tweedie_p", 
  value = "deviance"
)
head(perf)

# Deviance against p
plot(deviance ~ as.numeric(as.character(Tweedie_p)), data = perf, type = "s")

## -----------------------------------------------------------------------------
y <- 1:10
two_models <- cbind(m1 = 1.1 * y, m2 = 1.2 * y)
murphy_diagram(y, two_models, theta = seq(0.9, 1.3, by = 0.01))

