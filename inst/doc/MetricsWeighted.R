## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  fig.width = 5.5,
  fig.height = 4.5
)

## ----setup--------------------------------------------------------------------
library(MetricsWeighted)

## -----------------------------------------------------------------------------
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
medae(y_num, pred_num, w = weights) # median absolute error

# Normal deviance equals Tweedie deviance with parameter 0
deviance_normal(y_num, pred_num)
deviance_tweedie(y_num, pred_num, tweedie_p = 0)
deviance_tweedie(y_num, pred_num, tweedie_p = -0.001)

# Poisson deviance equals Tweedie deviance with parameter 1
deviance_poisson(y_num, pred_num)
deviance_tweedie(y_num, pred_num, tweedie_p = 1)
deviance_tweedie(y_num, pred_num, tweedie_p = 1.01)

# Gamma deviance equals Tweedie deviance with parameter 2
deviance_gamma(y_num, pred_num)
deviance_tweedie(y_num, pred_num, tweedie_p = 2)
deviance_tweedie(y_num, pred_num, tweedie_p = 1.99)
deviance_tweedie(y_num, pred_num, tweedie_p = 2.01)

## -----------------------------------------------------------------------------
# The data
y_cat <- iris[["Species"]] == "setosa"
fit_cat <- glm(y_cat ~ Sepal.Length, data = iris, family = binomial())
pred_cat <- predict(fit_cat, type = "response")

# Performance metrics
AUC(y_cat, pred_cat)  # unweighted
AUC(y_cat, pred_cat, w = weights)  # weighted
logLoss(y_cat, pred_cat)  # Logloss
deviance_bernoulli(y_cat, pred_cat)  # LogLoss * 2

## -----------------------------------------------------------------------------
summary(fit_num)$r.squared

# same
r_squared(y_num, pred_num)
r_squared(y_num, pred_num, deviance_function = deviance_tweedie, tweedie_p = 0)
r_squared(y_num, pred_num, deviance_function = deviance_tweedie, tweedie_p = 1.5)

# weighted
r_squared(y_num, pred_num, w = weights)
r_squared(y_num, pred_num, w = weights, deviance_function = deviance_gamma) 
r_squared(
  y_num, pred_num, w = weights, deviance_function = deviance_tweedie, tweedie_p = 2
)
r_squared(y_num, pred_num, deviance_function = deviance_tweedie, tweedie_p = 1.5)

# respect to 'own' deviance formula
myTweedie <- function(actual, predicted, w = NULL, ...) {
  deviance_tweedie(actual, predicted, w, tweedie_p = 1.5, ...)
}
r_squared(y_num, pred_num, deviance_function = myTweedie)

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
  key = "Tweedie p", 
  value = "deviance"
)
perf$`Tweedie p` <- as.numeric(as.character(perf$`Tweedie p`))
head(perf)

# Deviance vs p
plot(deviance ~ `Tweedie p`, data = perf, type = "s")

# Same for Pseudo-R-Squared regarding Tweedie deviance
multi_Tweedie_r2 <- multi_metric(
  r_squared, deviance_function = deviance_tweedie, tweedie_p = c(0, seq(1, 3, by = 0.2))
)
perf <- performance(
  ir, 
  actual = "Sepal.Length", 
  predicted = "pred", 
  metrics = multi_Tweedie_r2, 
  key = "Tweedie p", 
  value = "R-squared"
)
perf$`Tweedie p` <- as.numeric(as.character(perf$`Tweedie p`))

# Values vs. p
plot(`R-squared` ~ `Tweedie p`, data = perf, type = "s")

## -----------------------------------------------------------------------------
y <- 1:10
two_models <- cbind(m1 = 1.1 * y, m2 = 1.2 * y)
murphy_diagram(y, two_models, theta = seq(0.9, 1.3, by = 0.01))

