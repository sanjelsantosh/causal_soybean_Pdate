# this file is to try weightIT package
# R version 4.3.2

library(WeightIt)

#loading the data
load("data/obsData.RData")
data <- obsData
rm(obsData)

#clean data, remove NA
data_cleaned <- na.omit(data)
head(data_cleaned)

# weightIt
W.out <- weightit(A ~ latitude + longitude + MG.f + prev.crop.type,
                  data = data_cleaned, estimand = "ATT", method = "glm")
W.out

summary(W.out)

#
library(cobalt)
bal.tab(A ~ latitude + longitude + MG.f + prev.crop.type,
        data = data_cleaned, estimand = "ATT", thresholds = c(m = .05))

bal.tab(W.out, stats = c("m", "v"), thresholds = c(m = .05))

# Attach weights to dataset
data_cleaned$weights <- W.out$weights

# Fit outcome model
fit <- lm(Y ~ A + latitude + longitude + MG.f + prev.crop.type,
          data = data_cleaned, weights = weights)

# G-computation for the treatment effect
library("marginaleffects")
avg_comparisons(fit, variables = "A",
                vcov = "HC3",
                newdata = subset(data_cleaned, A == 1),
                wts = "weights")
plot(summary(W.out))


require(cobalt)
love.plot(W.out, binary = "std",
          thresholds = c(m = .1),
          abs = TRUE, 
          var.order = "unadjusted", 
          line = TRUE)

plot(W.out1, type = "density")
plot(density(data_cleaned$weights[data_cleaned$A==0]), 
     col = "red", main = "")
     