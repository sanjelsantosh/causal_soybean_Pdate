# Causal inference with == Regression analysis==

#Naive estimate of treatment effect using minimum sufficient adjustment set

load("data/obsData.RData")
# 1.Crude regression

# Adjust the treatment var: planting date decision
fit0 <- lm(Y~A, obsData)
summary(fit0)

# 2.Naive the treatment + covariates

# prepare the baseline vars with all the covariates
baselinevars <- names(dplyr::select(obsData,
                                    !c(yield, PD_yday, P_Rdoy, diff_pd_rpd, Y, A)))

# adjusted formula
out.formula <- as.formula(paste("Y~ A +", 
                                paste(baselinevars, 
                                      collapse = "+")))
print(out.formula)
fit1 <- lm(out.formula, data = obsData)

# just extract the coefficient for A/Average treatment effect (ATE)
summary_fit1 <- summary(fit1); summary_fit1$coefficients["A", ]


library(Publish)
adj.fit <- publish(fit1, digits=1)$regressionTable[2,]
adj.fit

#Check the residuals
par(mfrow = c(2, 2))
plot(fit1)
