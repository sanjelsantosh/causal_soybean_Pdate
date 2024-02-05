# caual inference ===== Inverse probability weighting of Treatment (IPWT) or
#                          Inverse probability wighting (IPW) ==================

load("data/pdate_y.RData")
# remove NA's in the data
obsData_cleaned <- na.omit(obsData)


#======== Step 1: exposure modelling
# prepare the baseline vars with all the covariates
baselinevars <- names(dplyr::select(obsData,
                                    !c(yield, PD_yday, P_Rdoy, diff_pd_rpd, Y, A)))

# propensity score formula
ps.formula <- as.formula(paste("A~", 
                               paste(baselinevars, collapse = "+")))

# fit logistic regression to estimate propensity scores
PS.fit <- glm(ps.formula,family="binomial", 
              data=obsData_cleaned)

summary(PS.fit)

obsData_cleaned$PS <- predict(PS.fit, type="response")
summary(obsData_cleaned$PS)

tapply(obsData_cleaned$PS, obsData_cleaned$A, summary)


plot(density(obsData_cleaned$PS[obsData_cleaned$A==0]), 
     col = "red", main = "Propensity score")
lines(density(obsData_cleaned$PS[obsData_cleaned$A==1]), 
      col = "blue", lty = 2)
legend("topright", c("Non-optimal","Optimal"), 
       col = c("red", "blue"), lty=1:2)



#============ Step 2: Convert PS to IPW

obsData_cleaned$IPW <- obsData_cleaned$A/obsData_cleaned$PS + (1-obsData_cleaned$A)/(1-obsData_cleaned$PS)
summary(obsData_cleaned$IPW)

require(WeightIt)

# Generate balancing weights for treatments
W.out <- weightit(ps.formula, 
                  data = obsData_cleaned, 
                  estimand = "ATE",
                  method = "ps")
summary(W.out$weights)
summary(W.out)

#============ Step 3: Balance checking

require(cobalt)

bal.tab(W.out, un = TRUE, 
        thresholds = c(m = .1))

love.plot(W.out, binary = "std",
          thresholds = c(m = .1),
          abs = TRUE, 
          var.order = "unadjusted", 
          line = TRUE)



# ================Step 4: outcome modelling

out.formula <- as.formula(Y ~ A)
out.fit <- glm(out.formula,
               data = obsData_cleaned,
               weights = IPW)
summary(out.fit)

cat(" IPW method resulted in the least value of treatment effect = 1.63")
