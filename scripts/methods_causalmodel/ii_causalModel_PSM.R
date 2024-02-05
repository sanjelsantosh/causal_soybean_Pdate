# causal model ======> Propensity score matching (PSM) ========================>

library(MatchIt)
library(tableone)
set.seed(123)

load("data/obsData.RData")
# remove NA's in the data
obsData_cleaned <- na.omit(obsData)

# prepare the baseline vars with all the covariates
baselinevars <- names(dplyr::select(obsData,
                                    !c(yield, PD_yday, P_Rdoy, diff_pd_rpd, Y, A)))

# fullmodel formula 
lm.formula <- as.formula(paste("Y~ A +", 
                                paste(baselinevars, 
                                      collapse = "+")))
print(lm.formula)

# propensity score formula
ps.formula <- as.formula(paste("A~", 
                               paste(baselinevars, collapse = "+")))
#print the PS formula
print(ps.formula)

# # Fit PS value
# PS.fit <- glm(ps.formula,family="binomial", 
#               data=obsData_cleaned)
# 
# #calculate PS score
# obsData_cleaned$PS <- predict(PS.fit, 
#                               newdata = obsData_cleaned, type="response") 
# 
# 
# logitPS <-  -log(1/obsData_cleaned$PS - 1)  

# Full match 
match.obj.full <- matchit(ps.formula, 
                          data = obsData_cleaned,
                          method = "full",
                          estimand = "ATE")

m.data <- match.data(match.obj.full, distance = "ps")


#Accessing balance after matching --------------------->>>>>>>>>>>>>>>>>>>>>>>>>
plot(match.obj.full, type = "jitter", interactive = FALSE)

# balance plot
require(cobalt)
cobalt::bal.plot(match.obj.full,  
         var.name = "distance", 
         which = "both", 
         type = "histogram",  
         mirror = TRUE)

cobalt::bal.tab(match.obj.full, un = TRUE, 
        thresholds = c(m = .1))

cobalt::love.plot(match.obj.full, binary = "std", 
          thresholds = c(m = .1))

#----------------------------  Propensity score fitting   ---------------------|
## Model fitting
fit <- lm(Y ~ A * splines::ns(qlogis(ps), 5),
          data = m.data, weights = weights)

library(marginaleffects)
g.comp <- comparisons(fit, newdata = subset(m.data, A == 1),
                      variables = "A", vcov = ~subclass)
summary(g.comp)
#______________________________________________________________________________ 

# Treatment effect with weight
fit.PSMmatched <- lm(full.model.formula,
                     data = m.data,
                     weights = weights) 

# PSM, Average treatment effect and CI
library(Publish)
adj.fit <- Publish::publish(fit.PSMmatched, digits=1)$regressionTable[2,]
adj.fit

# bootstrapping for 






