# causal inference P.Date -> yield

library(tidyverse)
library(tableone)
soydf_Pdate <- read_csv("data/soydf_Pdate.csv")

#change the letter date to dayOfYear
# Convert the character dates to Date objects
soydf_Pdate$P_Date <- as.Date(soydf_Pdate$P_Date, format = "%d-%b")

# Add a new column "P_Rdoy" containing day of year values
soydf_Pdate$P_Rdoy <- as.integer(format(soydf_Pdate$P_Date, "%j"))

##-->
#calculate the diff between the P_date and P_Rdoy
soydf_Pdate$diff_pd_rpd <- as.integer(soydf_Pdate$PD_yday - soydf_Pdate$P_Rdoy)
##

##---- visualize -- data exploration recommended Planting date -----------------

## Histogram  of diff_pd variable
# Remove missing values from the column
non_missing_values <- na.omit(soydf_Pdate$diff_pd_rpd)
# Calculate the recommended number of bins using Freedman-Diaconis rule
num_bins <- nclass.FD(non_missing_values)
print(num_bins)
##
ggplot(
  data = soydf_Pdate,
  mapping = aes(x = diff_pd_rpd)
) +
  geom_histogram(bins = num_bins) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 10, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = -10, linetype = "dashed", color = "blue") +
  annotate("text", x = 0, y = -25, label = "0", vjust = -0.5, color = "red", size = 4) +
  annotate("text", x = 10, y = -25, label = "10", vjust = -0.5, color = "blue", size = 4) +
  annotate("text", x = -10, y = -25, label = "-10", vjust = -0.5, color = "blue", size = 4) +
  labs(
    x = "observed planting date - recommended planting date",
    y = "frequncy"
  )
  

# In the histogram, region between the blue lines represent data for potential outcome 1

# PD_doy vs yield color pd_diff
soydf_Pdate |> 
  mutate(pd_diff_class = cut(soydf_Pdate$diff_pd_rpd, # add manaul classes to visualize
                             breaks = c(-Inf, -10, 10, 50, Inf),
                             labels = c("-22 to -10", "-10 to 10", "10 to 50", "above 50"),
                             right =  FALSE)) |> 
  ggplot(
    mapping = aes(x = PD_yday, y = yield, color = pd_diff_class)
  ) +
  geom_point() +
  labs(
    x = "Planting date (day of year)",
    y = "Yield",
    color = "PD - rPD"
  ) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme_minimal()


##-------------------Stratified by PD recommendation----------------------------
#                        Data exploration  

obsData <- dplyr::select(soydf_Pdate, c(latitude, longitude, yield, MG.f, PD_yday,  
                                        P_Rdoy, diff_pd_rpd, previous.crop))
obsData$Y <- obsData$yield
factors <- c("MG.f", "previous.crop")
obsData[factors] <- lapply(obsData[factors], as.factor)

# convert the treatment A(pd on time vs pd not on time) 
# change the treatment assignment based on P.Date decision 
# T = 1 if Planted on RPD (within ± 10 Recommended PD) = 1
# T= 0 if planted before or after recommended date

#--->
obsData$A <- ifelse((obsData$PD_yday - obsData$P_Rdoy) >= -10 & 
                      (obsData$PD_yday - obsData$P_Rdoy)<= 10, 1, 0)
# Frequency table
table(obsData$A)

# Explore the data distribution stratified by PDate treatment
ggplot(
  data = obsData,
  mapping = aes(x = factor(A))
) +
  geom_bar() +
  labs(
    title = "Frequency distribution-Planting date decision",
    x = "Recommended PD (N=0, Y=1) ",
    y = "Frequency") +
  theme_classic()

# histograms of stratified PD_recommendation 
obsData |> 
  filter(!is.na(A)) |> 
  ggplot(
    mapping = aes(x=diff_pd_rpd, color =factor(A), fill = factor(A))
  ) +
  geom_histogram(alpha=0.4) +
  facet_wrap(~factor(A)) +
  labs(title = "Distribution by planting date decision: 0=not-optimal, 1=Optimal",
       x = "PDate - Recommended PDate")

# Frequency table
table(obsData$A)

# Summary table of the stratified data
#library(tableone)
tab1 <- CreateTableOne(vars = c("Y"),
                       data = obsData,
                       strata = "A",
                       test = FALSE
)
print(tab1, showAllLevels = FALSE)

# visualize the yield difference stratified by planting decision
obsData |> 
  filter(!is.na(A)) |> 
  ggplot(
    mapping = aes( x = PD_yday, y = yield, color = factor(A))
  ) +
  geom_point() +
  geom_boxplot(alpha = 0.7) +
  geom_text(x = 126.25, y = 63, label = "62.54(15.97) \n Optimal planting \n (A1), n = 580", color = "black") +
  geom_text(x = 158, y = 57, label = "57.39(13.34) \n Non-optimal planting \n (A0), n = 4580", color = "black") +
  labs(
    title = "A1 - A0 = 5.15",
    x = "Planting date (day of year)"
  )

#--------------- wrangling: variable:- Previous.crop -------------------------------
#Use table function to get counts
crop_table <- table(obsData$previous.crop)

# Use prop.table to get percentage of total
crop_percentage <- prop.table(crop_table) * 100

# Convert table to data frame
crop_data <- data.frame(
  crop = names(crop_percentage),
  count = as.numeric(crop_table),
  percentage = as.numeric(crop_percentage)
)

# Create the bar plot using ggplot
ggplot(crop_data, aes(y = crop, x = percentage)) +
  geom_bar(stat = "identity") +
  labs(title = "Crop Distribution", y = "Percentage") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1, size = 4) +
  geom_text(aes(label = count), hjust = 2, size = 4)


# relabeling the previous crop ------------------------------------------------|

obsData$prev.crop.type <- ifelse(obsData$previous.crop %in% c("oats","sorghum", "barley", "rye","popcorn"), "other.cereals", 
                                 ifelse (obsData$previous.crop == "corn", "corn", 
                                         ifelse (obsData$previous.crop == "soybean", "soybean", 
                                                 ifelse(obsData$previous.crop == "wheat", "wheat",
                                                        "others"))))
# If there are NA values in previous.crop, you may want to handle them separately
obsData$prev.crop.type[is.na(obsData$previous.crop)] <- "unknown"

# Display unique values in the new variable
unique(obsData$prev.crop.type)

ggplot(
  data = obsData,
  mapping = aes(x = after_stat(count), y = prev.crop.type, fill = prev.crop.type)
) +
  geom_bar(stat = "count") +
  geom_text(aes(label = after_stat(count)), stat = "count", hjust = -0.2) +
  theme_bw()


#<<<<<<<<<<<<<<<<<<< Method 1: Regression Analysis >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# 1.Crude regression
# Adjust the treatment var: planting date decision
fit0 <- lm(Y~A, obsData)
summary(fit0)

# 2.Adjust the treatment + covariates

# prepare the baseline vars with all the covariates
baselinevars <- names(dplyr::select(obsData,
                                            !c(yield, PD_yday, P_Rdoy, diff_pd_rpd, Y, A)))

# adjusted formula
out.formula <- as.formula(paste("Y~ A +", 
                                paste(baselinevars, 
                                      collapse = "+")))
print(out.formula)
fit1 <- lm(out.formula, data = obsData)
summary(fit1)

summary_fit1 <- summary(fit1); summary_fit1$coefficients["A", ]

library(Publish)
adj.fit <- publish(fit1, digits=1)$regressionTable[2,]
adj.fit

# just extract the coefficient for A


#Check the residuals
par(mfrow = c(2, 2))
plot(fit1)


#<<<<<<<<<<<<<<< Method 2: Propensity score matching(PSM) >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
library(MatchIt)
set.seed(123)

# remove NA's in the data
obsData_cleaned <- na.omit(obsData)

# propensity score formula
ps.formula <- as.formula(paste("A~", 
                               paste(baselinevars, collapse = "+")))

ps.formula
# fitting logistic regression model -> likelihood of treatment based on covariates
PS.fit <- glm(ps.formula,family="binomial", 
              data=obsData_cleaned)

# calculating propensity scores
obsData_cleaned$PS <- predict(PS.fit, 
                      newdata = obsData_cleaned, type="response") 

summary(obsData_cleaned$PS)

# converting PS score to log odds
logitPS <-  -log(1/obsData_cleaned$PS - 1)  

match.obj <- matchit(ps.formula, data = obsData_cleaned,
                     distance = obsData_cleaned$PS,
                     method = "nearest", replace=FALSE,
                     ratio = 1,
                     caliper = .2*sd(logitPS))

# propensity score adjustment --> access the matching  
require(cobalt)
bal.plot(match.obj,  
         var.name = "distance", 
         which = "both", 
         type = "histogram",  
         mirror = TRUE)

bal.tab(match.obj, un = TRUE, 
        thresholds = c(m = .1))

love.plot(match.obj, binary = "std", 
          thresholds = c(m = .1))

cat("The love plot suggests satisfactory propensity score matching (all SMD < 0.1)\n SMD is a statistical measure used to assess the balance betwn the treatment and control group")

# PSM result
matched.data <- match.data(match.obj)   
tab1y <- CreateTableOne(vars = c("Y"),
                        data = matched.data, strata = "A", 
                        test = TRUE)
print(tab1y, showAllLevels = FALSE, 
      test = TRUE)

# Fitting a generalized linear model using matched data
fit.PSMmatched <- glm(Y~A,
                   family=gaussian,  
                   data = matched.data) 

#Average treatment effect
summary(fit.PSMmatched)

publish(fit.PSMmatched)


# PSM method produced almost similar ATE as adjusted linear regression = 3.3


#<<<<<<<<<<<<<<<<<<<<<< Method 3: G-computation >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# use the covariate adjusted linear regression output
# check the formula
fit1$terms

# Predicted outcome for optimal planting (A =1)
obsData$Pred.Y1 <- predict(fit1, 
                           newdata = data.frame(A = 1, 
                                                dplyr::select(obsData, !A)), 
                           type = "response")

mean(obsData$Pred.Y1, na.rm = TRUE)
sum(is.na(obsData$Pred.Y1))

# histogram
pY1 <- obsData  |> 
  ggplot(mapping = aes(x = Pred.Y1)) +
  geom_histogram(color = "black", fill = "lightgrey") +
  geom_vline(xintercept = mean(obsData$Pred.Y1, na.rm = TRUE), 
             color = "blue", linewidth = 1.5) +
  labs(
    x = "Y(A=1)",
    title = "Histogram for predicted outcome for optimal planting (A =1)"
  )
pY1

# predicted outcome for non-optimal planting (A=0)
obsData$Pred.Y0 <- predict(fit1, 
                           newdata = data.frame(A = 0, 
                                                dplyr::select(obsData, !A)), 
                           type = "response")
mean(obsData$Pred.Y0, na.rm = TRUE)
  
# histogram
pY0 <- obsData  |> 
  ggplot(mapping = aes(x = Pred.Y0)) +
  geom_histogram(color = "black", fill = "lightgrey") +
  geom_vline(xintercept = mean(obsData$Pred.Y0, na.rm = TRUE), 
             color = "blue", size = 1.5) +
  labs(
    x = "Y(A=1)",
    title = "Histogram for predicted outcome for non-optimal planting (A = 0)"
  )

#plot both histogramns together
library(gridExtra)
grid.arrange(pY1, pY0, nrow = 2)

# Subtract the mean of these two outcome predictions to get treatment effect estimate
obsData$Pred.TE <- obsData$Pred.Y1 - obsData$Pred.Y0

# The mean 
mean(obsData$Pred.TE, na.rm=TRUE)
summary(obsData$Pred.TE) # all 3.3 is it normal?


# Histogram of the TE
obsData  |> 
  ggplot(mapping = aes(x = Pred.TE)) +
  geom_histogram(bin =10, color = "black", fill = "lightgrey") +
  geom_vline(xintercept = mean(obsData$Pred.TE, na.rm = TRUE), 
             color = "blue", size = 1.5) +
  labs(
    x = "Y(A=1)-Y(A=0)",
    title = "Histogram for predicted treatment effect"
  )

# SD with boosting
require(boot)
gcomp.boot <- function(formula = out.formula, data = obsData, indices) {
  boot_sample <- data[indices, ]
  fit.boot <- lm(formula, data = boot_sample)
  Pred.Y1 <- predict(fit.boot, 
                     newdata = data.frame(A = 1, 
                                          dplyr::select(boot_sample, !A)), 
                     type = "response")
  Pred.Y0 <- predict(fit.boot, 
                     newdata = data.frame(A = 0, 
                                          dplyr::select(boot_sample, !A)), 
                     type = "response")
  Pred.TE <- mean(Pred.Y1) - mean(Pred.Y0)
  return(Pred.TE)
}

set.seed(123)
gcomp.res <- boot(data=obsData, 
                  statistic=gcomp.boot,
                  R=250, 
                  formula=out.formula)

#CI based on Normality assumption
CI1 <- boot.ci(gcomp.res, type="norm") 
CI1



#<<<<<<<<<<< Inverse probability Weighting (IPW) or 
#                Inverse probability of treatment weighting (IPTW) >>>>>>>>>>>>>

# remove NA's in the data
obsData_cleaned <- obsData |> 
  na.omit() |> 
  subset(select = -c(Pred.Y1, Pred.Y0, Pred.TE))


#======== Step 1: exposure modelling
# prepare the baseline vars with all the covariates
baselinevars <- names(dplyr::select(obsData_cleaned,
                                    !c(yield, PD_yday, P_Rdoy, diff_pd_rpd, Y, A)))

# propensity score formula
ps.formula <- as.formula(paste("A~", 
                               paste(baselinevars, collapse = "+")))
PS.fit <- glm(ps.formula,family="binomial", 
              data=obsData_cleaned)

#summary(PS.fit)

obsData_cleaned$PS <- predict(PS.fit, type="response")
summary(obsData_cleaned$PS)

tapply(obsData_cleaned$PS, obsData_cleaned$A, summary)


plot(density(obsData_cleaned$PS[obsData_cleaned$A==0]), 
     col = "red", main = "")
lines(density(obsData_cleaned$PS[obsData_cleaned$A==1]), 
      col = "blue", lty = 2)
legend("topright", c("Non-optimal","Optimal"), 
       col = c("red", "blue"), lty=1:2)



#============ Step 2: Convert PS to IPW

obsData_cleaned$IPW <- obsData_cleaned$A/obsData_cleaned$PS + 
  (1-obsData_cleaned$A)/(1-obsData_cleaned$PS)
summary(obsData_cleaned$IPW)
# inverse of Propensity score is used to assign the weight to each observation.
# IPW effectively re-weights the observations to create a pseudo-population 
# in which the treated and control groups are balanced with respect to 
# observed covariates

require(WeightIt)

W.out <- weightit(ps.formula, 
                  data = obsData_cleaned, 
                  estimand = "ATE",
                  method = "ps")
summary(W.out$weights)

#============ Step 3: Balance checking

require(cobalt)
bal.tab(W.out, un = TRUE, 
        thresholds = c(m = .1))

# bal.plot(W.out,  
#          var.name = "covs", 
#          which = "both", 
#          type = "histogram",  
#          mirror = TRUE)

require(cobalt)
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

# ==================
