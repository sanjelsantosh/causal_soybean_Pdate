# causal model ======== G computation with linear regressioin ==================

load("data/pdate_y.RData")

# prepare the baseline vars with all the covariates
baselinevars <- names(dplyr::select(obsData,
                                    !c(yield, PD_yday, P_Rdoy, diff_pd_rpd, Y, A)))

# adjusted formula - muliple linear regression
out.formula <- as.formula(paste("Y~ A +", 
                                paste(baselinevars, 
                                      collapse = "+")))
print(out.formula)

fit1 <- lm(out.formula, data = obsData)

# Predicted outcome for optimal planting (A =1)
obsData$Pred.Y1 <- predict(fit1, 
                           newdata = data.frame(A = 1, 
                                                dplyr::select(obsData, !A)), 
                           type = "response")

mean(obsData$Pred.Y1, na.rm = TRUE)
sum(is.na(obsData$Pred.Y1))

# histogram
obsData  |> 
  ggplot(mapping = aes(x = Pred.Y1)) +
  geom_histogram(color = "black", fill = "lightgrey") +
  geom_vline(xintercept = mean(obsData$Pred.Y1, na.rm = TRUE), 
             color = "blue", linewidth = 1.5) +
  labs(
    x = "Y(A=1)",
    title = "Histogram for predicted outcome for optimal planting (A =1)"
  )


# predicted outcome for non-optimal planting (A=0)
obsData$Pred.Y0 <- predict(fit1, 
                           newdata = data.frame(A = 0, 
                                                dplyr::select(obsData, !A)), 
                           type = "response")

# histogram
obsData  |> 
  ggplot(mapping = aes(x = Pred.Y0)) +
  geom_histogram(color = "black", fill = "lightgrey") +
  geom_vline(xintercept = mean(obsData$Pred.Y0, na.rm = TRUE), 
             color = "blue", linewidth = 1.5) +
  labs(
    x = "Y(A=1)",
    title = "Histogram for predicted outcome for non-optimal planting (A = 0)"
  )

# Subtract the mean of these two outcome predictions to get treatment effect estimate
obsData$Pred.TE <- obsData$Pred.Y1 - obsData$Pred.Y0

# The mean 
mean(obsData$Pred.TE, na.rm=TRUE)
summary(obsData$Pred.TE)


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
