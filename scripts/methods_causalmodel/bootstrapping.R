# Bootstrapping following PS matching

boot_fun <- function(data, i) {
  boot_data <- data[i,]
  
  #Do 1:1 PS matching with replacement
  m <- matchit(A ~ latitude + longitude + MG.f + prev.crop.type,
               data = boot_data,
               replace = TRUE)
  
  #Extract matched dataset
  md <- match.data(m, data = boot_data)
  
  #Fit outcome model
  fit <- lm(Y ~ A + latitude + longitude + MG.f + prev.crop.type,
             data = md, weights = weights)
  
  ## G-computation ##
  
  #Estimated potential outcomes under treatment
  p1 <- predict(fit, type = "response",
                newdata = transform(md, A = 1))
  Ep1 <- weighted.mean(p1, md$weights)
  
  #Estimated potential outcomes under control
  p0 <- predict(fit, type = "response",
                newdata = transform(md, A = 0))
  Ep0 <- weighted.mean(p0, md$weights)
  
  # Difference
  return(Ep1 - Ep0)
}

library("boot")
set.seed(54321)
boot_out <- boot(data_cleaned, boot_fun, R = 999)

boot_out

boot.ci(boot_out, type = "perc")

##-----------
cl <- makePSOCKcluster(5)  # 6 cores on the 2018 desktop, 16 on the 2023 HP desktop
clusterExport(cl, c('data_cleaned', 'ps.formula'))
clusterEvalQ(cl, {library("stats"); library("MatchIt")})

tic()
set.seed(54321)
# Using more bootstraps allows you to use Bca confidence intervals 
boot_out <- boot(data_cleaned, boot_fun, R = 4999, parallel = "snow", ncpus = 5, cl = cl)
toc() # 394.07 sec ~ 6.5 min on the 2018 desktop

stopCluster(cl)


# bias-corrected accelerated (BCa) bootstrap confidence interval (this takes some time):
tic()
bca <- boot.ci(boot_out, type = "bca")  # -22.68, 119.18 (95% CI)
toc()  # 77.15 sec on 2018 desktop

#------------------------------------------------------------------------------>
## the result was
# bca
# BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
# Based on 4999 bootstrap replicates
# 
# CALL : 
#   boot.ci(boot.out = boot_out, type = "bca")
# 
# Intervals : 
#   Level       BCa          
# 95%   ( 0.308,  4.006 )  
# Calculations and Intervals on Original Scale
# Some BCa intervals may be unstable

# sb = standard bootstrap
fit1.sb <-
  tibble(method = "Matching - linear model (no inter) - std bootstrap SE",
         tau = fit.avg$estimate,
         lci = bca$bca[4],
         uci = bca$bca[5])

