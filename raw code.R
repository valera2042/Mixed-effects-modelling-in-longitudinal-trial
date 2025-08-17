# Longitudional clinical trial using linear mixed-effects models

# install packages if required
requiredPackages <- c("readxl, tidyverse,
                      lubridate, ggplot2, data.table, DescTools, lmerTest,
                      huxtable, pwr")
for (package in requiredPackages) { #Installs packages if not yet installed
  if (!requireNamespace(package, quietly = TRUE))
    install.packages(package)
}

# upload packages
library(readxl)
library(tidyverse) # get for tibble
library(ggplot2)
library(ggplot2)
library(DescTools)
library(lmerTest)
library(huxtable) # printing pretty table
library(lme4)
library(lattice)
library(simr)
# temporarily turn off warnings
options(warn=0)

# Import and data preprocessing

# set working directory
path_ <- "C:/Users/valer/Desktop/R_project/Project 7/data.xlsx"

# loading data containing test and reference datasheets 
loading_data <- function(path_, sheet_) {
  
  # reading file
  data <- read_excel(path = path_, sheet = sheet_)
  return (data)
}

# dataset for the test medicinal product
data_test <- loading_data(path_, "data")

# print pretty table
data_hux_test <- 
  hux(data_test) |>  
  set_bold(row = 1, col = everywhere, value = TRUE) |> 
  set_all_borders(FALSE)
data_hux_test

# Treatment C is a placebo, Treatment B is 10 mg drug dose, Treatment A is 
# 20 mg drug dose

print(xyplot(Blood_pressure~Time|as.factor(Subject),type="l",groups=Treatment,
             strip=strip.custom(bg="white"), lty=c(1,8), lwd=2,
             layout=c(10,4), data_test))

print(xyplot(Blood_pressure~Time|Treatment,type="l",data_test,
             groups=as.factor(Subject),
             strip=strip.custom(bg="white")))

print(bwplot(Blood_pressure~as.factor(Time)|Treatment,data_test, xlab="Time",
             strip=strip.custom(bg="white")))

# From this figure, the obvious trend is revealed; i.e., that on average blood 
# pressure, declines at a faster rate in treatment A than in treatment B, and 
# in treatment B is faster than in treatment C.However the rate and extent of 
# decline varies across the 30 patients.


# Longitudinal Modeling

# Model will have Subject as random effect, while Treatment and Time will be
# considered as fixed effects.

# fit Model 2
mod2DBP = lmer(Blood_pressure~Treatment+Time+(1|Subject), data_test)
summary(mod2DBP)

# Conclusion
# It is observed that Treatment A (dose 20 mg) at initial timepoint equals to 
# 125.7, and this is statistically significant. As the time goes on 
# (-0.77 coefficient) the effect of a drug is increasing while the effects of 
# Treatment B (10 mg) and Treatment C (pacebo) decrease the blood pressure to 
# much lesser extent. Treatment B and Treamtment C both are not statistically 
# significant.


# adding Age and Sex to the model

# fit Model 3 include Age effect
mod3DBP = lmer(Blood_pressure~Treatment+Time+Age+(1|Subject), data_test)
summary(mod3DBP)

anova(mod2DBP, mod3DBP)
# the addition of the Age variable does not statistically significant change 
# the model.

# fit Model 4 including Age and Sex
mod4DBP = lmer(Blood_pressure~Treatment+Time+Age+Sex+(1|Subject), data_test)

# test the Sex effect
anova(mod3DBP, mod4DBP)

# This gives a p-value indicating that Age and Sex are not a statistically
# significant effect. 

