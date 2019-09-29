# Clay Ford
# 1/2/19


# Budget/Expenditure Modeling

# Purpose

# develop a model that can better predict library budget needs. There are
# multiple dimensions of this: (1) to identify what external changes generate
# additional costs for the library, and generate an estimate of the effect, (2)
# build a model that accurately forecasts budget needs

# ARL Data-Variables

# use Total Expenditures (TOTEXP) and Total Expenditures for Materials (EXPLM)
# as the key outcomes. 


# Independent/predictor variables:

# University Data: Total full-time student enrollment (TOTSTU), Total graduate
# student enrollment (GRADSTU), Ph.D. s awarded (PHDAWD), Ph.D. fields (PHDFLD),
# Instructional faculty (FAC), University Type (TYPE), Law library included
# (LAW), Medical library included (MED); Not listed in codebook, but available
# in data file: Total part time students (TOTPT), Part-time graduate students
# (GRADPT)
#
# Supplemental Data: Masters degrees awarded (MSDA), government grants and
# contracts (GGACPE)
#
# Public Services Data: Total lending (ILLTOT), Total borrowing (ILBTOT),
# Initial circulations (INITCIRC), Reference transactions (REFTRANS)

# UPDATE JAN 10, 2019: also include Total Electronic Circulations (TTEC) and
# Investment Return per FTE enrollment (IRPFE)


# stepwise variable selection via AIC -------------------------------------


library(MASS)
library(effects)
library(bootStepAIC)

# load workspace
load("arl.Rdata")

# Choose a model by AIC in a Stepwise Algorithm
arl_totexp <- arl_totexp %>% select(-c(phdawd, totpt, gradpt, ilbtot, reftrans, inrt, irpfe, irpgg))
arl_explm <- arl_explm %>% select(-c(phdawd, totpt, gradpt, ilbtot, reftrans, inrt, irpfe, irpgg))

# Model totexp
mod_totexp <- lm(totexp/1e6 ~ ., data = arl_totexp)
step_totexp <- step(mod_totexp, trace = 0)
summary(step_totexp)
par(mfrow=c(2,2))
plot(step_totexp, id.n = 5)
summary(influence.measures(step_totexp))
# arl_totexp["SUNY-ALBANY",]

# Model explm
mod_explm <- lm(explm/1e6 ~ ., data = arl_explm)
step_explm <- step(mod_explm, trace = 0)
summary(step_explm)
plot(step_explm, id.n = 5)
summary(influence.measures(step_explm))
# arl_explm["SUNY-ALBANY",]


# robust regression for non-constant variance -----------------------------

# robust regression
# iterated re-weighted least squares 
rlm_totexp <- rlm(formula(step_totexp), data = arl_totexp)
summary(rlm_totexp)
plot(rlm_totexp, id.n=5)

rlm_explm <- rlm(formula(step_explm), data = arl_explm)
summary(rlm_explm)
plot(rlm_explm, id.n = 5)

# compare models
AIC(step_totexp, rlm_totexp)
AIC(step_explm, rlm_explm)

# the lm model seems to do as well as the robust model


# Model predictions for UVa -----------------------------------------------


# make prediction for uva
predict(mod_totexp, newdata = uva_totexp)
uva_totexp[["totexp"]]/1e6
predict(mod_explm, newdata = uva_explm)
uva_explm[["explm"]]/1e6



# Investigate variability of model ----------------------------------------


# Implement a Bootstrap procedure to investigate the variability of model
# selection under the step() stepwise algorithm 

boot_totexp <- boot.stepAIC(mod_totexp, data = arl_totexp, B = 999)
boot_explm <- boot.stepAIC(mod_explm, data = arl_explm, B = 999)

# TOTEXP

# percentage of times each variable was selected
boot_totexp$Covariates

# percentage of times regression coefficient of each variable had sign + and -
boot_totexp$Sign

#  percentage of times the regression coefficient of each variable was
#  significant under the alpha significance level
boot_totexp$Significance

# EXPLM

# percentage of times each variable was selected
boot_explm$Covariates

# percentage of times regression coefficient of each variable had sign + and -
boot_explm$Sign

#  percentage of times the regression coefficient of each variable was
#  significant under the alpha significance level
boot_explm$Significance

