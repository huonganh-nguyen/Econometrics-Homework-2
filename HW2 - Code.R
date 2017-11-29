library(foreign)

############# Question 2 #############
mydata1 <- read.dta("ajr.dta")
summary(mydata1)
head(mydata1)

# run OLS regression of loggdp on risk
r1 <- lm(loggdp ~ risk, data = mydata1)
summary(r1)

# estimate the first-stage equation on risk using logmort0
r2.first.stage <- lm(risk ~ logmort0, data = mydata1)
summary(r2.first.stage)

# estimate the reduced-form equation on loggdp using logmort
r2.reduced <- lm(loggdp ~ logmort0, data = mydata1)
summary(r2.reduced)

# run the instrumental variable regression of loggdp on risk using logmort0 as an instrument
# use r2.first.stage to find risk.iv (estimated risk using logmort0 as an instrument)
mydata1$risk.iv <- predict(r2.first.stage, newdata = mydata1)
# use risk.iv to run second stage regression
r2.iv <- lm(loggdp ~ risk.iv, data = mydata1)
summary(r2.iv)

# run the instrumental variable regression of loggdp on risk using logmort0 as an instrument
r2.ivreg <- ivreg(loggdp ~ risk | logmort0, data = mydata1)
summary(r2.ivreg)

# r2.iv and r2.ivreg give the same results

# construct the instrumental variables estimate (beta_1)
# beta_1 = coefficient in the reduced form regression / coefficient in the 1st stage regression
beta_1 <- r2.reduced$coefficients[2] / r2.first.stage$coefficients[2]
beta_1

# run the instrumental variable regression of loggdp on risk using malaria as an instrument
r3.ivreg <- ivreg(loggdp ~ risk | malaria, data = mydata1)
summary(r3.ivreg)

# run a regression with risk, malaria, lattitude, rainmin, and meantemp
r4 <- lm(loggdp ~ risk + malaria + abs(latitude) + rainmin + meantemp, data = mydata1)
summary(r4)

############# Question 3 #############
mydata2 <- read.dta("Earnings_and_Height.dta")
head(mydata2)

# create 4 new indicator variables for education
mydata2$LT_HS <- ifelse(mydata2$educ < 12, 1, 0)
mydata2$HS <- ifelse(mydata2$educ == 12, 1, 0)
mydata2$Some_Col <- ifelse(mydata2$educ > 12 & mydata2$educ < 16, 1, 0)
mydata2$College <- ifelse(mydata2$educ >= 16, 1, 0)
# check if indicator variables for education were successfully added to df
head(mydata2)
summary(mydata2)

# create subset of data for women and men
mydata2.women <- mydata2[mydata2$sex == "0:female", ]
mydata2.men <- mydata2[mydata2$sex == "1:male", ]

# run a regression of earnings on height among women
r4.women.short <- lm(earnings ~ height, data = mydata2.women)
summary(r4.women.short)

# run a regression of earnings on height among women, including LT_HS, HS, and Some_Col as control variables
r4.women.long <- lm(earnings ~ height + LT_HS + HS + Some_Col, data = mydata2.women)
summary(r4.women.long)

# omitted variable bias among women
r4.women.short$coefficients[2] - r4.women.long$coefficients[2]

# test the joint null hypothesis that the coeff on education variables are zero in r4.women.long model
library(car)
linearHypothesis(r4.women.long, c("LT_HS = 0", "HS = 0", "Some_Col = 0"), vcov. = hccm)

# run a regression of earnings on height among men
r4.men.short <- lm(earnings ~ height, data = mydata2.men)
summary(r4.men.short)

# run a regression of earnings on height among men, including LT_HS, HS, and Some_Col as control variables
r4.men.long <- lm(earnings ~ height + LT_HS + HS + Some_Col, data = mydata2.men)
summary(r4.men.long)

# omitted variable bias among men
r4.men.short$coefficients[2] - r4.men.long$coefficients[2]

# test the joint null hypothesis that the coeff on education variables are zero in r4.women.long model
library(car)
linearHypothesis(r4.men.long, c("LT_HS = 0", "HS = 0", "Some_Col = 0"), vcov. = hccm)

############# Question 5 #############
library(stargazer)
load("wage1.RData") # automatically saved to a data frame called "data"
summary(data)

# run OLS regression
r5 <- lm(lwage ~ educ + exper + expersq, data = data)
stargazer(r5, type = "text")

# how many people have more than 41 years of experience?
nrow(data[data$exper > 41, ])

############# Question 6 #############
mydata4 <- read.dta("fertility.dta")
summary(mydata4)

# run regression of weeksworked on morekids
r6 <- lm(weeksm1 ~ morekids, data = mydata4)
summary(r6)

# run regression of morekids on samesex
r6.first.stage <- lm(morekids ~ samesex, data = mydata4)
summary(r6.first.stage)

# run regression of weeksworked on morekids using samesex as an instrument
r6.iv <- ivreg(weeksm1 ~ morekids | samesex, data = mydata4)
summary(r6.iv)

# include agem1, black, hispan, and othrace in r6
r6.long <- lm(weeksm1 ~ morekids + agem1 + black + hispan + othrace, data = mydata4)
summary(r6.long)
