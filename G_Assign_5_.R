setwd("~/Uni/24_WS_Hohenheim/Applied Econometrics/group_assignments/G_assignm5")

#install.packages("ggcorrplot")

# Load Packages ------------------------------------
library(readxl)
library(psych)
library(ggcorrplot)
library(estimatr)
library(dplyr)

library(tidyverse) 
library(haven)
library(ggpubr)
library(AER)
library(gt)
library(corrgram)
library(zoo)
library(tseries)
library(midasr)
library(stargazer)
library(forecast)
library(car)

library(plm)
#install.packages("lmtest")## for vce regression in panel data
library(lmtest)
#install.packages("modelsummary")
library(modelsummary)


# Load Data ---------------------------------------
Lwc <- read_excel("Lwc.xlsx")
range(Lwc$survey_date)
gt(as.Date(Lwc$survey_date))

n_distinct(Lwc$type_employment)
unique(Lwc$type_employment)
unique(Lwc$unavailable_alcohol)

# type_employement (student, wage, civil servant)
# self-quaratnine
# fulltime employement

# self_quaratnine ~ fulltime employement + corona_symptoms + gender



# fixed effects could be: age_group and time 8dummies or not?)
names(Lwc)


x <- Lwc %>% 
  select(  
    corona_symptoms,
    smoker,
    gender,
    age,
    type_employment,
    self_quarantine,
    fulltime_employment
  )

x %>% is.pbalanced()

gt(lwc_cl)
view(Lwc)
nrow(x)
plot.ts(sample(x=x, size=200))
select(x)
?sample()

# which sections and time dependencies sencefull?
# section cold be unique_ID = omits double test of one individum
# time = date_test
nrow(Lwc) - n_distinct(Lwc$unique.ID)

# TASK 1 -----------------------------------------

# LHS = test_corona
# RHS = corona symptoms, smoker, gender, age
x <- Lwc %>% 
  select(
    corona_symptoms,
    smoker,
    gender,
    age
    )


lwc_cl <- na.omit(x) # omitting NA values in variables. 

unique(lwc_cl$gender)
lwc_cl$gender_male <- ifelse(lwc_cl$gender=="Male", 1, 0)
lwc_cl$gender_female <- ifelse(lwc_cl$gender=="Female", 1, 0)
lwc_cl$gender_other<- ifelse(lwc_cl$gender=="Other", 1, 0)
?ifelse()

fm <- glm(corona_symptoms ~ smoker + age + gender_female + gender_other, data = lwc_cl, family = binomial(link = "probit"))
summary(fm)

exp(fm$coefficients) # backtransform values on the response scale

fm <- lm_robust(corona_symptoms ~ smoker + gender_female + gender_male + age, data=lwc_cl, se_type = "stata")
# excluding gender-male to omt coliniearity
?binomial()
pairs.panels(lwc_cl,
  method = "pearson",
  density = T)

summary(fm)

plot(lwc_cl$corona_symptoms ~ lwc_cl$age)


regpro<- glm(smoker ~ smkban + age + age2 + hsdrop + hsgrad + colsome
  + colgrad + black + hispanic + female, 
  family = binomial(link = "probit")
  , data = Smoking)
summary(regpro)

coeftest(regpro, vcov. = vcovHC, type = "HC1")


## Descriptive stat
summary(phillips$inf)
sd <- sd(phillips$inf, na.rm = TRUE)
sd
var <- var(phillips$inf, y = NULL, na.rm = TRUE)
var


## 10.a) #####
## Use the sample covariances below of the dataset "phillips" to estimate the 
# 1st, 2nd, and 3rd autocorrelation (acf) of the quarterly inflation (inf).

covariances <- data.frame(j = 0:3, cov = c(NA, 0.1053, 0.0273, -0.0296))
covariances

## autocorrelations
acf1 <- 0.1053/var
acf1
acf2 <- 0.0273/var
acf2
acf3 <- -0.0296/var
acf3

# or simply use the function below
acf(phillips$inf, lag.max = 3, na.action = na.pass, plot = F)


## 10. b) ####
#to view the plot of acf and pacf
acf(phillips$inf, lag.max = 40, na.action = na.pass, plot = T)

## Using Augmented Dickey-Fuller test to test for stationarity in the time series (adf test does not work with NAs present)
phillips2 <- na.aggregate(phillips)
adf.test(phillips2$inf)



