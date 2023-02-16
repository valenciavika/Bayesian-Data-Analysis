getwd()
data <- read.csv(file = "bmd.csv", header = TRUE)
summary(data)

sum(is.na(data))

data$bmi <- data$weight_kg / (data$height_cm/100)^2
summary(data)
modelLogistik <- glm(fracture=="fracture"~age + sex + medication + bmi + bmd, 
                     data = data, family = "binomial")
summary(modelLogistik)

set.seed(42)
library(rstanarm)
modelBayes <- stan_glm(fracture=="fracture"~age + sex + medication + bmi + bmd, 
                       data = data, family = "binomial", prior = normal(), 
                       prior_intercept = normal())
summary(modelBayes)
modelBayes$coefficients

posterior_interval(modelBayes)

data$medication <- as.character(data$medication)
data$medication <- as.array(data$medication)
c <- ("No medication")
data$medication[!(data$medication %in% c)] <- 'med'
data$medication <- as.factor(data$medication)
summary(data)

modelLogistik2 <- glm(fracture=="fracture"~age + sex + medication + bmi + bmd, 
                     data = data, family = "binomial")
summary(modelLogistik2)

modelBayes2 <- stan_glm(fracture=="fracture"~age + sex + medication + bmi + bmd, 
                       data = data, family = "binomial", prior = normal(), 
                       prior_intercept = normal())
summary(modelBayes2)
modelBayes2$coefficients

posterior_interval(modelBayes2)

#backward elimination 1
modelLogistik3 <- glm(fracture=="fracture"~age + sex + medication + bmd, 
                      data = data, family = "binomial")
summary(modelLogistik3)

modelBayes3 <- stan_glm(fracture=="fracture"~age + sex + medication + bmd, 
                        data = data, family = "binomial", prior = normal(), 
                        prior_intercept = normal())
posterior_interval(modelBayes3)

#backward elimination 2
modelLogistik4 <- glm(fracture=="fracture"~sex + medication + bmd, 
                      data = data, family = "binomial")
summary(modelLogistik4)

modelBayes4 <- stan_glm(fracture=="fracture"~sex + medication + bmd, 
                        data = data, family = "binomial", prior = normal(), 
                        prior_intercept = normal())
summary(modelBayes4)
modelBayes4$coefficients
posterior_interval(modelBayes4)

library(lmtest)
lrtest(modelLogistik)


library(ResourceSelection)
aktual <- ifelse(data$fracture=="no fracture",0,1)
hoslem.test(aktual, fitted(modelLogistik4))
hoslem.test(aktual, fitted(modelBayes4))




yhatBayes <- fitted(modelBayes4)
head(yhatBayes)
predictBayess <- as.factor(ifelse(round(yhatBayes)==1, "fracture", "no fracture"))
head(predictBayess)

yhatBiner <- fitted(modelLogistik4)
predictBiner <- as.factor(ifelse(round(yhatBiner)==1, "fracture", "no fracture"))
head(predictBiner)

library(caret)
confusionMatrix(data$fracture, predictBayess)
confusionMatrix(data$fracture, predictBiner)

aktuall <- as.factor(aktual)
library(Metrics)
rmse(as.numeric(aktuall), yhatBayes)
rmse(as.numeric(aktuall), yhatBiner)
mape(as.numeric(aktuall), yhatBayes)
mape(as.numeric(aktuall), yhatBiner)

