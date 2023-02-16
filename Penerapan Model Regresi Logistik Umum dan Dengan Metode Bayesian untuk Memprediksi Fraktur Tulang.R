#2440062123 - Vika Valencia Susanto

getwd()
housing <- read.csv(file = "cal_housing.csv")
head(housing)
summary(housing)

set.seed(42)
dataset <- housing[sample(nrow(housing), 500), ]
head(dataset)
summary(dataset)
sum(is.na(dataset))
dataset <- na.omit(dataset)

datas2 <- dataset
summary(datas2)

#mengubah ocean proximity menjadi sebuah faktor
dataset$ocean_proximity = factor(dataset$ocean_proximity,levels = 
                                   c('<1H OCEAN', 'INLAND', 
                                     'NEAR BAY', 'NEAR OCEAN'),
                                 labels = c(1, 2, 3, 4))
summary(dataset)
datas <- dataset

#model dengan longitude dan latitude
model_ols <- lm(median_house_value~housing_median_age +
                  total_rooms+ total_bedrooms + population + households + 
                  median_income + ocean_proximity, data = datas)
summary(model_ols)


library(rstanarm)
model_bayes <- stan_glm(median_house_value~housing_median_age+
                          total_rooms+ total_bedrooms + population + households + 
                          median_income + ocean_proximity, data = datas, prior = 
                          normal(), prior_intercept = normal())
summary(model_bayes)
#variabel independen bisa dibilang berpengaruh secara signifikan terhadap variabel 
#dependen jika dalam posterior/credible intervalnya tidak mencakup nilai 0
posterior_interval(model_bayes)
#yang tidak mempengaruhi expenses secara signifikan adalah age, rooms, bedrooms, population, household


datas$ocean_proximity = as.character(datas$ocean_proximity)
datas$ocean_proximity = as.array(datas$ocean_proximity)

summary(datas)
View(datas)
c2 <- c('2','4')
datas$ocean_proximity[!(datas$ocean_proximity %in% c2)] <- 'others'
datas$ocean_proximity = factor(datas$ocean_proximity,levels = 
                                   c('2', '4', 'others'),
                                 labels = c(2, 4, '1&3'))

#model dengan longitude dan latitude
model_ols1 <- lm(median_house_value~housing_median_age +
                  total_rooms+ total_bedrooms + population + households + 
                  median_income + ocean_proximity, data = datas)
summary(model_ols1)


#backward elimination
model_ols2 <- lm(median_house_value~.-total_bedrooms-longitude
                 -latitude, data = datas)
summary(model_ols2)

model_ols3 <- lm(median_house_value~.-total_bedrooms-longitude
                 -latitude-total_rooms, data = datas)
summary(model_ols3)

model_bayes1 <- stan_glm(median_house_value~housing_median_age+
                          total_rooms+ total_bedrooms + population + households + 
                          median_income + ocean_proximity, data = datas, prior = 
                          normal(), prior_intercept = normal())
summary(model_bayes1)
#variabel independen bisa dibilang berpengaruh secara signifikan terhadap variabel 
#dependen jika dalam posterior/credible intervalnya tidak mencakup nilai 0
posterior_interval(model_bayes1)

model_bayes2 <- stan_glm(median_house_value~.-total_bedrooms-longitude
                          -latitude, data = datas, prior = 
                            normal(), prior_intercept = normal())
posterior_interval(model_bayes2)

model_bayes3 <- stan_glm(median_house_value~.-total_bedrooms-longitude
                         -latitude-total_rooms, data = datas, prior = 
                           normal(), prior_intercept = normal())
posterior_interval(model_bayes3)


#model dengan county
#mengubah longitude dan latitude ke city
library(maps)
startm <- Sys.time()
county<-map.where(database="county", 
                  dataset$longitude, dataset$latitude)
endm <- Sys.time()
county1 <- as.array(county)
county2 <- as.data.frame(county)
dataset$county <- county1
summary(dataset)

summary(county2)
c <- c('california,los angeles','california,orange', 'california,san diego',
       'california,alameda', 'california,san bernardino')
dataset$county[!(dataset$county %in% c)] <- 'others'

dataset$county = factor(dataset$county,levels = 
                          c('california,los angeles','california,orange', 'california,san diego',
                            'california,alameda', 'california,san bernardino', 'others'),
                        labels = c(1, 2, 3, 4, 5, 6))
summary(dataset)

model_ols_county <- lm(median_house_value~housing_median_age+ county+
                   total_rooms+ total_bedrooms + population + households + 
                   median_income + ocean_proximity, data = dataset)
summary(model_ols_county)


model_bayes_county <- stan_glm(median_house_value~housing_median_age+ county+
                          total_rooms+ total_bedrooms + population + households + 
                          median_income + ocean_proximity, data = dataset, 
                         prior = normal(), prior_intercept = normal())
summary(model_bayes_county)
posterior_interval(model_bayes_county)

dataset$ocean_proximity = as.character(dataset$ocean_proximity)
dataset$ocean_proximity = as.array(dataset$ocean_proximity)

summary(dataset)
View(dataset)
c2 <- c('2','4')
dataset$ocean_proximity[!(dataset$ocean_proximity %in% c2)] <- 'others'
dataset$ocean_proximity = factor(dataset$ocean_proximity,levels = 
                                 c('2', '4', 'others'),
                               labels = c(2, 4, '1&3'))

dataset$county = as.character(dataset$county)
dataset$county = as.array(dataset$county)
summary(dataset)
View(dataset)
c3 <- c('2', '3')
dataset$county[!(dataset$county %in% c3)] <- 'others'
dataset$county = factor(dataset$county,levels = 
                                   c('2', '3', 'others'),
                                 labels = c(2, 3, '1,4,5,6'))

model_ols_county2 <- lm(median_house_value~housing_median_age+ county+
                         total_rooms+ total_bedrooms + population + households + 
                         median_income + ocean_proximity, data = dataset)
summary(model_ols_county2)

model_bayes_county2 <- stan_glm(median_house_value~housing_median_age+ county+
                                  total_rooms+ total_bedrooms + population + households + 
                                  median_income + ocean_proximity, data = dataset, 
                                prior = normal(), prior_intercept = normal())
summary(model_bayes_county2)
posterior_interval(model_bayes_county2)

dataset$county = as.character(dataset$county)
dataset$county = as.array(dataset$county)
summary(dataset)
View(dataset)
c4 <- c('1,4,5,6')
dataset$county[!(dataset$county %in% c4)] <- 'others'
dataset$county = factor(dataset$county,levels = 
                          c('1,4,5,6', 'others'),
                        labels = c('1,4,5,6', '2&3'))


model_ols_county3 <- lm(median_house_value~housing_median_age+ county+
                          total_rooms+ total_bedrooms + population + households + 
                          median_income + ocean_proximity, data = dataset)
summary(model_ols_county3)

model_ols_county3_1 <- lm(median_house_value~.-longitude-latitude-
                            total_bedrooms, data = dataset)
summary(model_ols_county3_1)

model_ols_county3_2 <- lm(median_house_value~.-longitude-latitude-
                            total_bedrooms-total_rooms, data = dataset)
summary(model_ols_county3_2)

model_bayes_county3 <- stan_glm(median_house_value~housing_median_age+ county+
                                  total_rooms+ total_bedrooms + population + households + 
                                  median_income + ocean_proximity, data = dataset, 
                                prior = normal(), prior_intercept = normal())
summary(model_bayes_county3)
posterior_interval(model_bayes_county3)

model_bayes_county3_1 <- stan_glm(median_house_value~.-longitude-latitude-
                                    total_bedrooms, data = dataset, 
                                prior = normal(), prior_intercept = normal())
posterior_interval(model_bayes_county3_1)

model_bayes_county3_2 <- stan_glm(median_house_value~.-longitude-latitude-
                                    total_bedrooms-total_rooms, data = dataset, 
                                  prior = normal(), prior_intercept = normal())
posterior_interval(model_bayes_county3_2)

model_ols3$coefficients
model_bayes3$coefficients
model_ols_county3_2$coefficients
model_bayes_county3_2$coefficients

library(Metrics)
ypredols = predict(model_ols3, datas)
ypredbayes = predict(model_bayes3, datas)
rmse(datas$median_house_value, ypredols)
rmse(datas$median_house_value, ypredbayes)
mse(datas$median_house_value, ypredols)
mse(datas$median_house_value, ypredbayes)
smape(datas$median_house_value, ypredols)
smape(datas$median_house_value, ypredbayes)
mape(datas$median_house_value, ypredbayes)
mape(datas$median_house_value, ypredols)

ypredolscounty = predict(model_ols_county3_2, dataset)
ypredbayescounty = predict(model_bayes_county3_2, dataset)
rmse(dataset$median_house_value, ypredolscounty)
rmse(dataset$median_house_value, ypredbayescounty)
mse(dataset$median_house_value, ypredolscounty)
mse(dataset$median_house_value, ypredbayescounty)
smape(dataset$median_house_value, ypredolscounty)
smape(dataset$median_house_value, ypredbayescounty)
mape(dataset$median_house_value, ypredolscounty)
mape(dataset$median_house_value, ypredbayescounty)

