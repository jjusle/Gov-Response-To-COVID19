setwd("C:/Users/Justin/Desktop/ECON 124/Final Paper datasets")

# https://www.openicpsr.org/openicpsr/project/119061/version/V7/view;jsessionid=95551619CD8DE77E69617E68B959F496

install.packages("readstata13")
library(readstata13)

data_set <- read.dta13("Gov_Responses_Covid19_last.dta")
sum(is.na(data_set))
data_set_omitted <- na.omit(data_set)

country_count <- tapply(rep(1,nrow(data_set_omitted)), data_set_omitted$country, sum)
barplot(sort(country_count), main = "Frequency Distribution of Countries", las = 2)

summary(data_set_omitted$school)
sd(data_set_omitted$school)

summary(data_set_omitted$domestic)
sd(data_set_omitted$domestic)

summary(data_set_omitted$travel)
sd(data_set_omitted$travel)

summary(data_set_omitted$travel_dom)
sd(data_set_omitted$travel_dom)

summary(data_set_omitted$curf)
sd(data_set_omitted$curf)

summary(data_set_omitted$mass)
sd(data_set_omitted$mass)

summary(data_set_omitted$sport)
sd(data_set_omitted$sport)

summary(data_set_omitted$rest)
sd(data_set_omitted$rest)

summary(data_set_omitted$testing)
sd(data_set_omitted$testing)

summary(data_set_omitted$masks)
sd(data_set_omitted$masks)

summary(data_set_omitted$state)
sd(data_set_omitted$state)




linear_model <- glm(Rigidity_Public_Health ~ school + domestic + travel + travel_dom +
                      curf + mass + elect + sport + rest + testing + masks + surveillance + state, data = data_set_omitted)

summary(linear_model)
sort(coef(linear_model), decreasing=TRUE)[1:4]

fit <- (1 - (linear_model$deviance/linear_model$null.deviance)) * 100
fit



set.seed(0)

test_indices <- sample(1:nrow(data_set_omitted), size = round(nrow(data_set_omitted) * 0.2))
training_set <- data_set_omitted[-test_indices, ]
test_set <- data_set_omitted[test_indices, ]

ols_model <- glm(Rigidity_Public_Health ~ school + domestic + travel + travel_dom +
                      curf + mass + elect + sport + rest + testing + masks + surveillance + state, data = training_set[ , -1])

summary(ols_model)

IS_r2 <- (1 - (ols_model$deviance / ols_model$null.deviance)) * 100
IS_r2


ols_deviance <- function(y, ols_prediction) {
  return(sum((y - ols_prediction)^2))
  }

OOS_ols_deviance <- ols_deviance(test_set$Rigidity_Public_Health, predict(ols_model, newdata = test_set[ , -1]))
OOS_ols_deviance





linear_model2 <- glm(Rigidity_Public_Health ~ school + school:school_local + domestic + domestic:domestic_local 
                     + travel + travel:travel_partial + travel_dom+ travel_dom:travel_dom_partial 
                     + curf + curf:curf_partial + mass + mass:mass_partial + elect + elect:elect_partial 
                     + sport + sport:sport_partial + rest:rest_local + testing + testing:testing_narrow 
                     + masks + masks:masks_partial + surveillance + surveillance:surveillance_partial 
                     + state + state:state_partial, data = data_set_omitted)

summary(linear_model2)
sort(coef(linear_model2), decreasing=TRUE)[1:4]

fit2 <- (1 - (linear_model2$deviance/linear_model2$null.deviance)) * 100
fit2


ols_model2 <- glm(Rigidity_Public_Health ~ school + school:school_local + domestic + domestic:domestic_local 
                     + travel + travel:travel_partial + travel_dom+ travel_dom:travel_dom_partial 
                     + curf + curf:curf_partial + mass + mass:mass_partial + elect + elect:elect_partial 
                     + sport + sport:sport_partial + rest:rest_local + testing + testing:testing_narrow 
                     + masks + masks:masks_partial + surveillance + surveillance:surveillance_partial 
                     + state + state:state_partial, data = training_set[ , -1])

summary(ols_model2)

IS_r22 <- (1 - (ols_model2$deviance / ols_model2$null.deviance)) * 100
IS_r22

OOS_ols_deviance2 <- ols_deviance(test_set$Rigidity_Public_Health, predict(ols_model2, newdata = test_set[ , -1]))
OOS_ols_deviance2


linear_model3 <- glm(Rigidity_Public_Health ~ domestic + testing + state, data = data_set_omitted)

summary(linear_model3)

fit3 <- (1 - (linear_model3$deviance/linear_model3$null.deviance)) * 100
fit3

linear_model4 <- glm(Rigidity_Public_Health ~ domestic + testing + state + masks + school + travel_dom + sport + rest, data = data_set_omitted)

summary(linear_model4)

sort(coef(linear_model4), decreasing=TRUE)[1:5]

fit4 <- (1 - (linear_model4$deviance/linear_model4$null.deviance)) * 100
fit4

library(gamlr)
training_set <- naref(training_set)
test_set <- naref(test_set)

X <- model.matrix(Rigidity_Public_Health ~ school + domestic + travel + travel_dom +
                    curf + mass + elect + sport + rest + testing + masks + surveillance + state, data = training_set[,-1])[,-1]

lasso_model <- gamlr(X, training_set$Rigidity_Public_Health)
plot(lasso_model, ylab="Estimated Betas From Lasso Model")

cv_lasso_model <- cv.gamlr(X, training_set$Rigidity_Public_Health, nfold = 10)
plot(cv_lasso_model)

best_lambda <- cv_lasso_model$lambda.min
best_lambda

betas_cv <- coef(cv_lasso_model, select = "min")
betas_cv[which(betas_cv != 0), ]
sort(betas_cv[which(betas_cv != 0), ], decreasing=TRUE)




X1 <- model.matrix(Rigidity_Public_Health ~ domestic + testing + state 
                   + masks + school + travel_dom + sport + rest, data = training_set[,-1])[,-1]


lasso_model1 <- gamlr(X1, training_set$Rigidity_Public_Health)
plot(lasso_model1, ylab="Estimated Betas From Lasso Model")

cv_lasso_model1 <- cv.gamlr(X1, training_set$Rigidity_Public_Health, nfold = 10)
plot(cv_lasso_model1)

best_lambda1 <- cv_lasso_model1$lambda.min
best_lambda1

betas_cv1 <- coef(cv_lasso_model1, select = "min")
betas_cv1[which(betas_cv != 0), ]
sort(betas_cv1[which(betas_cv1 != 0), ], decreasing=TRUE)





