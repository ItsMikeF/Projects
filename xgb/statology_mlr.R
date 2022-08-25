library(GGally)

#Multiple Linear Regression

data <- mtcars[, c("mpg", "disp", "hp", "drat")]
pairs(data, pch = 18 , col = "steelblue")
ggpairs(data)

model <- lm(mpg ~ disp + hp + drat, data = data)
hist(residuals(model), col = "steelblue")

#create fitted value vs residual plot
plot(fitted(model), residuals(model))

#add horizontal line at 0
abline(h = 0, lty = 2)

summary(model)

intercept <- coef(summary(model))["(Intercept)", "Estimate"]
disp <- coef(summary(model))["disp", "Estimate"]
hp <- coef(summary(model))["hp", "Estimate"]
drat <- coef(summary(model))["drat", "Estimate"]

#use the model coefficients to predict the value for mpg
intercept + disp*220 + hp*150 + drat*3
