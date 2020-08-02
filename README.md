# incomeCollege
We analyze very briefly the dependency between income and major college
## We load the needed libraries together with the data
library(dplyr)
library(collegeIncome)
data(college)
head(college)

## We convert the variable "major_category" into a factor one
college <- mutate(college,major_category = as.factor(major_category))

## We plot the income vs. "major_category"
plot(p25th~major_category,data = college, xlab="",las=2)
plot(median~major_category,data = college, xlab="",las=2)
plot(p75th~major_category,data = college, xlab="",las=2)

## We propose a model of income variation with the variable "major_category"
fit <- lm(median~major_category, data = college)
summary(fit)

## We propose the same model with no intercept (the p-values are incredibly low):
fit2 <- lm(median~major_category-1, data = college)
summary(fit2)

## We relevel the variable "major_category" so as to put "Business" first
major_category3 <- relevel(college$major_category, "Business")
fit3 <- lm(median~major_category3, data = college)
summary(fit3)
## The above results are better than first, but worse than second.


## We propose the same model but with no intercept with "Business" first
major_category4 <- relevel(college$major_category, "Business")
fit4 <- lm(median~major_category4-1, data = college)
summary(fit4)
## The results are the same as the one of the second model.

####################################################################
par(mfrow=c(2,2))
plot(fit)

par(mfrow=c(2,2))
plot(fit2)

par(mfrow=c(2,2))
plot(fit3)
