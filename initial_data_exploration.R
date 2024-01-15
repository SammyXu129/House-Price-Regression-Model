library(dplyr)
library(ggplot2)
library(readr)
library(janitor)
library(tidyr)
library(tidyverse)
library(caret)

#importing data
rawdata = read_delim("housing-prices-ge19.txt", delim = "\t")
data = janitor::clean_names(rawdata)


#exploring data
anyNA(data) #no na's
names(data)
length(names(data))
count(data)

unique(data$fuel_type)
unique(data$heat_type)
unique(data$sewer_type)

#full model 
data1 = data %>% select(-c("test"))
fullmodel = lm(price ~ ., data = data1)
round(summary(fullmodel)$coef, 3)

drop1(fullmodel, test="F")
fullmodel = update(fullmodel, .~. -sewer_type)

drop1(fullmodel, test="F")
fullmodel = update(fullmodel, .~. -fuel_type)

drop1(fullmodel, test="F")
fullmodel = update(fullmodel, .~. -fireplaces)

drop1(fullmodel, test="F")
fullmodel = update(fullmodel, .~. -pct_college)

drop1(fullmodel, test="F")

#AIC optimised
step.back.aic = step(fullmodel, direction="backward", trace=FALSE)
round(summary(step.back.aic)$coef, 3)

#Creating new model, splitting up heat_type and keeping only relevant variables from above

data2 = data1 %>% select("price", "lot_size", "waterfront", "age", "land_value", "new_construct", "central_air", "heat_type", "living_area", 
                         "bedrooms", "bathrooms", "rooms")
data2 = data2 %>% mutate(
  heat_type_hotwater = if_else(heat_type=="Hot Water", 1, 0),
  heat_type_hotair = if_else(heat_type=="Hot Air", 1, 0),
  heat_type_none = if_else(heat_type=="None", 1, 0)
) %>% select(-c("heat_type"))

fullmodel2 = lm(price ~ ., data = data2)
round(summary(fullmodel2)$coef, 3)
drop1(fullmodel2, test="F")

fullmodel2 = update(fullmodel2, .~. -heat_type_hotwater)
drop1(fullmodel2, test="F")

fullmodel2 = update(fullmodel2, .~. -heat_type_none)
drop1(fullmodel2, test="F")

step.back.aic2 = step(fullmodel2, direction="backward", trace=FALSE)
round(summary(step.back.aic2)$coef, 3)

#Creating Interim/draft final model dataframe and checking assumptions
## 
leading_determinants = data2 %>% select(-c("heat_type_hotwater", "heat_type_none"))
leading_determinants = leading_determinants %>% mutate(residuals = step.back.aic2$residuals, fitted = step.back.aic2$fitted.values)

leading_determinants %>% ggplot() + aes(sample = residuals) + geom_qq(size = 2) + geom_qq_line() +
  labs(x = "Theoretical Normal Quantiles", y = "Actual Residual Quantiles")

qqnorm(leading_determinants$residuals/100000)
qqline(leading_determinants$residuals/100000)

ggplot(leading_determinants, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "loess", se=FALSE)

ggplot(leading_determinants, aes(x = fitted, y = residuals)) +
  xlim(quantile(leading_determinants$price, probs=c(0.05,0.95))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "loess", se=FALSE)

#out-of-sample performance
cv_full = train(
  price ~ living_area + land_value + bathrooms + bedrooms + heat_type_hotair + waterfront + age + new_construct + lot_size + central_air + rooms, leading_determinants,
  method= 'lm',
  trControl = trainControl(
    method='CV',number= 10,
    verboseIter = FALSE
  )
)
cv_full

#in-sample performance
## r-squared
summary(step.back.aic2)$r.squared

## root mean square error 
sqrt(mean((leading_determinants$price-leading_determinants$fitted)^2)) #rmse is quite large for predictions
mean(abs(leading_determinants$price-leading_determinants$fitted)) #more resistant to outliers --> slightly better 
median(leading_determinants$price)