library(olsrr)
library(dplyr)

data <- read.csv("dataset2.csv")

data[, 4] <- as.factor(data[, 4])
data[, 6] <- as.factor(data[, 6])
data[, 7] <- as.factor(data[, 7])
data[, 11] <- as.factor(data[, 11])
data = data[, -2] # This variable has only one factor

model <- glm(Total.Number.of.Family.members ~ ., data = data,
             family = binomial(link = "logit"))
summary(model)

### Fitting all possible models

# 10 total variables, need a vector of length 10

encode <- function(x) {
  results <- c()
  for(i in 1:10) {
    results[i] <- ifelse(x%%2 == 1, TRUE, FALSE)
    x <- floor(x/2)
  }
  return (results)
}

min.aic = model$aic
which.model = 0
for (i in 33:1023) { 
  # 32 contains single variable: Total.Number and will not be used, 
  # Number of all possible models is 2^10
  code <- encode(i)
  if (code[6] == FALSE){ # Total.Number is not chosen, we skip
    next
  }
  print(i)
  this.aic <- glm(Total.Number.of.Family.members ~ ., data = data[, code], family = binomial(link = "logit"))$aic
  if (min.aic > this.aic) {
    min.aic <- this.aic
    which.model <- code
  }
}
print(min.aic)
print(which.model)
print(colnames(data[, which.model]))

model <- glm(Total.Number.of.Family.members ~ ., data = data[, which.model],
             family = binomial(link = "logit"))
summary(model)

fit.data <- data %>% 
  mutate(Total.Number.Predict = fitted(model))

plot(fit.data$Total.Number.of.Family.members, fit.data$Total.Number.Predict, 
     xlab = "True", ylab = "Predict")

