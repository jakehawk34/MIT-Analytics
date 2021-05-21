# Final Exam
# Predicting Bank Telemarketing Success

bank = read.csv("bank.csv")
str(bank)

boxplot(bank$duration ~ bank$job)
sort(tapply(bank$duration, bank$job, mean))

# Examine the correlation between the following variables
cor(select(bank, emp.var.rate, cons.price.idx, cons.conf.idx, euribor3m, nr.employed))

# Splitting into a Training and Testing Set
set.seed(201)

library(caTools)

spl = sample.split(bank$y, 0.7)

training = subset(bank, spl == TRUE)
testing = subset(bank, spl ==  FALSE)

# Training a Logistic Regression Model
mod = glm(y ~ . - duration - euribor3m - nr.employed, data = training, family="binomial")
summary(mod)

# When the month is March, the odds of subscribing to the product are 261.8% higher than an otherwise identical contact.

# What is the meaning of the AUC?

# The proportion of the time the model can differentiate between a randomly selected client who subscribed to a term deposit 
# and a randomly selected client who did not subscribe 
