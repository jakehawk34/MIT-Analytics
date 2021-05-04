# Final Exam
# Predicting Bank Telemarketing Success

bank = read.csv("bank.csv")

boxplot(bank$duration ~ bank$job)
sort(tapply(bank$duration, bank$job, mean))

# Examine the correlation between the following variables
cor(bank$emp.var.rate, bank$cons.price.idx, bank$cons.conf.idx)


