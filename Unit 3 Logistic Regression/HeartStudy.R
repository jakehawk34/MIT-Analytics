# Unit 3
# Framingham Heart Study

# In this lecture, we'll be using analytical models to prevent heart disease.

# We'll be predicting the 10-year risk of coronary heart disease or CHD.

framingham = read.csv("framingham.csv")
str(framingham)

set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)

train = subset(framingham, split == TRUE)
test = subset(framingham, split == FALSE)

# You typically want to put somewhere between 50% and 80% of the data in the training set.

# Build the logistic regression model using the training set and all of the independent variables in the dataset
framinghamLog = glm(TenYearCHD ~ ., data = train, family=binomial)
summary(framinghamLog)
# male, age, prevalentStroke, totChol, sysBP, and glucose are all significant and have positive coefficients

# Use this model to make predictions on the test set
predictTest = predict(framinghamLog, type="response", newdata = test)

# Use a threshold value of 0.5 to create a confusion matrix
table(test$TenYearCHD, predictTest > 0.5)

# Total accuracy of the model
(1069 + 11) / (1069 + 6 + 187 + 11)

# We need to compare this to the accuracy of a simple baseline method.
# The more frequent outcome in this case is 0, so the baseline method would always predict 0 or no CHD.
(1069 + 6) / (1069 + 6 + 187 + 11)

summary(train)

# Compute the out of sample AUC with ROCR and the testing data set
# The first line creates a data frame of TenYearCHD (from the test set) and predictTest. Then, use na.omit to get rid of NAs.
# The second just feeds the data from df to ROCR's prediction function similar to the original code.
# The third line is the original code.
df <- na.omit(data.frame(cbind(TenYearCHD = test$TenYearCHD, predictTest)))
ROCRpred = prediction(df$predictTest, df$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)




