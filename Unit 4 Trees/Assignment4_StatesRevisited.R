# Assignment 4
# State data Revisited

data(state)
statedata = data.frame(state.x77)
str(statedata)

# Build a linear regression model to predict Life.Exp
linModel = lm(Life.Exp ~ ., data = statedata)
summary(linModel)
linPredict = predict(linModel)
SSE = sum((statedata$Life.Exp - linPredict)^2)
SSE

# Build a second linear regression model using just Population, Murder, Frost, and HS.Grad as independent variables
bestReg = lm(Life.Exp ~ Population + Murder + Frost + HS.Grad, data = statedata)
summary(bestReg)
bestPredict = predict(bestReg)
bestSSE = sum((statedata$Life.Exp - bestPredict)^2)
bestSSE
sum(bestReg$residuals^2)

# Build a CART model to predict Life.Exp using all of the other variables as independent variables
CARTmodel = rpart(Life.Exp ~ ., data = statedata)
prp(CARTmodel)
CARTpredict = predict(CARTmodel)
SSE1 = sum((statedata$Life.Exp - CARTpredict)^2)

# Build another CART model with minbucket set to 5
CARTmodel2 = rpart(Life.Exp ~ ., data = statedata, minbucket=5)
prp(CARTmodel2)
CARTpredict2 = predict(CARTmodel2)
SSE2 = sum((statedata$Life.Exp - CARTpredict2)^2)

# Build another CART model predicting Life.Exp with only Area and minbucket = 1
CARTmodel3 = rpart(Life.Exp ~ Area, data = statedata, minbucket=1)
prp(CARTmodel3)
CARTpredict3 = predict(CARTmodel3)
SSE3 = sum((statedata$Life.Exp - CARTpredict3)^2)

# Cross-validation
set.seed(111)
tr.control = trainControl(method = "cv", number = 10)
cartGrid = expand.grid( .cp = seq(0.01,0.5,0.01))
tr = train(Life.Exp ~ ., data = statedata, method="rpart", trControl=tr.control, tuneGrid=cartGrid)
tr

# Create a tree with the cp value found during cross-validation
CARTmodel4 = rpart(Life.Exp ~ ., data = statedata, cp = 0.12)
prp(CARTmodel4)
CARTpredict4 = predict(CARTmodel4)
SSE4 = sum((statedata$Life.Exp - CARTpredict4)^2)

# The purpose of cross-validation is to pick the tree that will perform the best on a test set. 
# So we would expect the model we made with the "best" cp to perform best on a test set.

# Use train with the same parameters as before but just using Area as an independent variable to find the best cp value (set the seed to 111 first)
set.seed(111)
train(Life.Exp ~ Area, data=statedata, method="rpart", trControl = tr.control, tuneGrid = cartGrid )
CARTmodel5 = rpart(Life.Exp ~ Area, data=statedata, cp=0.03)
prp(CARTmodel5)
CARTpredict5 = predict(CARTmodel5)
SSE5 = sum((statedata$Life.Exp - CARTpredict)^2)

# The original Area tree was overfitting the data - it was uninterpretable. 
# Area is not as useful as Murder - if it was, it would have been in the cross-validated tree. 
# Cross-validation is not designed to improve the fit on the training data, but it won't necessarily make it worse either. 
# Cross-validation cannot guarantee improving the SSE on unseen data, although it often helps.
