# Unit 4
# Keeping an Eye on Healthcare Costs: The D2Hawkeye Story

# This is a story of D2Hawkeye, a medical data mining company located in Waltham, Massachusetts.

# It starts with medical claims that consist of diagnoses, procedures, and drugs.
# These medical claims are then processed via process of aggregation, cleaning, and normalization.
# This data then enters secure databases on which predictive models are applied.
# The output of predictive models are specific reports that give insight to the various questions that D2Hawkeye aspires to answer.

# D2Hawkeye had many different types of clients. The most important were third party administrators of medical claims.

# To analyze the data, the company used what we call a pre-analytics approach.
# This was based on the human judgment of physicians who manually analyze patient histories and developed medical rules.
# Of course, this involved human judgment, utilized a limited set of data, it was often costly, and somewhat inefficient.

# The key question we analyze in this lecture is "Can we use analytics instead?"

# Health care industry is data-rich, but data may be hard to access.
# Claims data, requests for reimbursement submitted to insurance companies or state-provided insurance
# by doctors, hospitals and pharmacies.
# Eligibility information and demographic information are also good sources.
# Claims data is rich, structured, high dimension. However, this collection of data does not
# capture all aspects of a person's treatment or health. Many things must be inferred.
# Observation period from 2001-2003. Results period in 2004. Patients with at least 10 months of data in each period were included.

# 13,000 diagnoses --> 217 diagnosis groups
# 22,000 procedures --> 213 procedure groups
# 45,000 prescription drugs --> 189 therapeutic groups
# In addition to the defined groups, we also defined in collaboration with medical doctors, 269 medically-defined rules.
# Interactions between illnesses, interactions between illness and age, noncompliance treatment, and illness severity.
# Five buckets defined with 20% of all costs in each bucket.
# Partitions were from 0 to $3,000, $3,000 to $8,000, $8,000 to $19,000, $19,000 to $55,0000, and >$55,000
# 78% of all patients were in the bucket from 0 to $3,000
# Bucket 1 was low risk, Bucket 2 is emerging risk, Bucket 3 is moderate risk, Bucket 4 is high risk, and Bucket 5 is very high risk.

# Error Measures
# In D2Hawkeye, failing to classify a high risk patient was much more costly than failing to classify a low risk patient correctly.
# To account for this, a penalty error used asymmetric penalties in penalty matrix.
# Baseline: predict that the cost in the next period will be the same as the cost in the current period.
# Baseline had an accuracy of 75% and a penalty error of 0.56.

# Multi-class classification
# Costs were the most important for determining splits in the beginning of the tree.
# So let us give some examples of bucket five.
# 1. The patient is under 35 years old, he has between 3,300 and 3,900 in claims, coronary artery disease as a diagnosis, but no office visits in the last year.
# 2. Claims between $3,900 and $43,000 with at least $8,000 paid in the last 12 months, $4,300 in pharmacy claims, and acute cost profile and cancer diagnosis.
# 3. More than $58,000 in claims, but at least $50,000 paid in the last 12 months, but not an acute profile.

# The observations represent a 1% random sample of Medicare beneficiaries, limited to those still alive at the end of 2008.
# Our independent variables are from 2008, and we will be predicting cost in 2009.
Claims = read.csv("ClaimsData.csv", stringsAsFactors = TRUE)
str(Claims)

table(Claims$bucket2009) / nrow(Claims)

library(caTools)
set.seed(88)
spl = sample.split(Claims$bucket2009, SplitRatio = 0.6)
ClaimsTrain = subset(Claims, spl == TRUE)
ClaimsTest = subset(Claims, spl == FALSE)

# Quick Question 6
mean(ClaimsTrain$age)
mean(ClaimsTrain$diabetes)

# Baseline method
table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)
# Accuracy is sum of the diagonal divided by all observations
(110138 + 10721 + 2774 + 1539 + 104) / nrow(ClaimsTest)

# Penalty error and penalty matrix; actual outcomes on the left, predicted outcomes on top.
PenaltyMatrix = matrix(c(0, 1, 2, 3, 4,
                         2, 0, 1, 2, 3,
                         4, 2, 0, 1, 2,
                         6, 4, 2, 0, 1,
                         8, 6, 4, 2, 0), byrow = TRUE, nrow = 5)
PenaltyMatrix

# Compute the penalty error of the baseline method by multiplying the penalty matrix by the classification matrix
as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)) * PenaltyMatrix

sum(as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)) * PenaltyMatrix) / nrow(ClaimsTest)
# Our goal will be to create a CART model that has an accuracy higher than 68% and a penalty error lower than 0.74.

# Quick Question 7
table(ClaimsTest$bucket2009) / nrow(ClaimsTest)

PenaltyMatrix[,1] * table(ClaimsTest$bucket2009)
sum(PenaltyMatrix[,1] * table(ClaimsTest$bucket2009)) / nrow(ClaimsTest)

library(rpart)
library(rpart.plot)

# Build the CART model
ClaimsTree = rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=ClaimsTrain, method="class", cp=0.00005)
prp(ClaimsTree)

# Make predictions on the test set
PredictTest = predict(ClaimsTree, newdata=ClaimsTest, type="class")

table(ClaimsTest$bucket2009, PredictTest)

# Accuracy
(114141 + 16102 + 118 + 201 + 0) / nrow(ClaimsTest)

# Penalty error
as.matrix(table(ClaimsTest$bucket2009, PredictTest)) * PenaltyMatrix
sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest)) * PenaltyMatrix) / nrow(ClaimsTest)
# So while we increased the accuracy, the penalty error also went up. Why?
# By default, rpart will try to maximize the overall accuracy, and every type of error is seen as having a penalty of one.
# Our CART model predicts 3, 4, and 5 so rarely because there are very few observations in these classes.

# Add loss to the CART model and calculate new accuracy, penalty error values
ClaimsTree = rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=ClaimsTrain, method="class", cp=0.00005, parms=list(loss=PenaltyMatrix))
PredictTest = predict(ClaimsTree, newdata=ClaimsTest, type="class")
table(ClaimsTest$bucket2009, PredictTest)
(94310 + 18942 + 4692 + 636 + 2) / nrow(ClaimsTest)
as.matrix(table(ClaimsTest$bucket2009, PredictTest)) * PenaltyMatrix
sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest)) * PenaltyMatrix) / nrow(ClaimsTest)
# Our accuracy (0.647) is now lower than the baseline model, but so is the penalty error (0.642)

# According to the penalty matrix, some of the worst types of errors are to predict bucket 1 when the actual cost bucket is higher. 
# Therefore, the model with the penalty matrix predicted bucket 1 less frequently than the baseline model.


# CONCLUSIONS
# So we first observe that the overall accuracy of the method regarding the percentage that it accurately
# predicts is 80%, compared to 75% of the baseline. But notice that this is done in an interesting way.
# For bucket one patients, the two models are equivalent. But of course this suggests the idea
# that healthy people stay healthy, which is the idea of the baseline model. The cost repeats is valid in the data.
# But then for buckets two to five, notice that the accuracy increases substantially from 31% to 60%-- it doubles-- from 21% to 53%--
# more than doubles-- and from 19% to 39%-- doubles. There's an improvement from 23% to 30%, not as big as before,
# but there is indeed an improvement for bucket five. But notice the improvement on the penalty from 0.56 to 0.52 overall.
# A small improvement in bucket one, but a significant improvement as we increase on the buckets. For example, here for bucket five,
# the penalty error decreases from 1.88 to 1.01, a substantial improvement.

