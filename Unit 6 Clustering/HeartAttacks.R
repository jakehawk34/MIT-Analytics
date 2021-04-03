# Unit 6: Clustering
# Predictive Diagnosis: Discovering Patterns for Disease Detection

# Heart Attacks

# A 2012 report from the American Heart Association estimates about 715,000 Americans have a heart attack every year.
# To put this number into perspective, this means that every 20 seconds, a person has a heart attack in the United States.
# It is also equivalent of September the 11th repeating itself every 24 hours, 365 days a year.

# The nature of heart attacks, however, makes it hard to predict, prevent, and even diagnose.
# Here are some statistics.
# 25% of heart attacks are silent.
# 47% of sudden cardiac deaths occur outside hospitals, suggesting that many patients do not act on early warning signs.
# Only 27% percent of respondents to a 2005 survey recognized the symptoms and called 911 for help.

# How can analytics help? The key to helping patients is to understand the clinical characteristics of patients
# in whom heart attacks was missed.
# We need to better understand the patterns in a patient's diagnostic history that link to heart attack
# and to predicting whether a patient is at risk for a heart attack.

# Quick Question 1

# Logistic Regression, CART, and Random Forest are all designed to be used to predict whether or not someone has a heart attack, 
# since this is a classification problem.

# The Data

# We'll use health insurance claims filed for about 7,000 members from January 2000 until November 2007.
# We concentrated on members with the following attributes. At least five claims with coronary artery disease
# diagnosis, at least five claims with hypertension diagnostic codes, at least 100 total medical claims,
# at least five pharmacy claims, and data from at least five years.
# These selections yield patients with a high risk of heart attack, and a reasonably rich medical history with continuous coverage.

# The resulting data sets includes about 20 million health insurance entries, including individual, medical, and pharmaceutical records.
# Diagnosis, procedures, and drug codes in the data set comprised tens of thousands of attributes.
# The codes were aggregated into groups. 218 diagnosis groups, 180 procedure groups, 538 drug groups.
# 46 diagnosis groups were considered by clinicians as possible risk factors for heart attacks.

# The target prediction variable is the occurrence of a heart attack.
# We define this from a combination of several claims. Namely, diagnosis of a heart attack, alongside a trip to the emergency room,
# followed by subsequent hospitalization.
# Only considering heart attack diagnosis that are associated with a visit to the emergency room,
# and following hospitalization helps ensure that the target outcome is in fact a heart attack event.

# There were 147 variables.
# Variable one is the patient's identification number, and variable two is the patient's gender.
# There were variables related to the diagnoses group counts nine, six, and three months before the heart attack target period.
# There were variables related to the total cost nine, six, and three months before the heart attack target period, 
# and the final variable 147, includes the classification of whether the event was a heart attack or not.

# Patients with expenses over $10,000 in the nine month period were allocated to cost bucket 3.
# Patients with less than $2,000 in expenses were allocated to cost bucket 1.
# And the remaining patients with costs between $2,000 and $10,000 to cost bucket 2.
# Please note that the majority of patients, 4,400 out of 6,500, or 67.5% of all patients fell into the first bucket of low expenses.

# Predicting Heart Attacks using Clustering

# The clustering methods we used were spectral clustering and k-means clustering.
# We first specify the number of clusters k.
# Then we randomly assign each data point to a cluster.
# We then compute the cluster centroids.
# We re-assign each point to the closest cluster centroid.
# We then re-compute the cluster centroids,
# and we repeat steps 4 and 5 until no improvement is made.

# After we construct the clusters in the training set, we assign new observations to clusters by proximity to the centroid of each cluster.
# We measure performance by recording the average performance rate in each cluster.

# Understanding Cluster Patterns

# We selected six patterns to present in this lecture--
# Cluster 1, 6, and 7, in Cost Bucket 2, and Clusters 4, 5, and 10, in Cost Bucket 3.

# The first pattern shows the occurrence of chest pain three months before the heart attack.

# The next pattern reveals an increasing occurrence of chronic obstructive pulmonary disease, COPD, for short.
# Patients from Cluster 7 in Bucket 2 have regular doctor visits for COPD.

# The next pattern shows gradually increasing occurrence of anemia.

# The final pattern shows the occurrence of diabetes as a pattern for heart attacks.

# The Analytics Edge

# What is the impact of clustering?
# Clustering members within each cost bucket yielded better predictions for heart attacks within clusters.
# Grouping patients in clusters exhibits temporal diagnostic patterns within nine months of a heart attack.
# These patterns can be incorporated in the diagnostic rules for heart attacks.
# The approach shows that using analytics for early heart failure detection through pattern recognition can lead to interesting new insights.




