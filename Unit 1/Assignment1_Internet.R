# Assignment 1
# Internet Privacy Poll

poll = read.csv("AnonymityPoll.csv", stringsAsFactors = TRUE)
str(poll)
summary(poll)

# 472 interviewees do not use a smartphone, 487 interviewees use a smartphone
table(poll$Smartphone)
# 43 interviewees have a missing value for whether or not they have a smartphone
table(is.na(poll$Smartphone))

# Table for State and Region
table(poll$State, poll$Region)
# Illinois, Indiana, Iowa, Kansas, Michigan, Minnesota, Missouri, 
# Nebraska, North Dakota, Ohio, South Dakota, Wisconsin are in the Midwest region

SouthInterviewees = subset(poll, Region == "South")
table(SouthInterviewees$State)
# Texas was the state in the South region with largest number of interviewees

table(poll$Internet.Use, poll$Smartphone)
# 186 interviewees reported not having used the Internet and not having used a smartphone
# 470 interviewees reported having used the Internet and having used a smartphone
# 285 interviewees reported having used the Internet and not having used a smartphone?
# 17 interviewees reported not having used the Internet and having used a smartphone?

summary(poll)
# 1 interviewee has a missing value for Internet Use
# 43 interviewees have missing values for Smartphone

limited = subset(poll, Smartphone == 1 | Internet.Use == 1)
# 792 interviewees are in the "limited" subset

summary(limited)

mean(limited$Info.On.Internet)
# Average of 3.795 pieces of personal information on the Internet

table(limited$Info.On.Internet)
# 105 interviewees reported 0 for number of pieces of personal info on Internet
# 8 interviewees reported 11 for number of pieces of personal info on Internet

table(limited$Worry.About.Info)
summary(limited)
# 0.489 or 48.9% of interviewees worry about their info being on the Internet (excluding missing values)
# 0.3692 or 36.92% of interviewees believe anonymity is possible on the Internet
# 0.1633 or 16.33% of interviewees have tried masking their identity on the Internet
# 0.2559 or 25.59% of interviewees believe US Privacy Laws are effective

# Histogram for age in limited subset
hist(limited$Age)
# People around 60 years old are the best represented age group in the data set

# Plot Age against Info.On.Internet
plot(limited$Age, limited$Info.On.Internet)
max(table(limited$Age, limited$Info.On.Internet))
# 6 is the largest number of interviewees that have exactly the same value in their Age variable 
# AND the same value in their Info.On.Internet variable

# Demo of jitter()
jitter(c(1, 2, 3))

# Plot Age against Info.On.Internet with jitter
plot(jitter(limited$Age), jitter(limited$Info.On.Internet))

tapply(limited$Info.On.Internet, limited$Smartphone, summary)
# 4.368 is the average number of pieces of info for smartphone users
# 2.923 is the average number of pieces of info for non-smartphone user

tapply(limited$Tried.Masking.Identity, limited$Smartphone, summary)
# 0.1174 of non-smartphone users have tried masking their identity
# 0.1925 of smartphone users have tried masking their identity






