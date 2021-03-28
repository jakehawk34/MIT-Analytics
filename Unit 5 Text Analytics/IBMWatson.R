# Unit 5: Text Analytics
# Man vs. Machine: How IBM Built a Jeopardy Champion

# In 2005, a team at IBM Research started creating a computer that would compete at Jeopardy!

# Six years later, a two-game exhibition match aired on television.
# The winner would receive a million dollars.
# The contestants were Ken Jennings, the longest winning champion, whose longest winning streak was 75 days;
# Brad Rutter, who was the biggest money winner of over 3.5 million; 
# and Watson, a supercomputer with 3,000 processors and a database of 200 million pages of information.

# Watson received each question in text form. Normally, the players see and hear the questions,
# but Watson couldn't hear anything, so they decided to feed him the questions in text instead.
# With the question in text form, IBM was able to use text analytics and other analytical methods
# to make Watson a competitive player. Overall, they used 100 different techniques for analyzing natural language, finding hypotheses
# for the questions, and ranking these hypotheses to pick an answer. Watson had a huge database of sources and several basic tools
# to help understand language. The database consisted of a massive number of data sources, like encyclopedias, texts, manuals, magazines,
# and downloaded pages of Wikipedia.

# Then, using these data sources and tools, Watson would answer a question by going through four major steps.
# The first step is question analysis, where Watson tries to figure out what the question is looking for.
# The second step is hypothesis generation, where Watson searches the information sources for possible answers.
# After this, Watson moves on to step three, when the different hypotheses are scored. 
# This means that a confidence level has to be computed for each answer.
# The final step is ranking the hypotheses to look for a highly-supported answer.

# When Watson receives a question, the first step is question analysis.
# One of the things Watson tries to figure out in this step is what the question is looking for.
# This is defined as trying to find the Lexical Answer Type, or LAT, of the question.
# The LAT is the word or noun in the question that specifies the type of answer.
# You should be able to replace the LAT with the answer to complete the sentence.

# However, in an analysis of 20,000 questions, 2,500 distinct LATs were found, and 12% of the questions did not even have an explicit LAT.
# They had LATs like "it's." Furthermore, even the most frequent 200 explicit LATs cover less than 50% of the questions.
# So to enhance the question analysis step, Watson also performs relation detection
# to find relationships among words and decomposition to split the question into different clues.

# The second step in Watson is hypothesis generation. The goal of this step is to use the question analysis of step
# one to produce candidate answers by searching the databases.
# In this step several hundred candidate answers are generated.
# If the correct answer is not generated at this stage, Watson has no hope of getting the question right.
# Therefore, this step errors on the side of generating a lot of hypotheses and leaves it up to the next step to find the correct answer.

# After Watson has completed the initial two steps of question analysis and hypothesis generation,
# it's then time to move on to step three, where each of the hypotheses are scored.
# In this step, Watson computes confidence levels for each possible answer or hypothesis.
# This is necessary to accurately estimate the probability of a proposed answer being correct.
# Watson will only buzz in to answer a question if a confidence level for one of the hypotheses is above a threshold.

# To compute these confidence levels, Watson combines a large number of different methods.
# First, Watson starts with lightweight scoring algorithms to prune down the large set of hypotheses.
# An example of a lightweight scoring algorithm is computing the likelihood that a candidate answer is actually
# an instance of the LAT.

# Then Watson moves into more advanced scoring analytics. Watson needs to gather supporting evidence for each candidate answer.
# One way of doing this is through a method called passage search, where passages are retrieved that contain the hypothesis text.

# Now, the scoring analytics determine the degree of certainty that the evidence supports the candidate answers.
# More than 50 different scoring components are used. One example is analyzing temporal relationships. 
# Consider the Jeopardy question-- "In 1594, he took a job as a tax collector in Andalusia."
# Two candidate answers are Thoreau and Cervantes. However, this algorithm would determine that Thoreau was not born until 1817.
# So it would give a higher score to Cervantes.

# Once all of the scoring algorithms are run, Watson needs to select the single best supported hypothesis.
# Before this can be done, similar answers need to be merged, since multiple candidate answers may be equivalent.
# Now, Watson is ready to rank the hypotheses and estimate an overall confidence for each.
# To do this, predictive analytics are used.
# To compute an overall confidence level for each candidate answer, Watson uses logistic regression.
# The training data is a set of historical Jeopardy questions and all of the candidate answers.
# Each of the scoring algorithms is used as an independent variable.
# Then, logistic regression is used to predict whether or not a candidate answer is correct using the scores.
# This gives each score a weight and computes an overall profitability or confidence that a candidate answer is correct.
# If the highest confidence level for one of the candidate answers for a question is high enough, Watson buzzes in to answer the question.

# Let us look at the results of the game.
# Looking at these numbers, it is fair to say that Watson's victory over the two best human players has been a decisive one.
# In the first day, Watson's winnings were more than double the sum of the winnings of both players.
# And in the second day, it was more than their sum.
# Overall, the winnings for Watson were almost double the sum of the winnings of both players-- a rather decisive victory.




