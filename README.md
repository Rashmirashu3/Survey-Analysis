# Net Promoter Score - Survey Analysis

The final commented code can be found in the file "Code - Survey Analysis.R"

The code consists of the following 3 parts:

### 1. Data Preparation

#####  1.1 Data Cleaning 
#####  1.2 Data Wrangling

### 2. Modeling 

#####  2.1 Logistic Regression
#####  2.2 Random forest

### 3. Text analysis using unsupervised learning algorithms

~~

## 1. Data Preparation

This section can be found in the document "Data Wrangling Report.pdf". 
The final fields used for the analysis can be found in the file "Final Fields used for Analysis.xlsx"

## 2. Models to Predict NPS Score
1. Multinomial Logistic Regression
2. Random Forest

We have used machine learning techniquest to predict NPS scores of customers who might not have participated in the survey. 
Out of the two techniques Random Forest performed performed better with 76% accuracy. 

The most important variables for Random Forest turned out to be the following. 
* Home Agency State
* Client Zip Region
* life Annualized Premium
* life Coverage Face Amount 
* life Net Cash Value
* life Account Value
* Owner Age Bin
* Agent Tenure Bin
* Agent Age Bin
* Annuity Account Value

To see the list of all the important variables and related plots, refer to the file "Variable Importance - Random forest.xlsx"

We then used Hyper parameter tuning with "ranger" package in R to perform parameter tuning. There were total 256 different models tested to find the best fit which reduced the OOB-error to 25.53%

## 3. Text Analysis

### Overview:
This analysis was done on detractors population for survey responses of Q3, Q8 and promoters population for survey responses of Q3.
We have created Document Term Matrix by implementing NLP and tokenization. We have taken tokens of both unigrams and bigrams in our analysis. Later, Latent Dirchlet Allocation with Gibbs sampling is used to group the similar tokens and label the topics with most frequent bigrams. 

### Evaluation of the Model:
Probabilistic coherence measures how associated words are in a topic, controlling for statistical independence. For example, suppose you have a corpus of articles from the sports section of a newspaper. A topic with the words {sport, sports, ball, fan, athlete} would look great if you look at correlation, without correcting for independence. But we actually know that it’s a terrible topic because the words are so frequent in this corpus as to be meaningless. In other words, they are highly correlated with each other but they are statistically-independent of each other.

For each pair of words {a,b} in the top M words in a topic, probabilistic coherence calculates P(b|a)−P(b), where {a} is more probable than {b} in the topic.

Here’s the logic: if we restrict our search to only documents that contain the word {a}, then the word {b} should be more more probable in those documents than if chosen at random from the corpus. P(b|a) measures how probable {b} is only in documents containing {a}. P(b) measures how probable {b} is in the corpus as a whole. If {b} is not more probable in documents containing {a}, then the difference P(b|a)−P(b) should be close to zero.

For example, suppose the top 4 words in a topic are {a,b,c,d}. Then, we calculate

P(a|b)−P(b), P(a|c)−P(c), P(a|d)−P(d)
P(b|c)−P(c), P(b|d)−P(d)
P(c|d)−P(d)
And all 6 differences are averaged together, giving the probabilistic coherence measure.

### Prediction:
A simple dot product with the DTM of your new documents will get new topic predictions.

Θnew=A⋅ΓT

Both methods are available through predict.lda_topic_model with the method argument (“dot” or “gibbs”). Which method should you use? In most cases, we have used dot method, as it has higher speed and topic distribution.


### References:

1. https://cran.r-project.org/web/packages/textmineR/vignettes/c_topic_modeling.html
2. https://medium.com/@tomar.ankur287/topic-modeling-using-lda-and-gibbs-sampling-explained-49d49b3d1045
3. https://www.tidytextmining.com/topicmodeling.html
4. https://www.kaggle.com/regiso/tips-and-tricks-for-building-topic-models-in-r
5. https://rpubs.com/wsevier/LDA
