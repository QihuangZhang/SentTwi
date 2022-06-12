#  0. Global Parameters and Module Loadings
#  0.1 Load Packages
import nltk
#nltk.download('stopwords') 
#nltk.download('wordnet')
#nltk.download('vader_lexicon')
import os
import datetime
from nrclex import NRCLex

# Set random seed
seed = 123
# Data manipulation/analysis
import numpy as np
import pandas as pd
import csv
# Text preprocessing/analysis
import re
from nltk.corpus import stopwords
from nltk.stem import WordNetLemmatizer
from nltk.tokenize import RegexpTokenizer
from nltk.sentiment.vader import SentimentIntensityAnalyzer
from textblob import TextBlob
from scipy.sparse import hstack, csr_matrix
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.preprocessing import MinMaxScaler
# Modelling
from sklearn.model_selection import train_test_split, cross_validate, GridSearchCV, RandomizedSearchCV
from sklearn.linear_model import LogisticRegression, SGDClassifier
from sklearn.naive_bayes import MultinomialNB
from sklearn.metrics import classification_report, confusion_matrix
from sklearn.pipeline import Pipeline

# Remove Stop words
from nltk.corpus import stopwords 
from nltk.tokenize import word_tokenize

#  0.2 Load Useful Functions

def remove_url(txt):
    """Replace URLs found in a text string with nothing 
    (i.e. it will remove the URL from the string).
    Parameters
    ----------
    txt : string
        A text string that you want to parse and remove urls.

    Returns
    -------
    The same txt string with url's removed.
    """
    #
    url_pattern = re.compile(r'https?://\S+|www\.\S+')
    no_url = url_pattern.sub(r'', txt)
    no_at = " ".join(filter(lambda x:x[0]!='@', no_url.split()))
    #
    return no_at

# VADER = SentimentIntensityAnalyzer()

# 1. Sentiment Analsysis - VADER


# os.chdir('N:\\Work\\waterloo\\2020\\SentTwi\\code\\SentTwi\\Toronto')

# keyword = "lockdown+"
keyword = "vaccine+"

start_date = datetime.date(2020, 2, 24)
end_date = datetime.date(2020, 10, 14)
delta = datetime.timedelta(days=1)

date =  []
VADERscore = []
SentiScores = pd.DataFrame(columns = ["anticip","positive","negative","sadness","disgust","joy","anger","surprise","fear","trust","neg","neu","pos","compound"])


#while start_date <= end_date:
#    print(start_date)
#    tweetday = pd.read_csv('Toronto'+start_date.strftime('%y-%m-%d')+'.csv')
#    tweetqc= [remove_url(tweet) for tweet in tweetday.content]
#    NRCobj = [NRCLex(tweet).raw_emotion_scores for tweet in tweetqc] 
#    nwords = [len(NRCLex(tweet).words) for tweet in tweetqc]
#    nwordstotal = sum(nwords)
#    NRCscore = pd.DataFrame(NRCobj).sum(axis=0)
#    NRCfreq = pd.Series([i/nwordstotal for i in NRCscore])
#    NRCfreq.index = NRCscore.index
#    polarscore = pd.DataFrame([VADER.polarity_scores(tweet) for tweet in tweetqc])
#    avgscore  = polarscore.mean(axis = 0 )
#    newitems = pd.concat([avgscore, NRCfreq], axis =0)
#    SentiScores = SentiScores.append(newitems.to_frame().transpose())
#    tweetday[['neg', 'neu', 'pos', 'compound']]  = polarscore
#    tweetday[["anticip","positive","negative","sadness","disgust","joy","anger","surprise","fear","trust"]]  = pd.DataFrame(NRCobj)
#    tweetday.to_csv('Toronto_sentiscore_'+start_date.strftime('%y-%m-%d')+'.csv', encoding ='utf-8-sig')
#    start_date += delta

#tweetcounts = pd.read_csv('Summary_Toronto.csv')
#SentiScores.index = tweetcounts.index
#combine = pd.concat([tweetcounts, SentiScores], axis=1, sort=False)

#combine.to_csv('Summary_Toronto_scored.csv', encoding ='utf-8-sig')

citiesiter = ["New_York", "Los_Angeles", "Seattle"]
# citiesiter = ["Toronto", "Montreal","Vancouver", "Calgary", "New_York", "Los_Angeles", "Seattle", "Chicago"]


# Duration of the Original Version
# start_date = datetime.date(2020, 2, 24) 
# end_date = datetime.date(2020, 10, 14)
# delta = datetime.timedelta(days=1)

for city_i in citiesiter:
    print(city_i)

    start_date = datetime.date(2020, 2, 24) 
    end_date = datetime.date(2020, 10, 14)
    delta = datetime.timedelta(days=1)


    ### Transform the FileExistsError json into csv file filtering with English tweets only

    os.chdir('N:\\Work\\waterloo\\2020\\SentTwi\\code\\SentTwi\\'+city_i)

    SentiScores = pd.DataFrame(columns = ["anticipation","positive","negative","sadness","disgust","joy","anger","surprise","fear","trust","neg","neu","pos","compound"])

    while start_date <= end_date:
        print(start_date)
        tweetday = pd.read_csv(keyword+city_i+start_date.strftime('%y-%m-%d')+'.csv')
        if tweetday.empty:
            newitem = pd.DataFrame([0] * 14).transpose()
            newitem.columns = ["anticipation","positive","negative","sadness","disgust","joy","anger","surprise","fear","trust","neg","neu","pos","compound"]
            SentiScores = SentiScores.append(newitem, ignore_index= True)
        else:
            tweetqc= [remove_url(tweet) for tweet in tweetday.content]
            NRCobj = [NRCLex(tweet).raw_emotion_scores for tweet in tweetqc] 
            nwords = [len(NRCLex(tweet).words) for tweet in tweetqc]
            nwordstotal = sum(nwords)
            NRCscore = pd.DataFrame(NRCobj).sum(axis=0)
            NRCfreq = pd.Series([i/nwordstotal for i in NRCscore])
            NRCfreq.index = NRCscore.index
            polarscore = pd.DataFrame([VADER.polarity_scores(tweet) for tweet in tweetqc])
            avgscore  = polarscore.mean(axis = 0 )
            newitems = pd.concat([avgscore, NRCfreq], axis =0)
            SentiScores = SentiScores.append(newitems.to_frame().transpose())
            tweetday[['neg', 'neu', 'pos', 'compound']]  = polarscore
            NRCtemplete = pd.DataFrame(columns = ["anticipation","positive","negative","sadness","disgust","joy","anger","surprise","fear","trust"])
            NRCtemplete = NRCtemplete.append(pd.DataFrame(NRCobj))
            tweetday[["anticipation","positive","negative","sadness","disgust","joy","anger","surprise","fear","trust"]]  = NRCtemplete
            tweetday[["nwords"]] = pd.DataFrame(nwords)
        tweetday.to_csv(keyword+city_i+'_sentiscore_'+start_date.strftime('%y-%m-%d')+'.csv', encoding ='utf-8-sig')
        start_date += delta
    #
    tweetcounts = pd.read_csv(keyword+'Summary_'+ city_i +'.csv')
    SentiScores.index = tweetcounts.index
    combine = pd.concat([tweetcounts, SentiScores], axis=1, sort=False)
    #
    combine.to_csv(keyword+'Summary_'+ city_i +'_scored.csv', encoding ='utf-8-sig')
# end for



# 2. Sentiment Analsysis - roBERTa
from transformers import AutoModelForSequenceClassification
from transformers import TFAutoModelForSequenceClassification
from transformers import AutoTokenizer
import numpy as np
from scipy.special import softmax

task='sentiment'
MODEL = f"twitter-roberta-base-{task}"

tokenizer = AutoTokenizer.from_pretrained(MODEL)

labels_sentiments = ["negative", "neutral", "positive"]
labels_emotions = ["anger", "joy", "optimism", "sadness"]

text = "He is not a good person"
# text = preprocess(text)
encoded_input = tokenizer(text, return_tensors='pt')
output = model(**encoded_input)
scores = output[0][0].detach().numpy()
scores = softmax(scores)

# # TF
# model = TFAutoModelForSequenceClassification.from_pretrained(MODEL)
# model.save_pretrained(MODEL)

# text = "Good night 😊"
# encoded_input = tokenizer(text, return_tensors='tf')
# output = model(encoded_input)
# scores = output[0][0].numpy()
# scores = softmax(scores)

ranking = np.argsort(scores)
ranking = ranking[::-1]
for i in range(scores.shape[0]):
    l = labels[ranking[i]]
    s = scores[ranking[i]]
    print(f"{i+1}) {l} {np.round(float(s), 4)}")

