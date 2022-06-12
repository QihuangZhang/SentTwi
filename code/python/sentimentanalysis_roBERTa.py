#  0. Global Parameters and Module Loadings
#  0.1 Load Packages
import nltk
#nltk.download('stopwords') 
#nltk.download('wordnet')
#nltk.download('vader_lexicon')
import os
import datetime

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
# from textblob import TextBlob
from scipy.sparse import hstack, csr_matrix

# Remove Stop words
from nltk.corpus import stopwords 
from nltk.tokenize import word_tokenize

from transformers import AutoModelForSequenceClassification
from transformers import TFAutoModelForSequenceClassification
from transformers import AutoTokenizer
from scipy.special import softmax


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

# 2. Sentiment Analsysis - roBERTa

MODEL_sen = f"twitter-roberta-base-sentiment"
MODEL_emo = f"twitter-roberta-base-emotion"

tokenizer_sen = AutoTokenizer.from_pretrained(MODEL_sen)
tokenizer_emo = AutoTokenizer.from_pretrained(MODEL_emo)

labels_sentiments = ["negative", "neutral", "positive"]
labels_emotions = ["anger", "joy", "optimism", "sadness"]
labels_all = labels_sentiments + labels_emotions


text = "He is NOT a good person"

model_sen = AutoModelForSequenceClassification.from_pretrained(MODEL_sen)
model_sen.save_pretrained(MODEL_sen)
model_emo = AutoModelForSequenceClassification.from_pretrained(MODEL_emo)
model_emo.save_pretrained(MODEL_emo)

# text = preprocess(text)
def get_sentiment_score(text):
    try:
        encoded_input = tokenizer_sen(text, return_tensors='pt')
        output = model_sen(**encoded_input)
        scores = output[0][0].detach().numpy()
        scores = softmax(scores)
        return scores
    except IndexError:
        print ("inprorper tweet detected.")
        return [0]

def get_sentiment_emotions(text):
    encoded_input = tokenizer_emo(text, return_tensors='pt')
    output = model_emo(**encoded_input)
    scores = output[0][0].detach().numpy()
    scores = softmax(scores)
    return scores

get_sentiment_score(text)
get_sentiment_emotions(text)


# # TF
# model = TFAutoModelForSequenceClassification.from_pretrained(MODEL)
# model.save_pretrained(MODEL)

# text = "Good night 😊"
# encoded_input = tokenizer(text, return_tensors='tf')
# output = model(encoded_input)
# scores = output[0][0].numpy()
# scores = softmax(scores)

# ranking = np.argsort(scores)
# ranking = ranking[::-1]
# for i in range(scores.shape[0]):
#     l = labels[ranking[i]]
#     s = scores[ranking[i]]
#     print(f"{i+1}) {l} {np.round(float(s), 4)}")





# os.chdir('N:\\Work\\waterloo\\2020\\SentTwi\\code\\SentTwi\\Toronto')

keyword = ""
keyword = "mask+"
# keyword = "lockdown+"
# keyword = "vaccine+"

start_date = datetime.date(2020, 2, 24)
end_date = datetime.date(2020, 10, 14)
delta = datetime.timedelta(days=1)

date =  []
SentiScores = pd.DataFrame(columns = labels_all)


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


citiesiter = ["Toronto", "Montreal","Vancouver", "Calgary", "New_York", "Los_Angeles", "Seattle", "Chicago"]
# citiesiter2 = ["Toronto", "Montreal","Vancouver", "Calgary", "New_York", "Los_Angeles", "Seattle", "Chicago"]


# Duration of the Original Version
# start_date = datetime.date(2020, 2, 24) 
# end_date = datetime.date(2020, 10, 14)
# delta = datetime.timedelta(days=1)

for city_i in citiesiter:
    print(city_i)
    # The Date to be iterated
    start_date = datetime.date(2020, 2, 24) 
    end_date = datetime.date(2020, 10, 14)
    delta = datetime.timedelta(days=1)
    ### Transform the FileExistsError json into csv file filtering with English tweets only
    SentiScores = pd.DataFrame(columns = labels_all)
    #
    while start_date <= end_date:
        print(start_date)
        tweetday = pd.read_csv('data/{city}/{keyword}{city}{thedate}.csv'.format(city = city_i, keyword = keyword, thedate = start_date.strftime('%y-%m-%d')))
        if tweetday.empty:
            newitem = pd.DataFrame([0] * 7).transpose()
            newitem.columns = labels_all
            SentiScores = SentiScores.append(newitem, ignore_index= True)
        else:
            tweetqc= [remove_url(tweet) for tweet in tweetday.content if type(tweet) is str]
            sent_roberta = np.stack([get_sentiment_score(tweet) for tweet in tweetqc if len(get_sentiment_score(tweet))>1])
            emo_roberta = np.stack([get_sentiment_emotions(tweet) for tweet in tweetqc if len(get_sentiment_score(tweet))>1])
            all_roberta = np.concatenate ((sent_roberta, emo_roberta), axis = 1)  
            all_roberta_pd = pd.DataFrame (all_roberta, columns = labels_all)
            all_roberta_mean = all_roberta_pd.mean(axis = 0)
            SentiScores = SentiScores.append(all_roberta_mean.to_frame().transpose())
            tweetday[labels_all]  = all_roberta_pd
            # nwords = [len(NRCLex(tweetday).words) for tweet in tweetqc]
            # tweetday[["nwords"]] = pd.DataFrame(nwords)
        tweetday.to_csv('data/{city}/{keyword}{city}_robertascore_{thedate}.csv'.format(city = city_i, keyword = keyword, thedate = start_date.strftime('%y-%m-%d')), encoding ='utf-8-sig')
        start_date += delta
    #
    tweetcounts = pd.read_csv('data/{city}/{keyword}Summary_{city}.csv'.format(city = city_i, keyword = keyword))
    SentiScores.index = tweetcounts.index
    combine = pd.concat([tweetcounts, SentiScores], axis=1, sort=False)
    #
    combine.to_csv('data/{city}/{keyword}Summary_{city}_robertascored.csv'.format(city = city_i, keyword = keyword), encoding ='utf-8-sig')
# end for
