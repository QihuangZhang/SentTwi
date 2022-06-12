# Sentiment Analysis and Causal Learning of COVID-19 Tweets prior to the Rollout of Vaccines


### Qihuang Zhang, Grace Y. Yi, Li-Pang Chen, Wenqing He



While the impact of the COVID-19 pandemic has been widely studied, relatively fewer discussions about the sentimental reaction of the public are available. In this article, we scrape \mbox{COVID-19} related tweets on the microblogging platform, Twitter, and examine the tweets from Feb 24, 2020 to Oct 14, 2020 in four Canadian cities (Toronto, Montreal, Vancouver, and Calgary) and four U.S. cities (New York, Los Angeles, Chicago, and Seattle). Applying  the RoBERTa,  Vader and NRC approaches, we evaluate sentiment intensity scores and visualize the information over different periods of the pandemic. Sentiment scores for the tweets concerning three anti-epidemic measures, masks, vaccine, and lockdown, are computed for comparison. We explore possible causal relationships among the variables concerning tweet activities and sentiment scores of COVID-19 related tweets by integrating the echo state network method with convergent cross-mapping. Our analysis shows that public sentiments about COVID-19 vary from time to time and from place to place and that they are different with respect to anti-epidemic measures of "masks",  "vaccines", and "lockdown". Evidence of the causal relationship is revealed for the examined variables, assuming the suggested model is feasible.

## Data 
In this repository, we include the sentiment scores data generated in the study at  [data folder](https://github.com/QihuangZhang/SentTwi/tree/main/data). The files in the folder include time series of sentiment scores for the topic of "COVID-19", "vaccine", "mask", and "lockdown".



## Code File Structure
### Data Analaysis
* [Text scarping](https://github.com/QihuangZhang/SentTwi/blob/main/code/python/scrapTweet.txt)
* [Sentiment Analysis - RoBERTa](https://github.com/QihuangZhang/SentTwi/blob/main/code/python/sentimentanalysis_roBERTa.py)
* [Example of Resevoir Commputing (Echo State Network)](https://github.com/QihuangZhang/COVID-AR-Error/blob/main/code/python/RC_compute.py)
* [Cross-validation in determine the optiomal parameters of Echo State Network](https://github.com/QihuangZhang/SentTwi/blob/main/code/python/crossvalid.py)
* [Example of Resevoir Commputing (Echo State Network)](https://github.com/QihuangZhang/COVID-AR-Error/blob/main/code/python/RC_compute.py)


### Data visalization
* [Descriptive Statistics](https://github.com/QihuangZhang/SentTwi/blob/main/code/R/descriptivestatistics_roberta.R)
* [Sentiment Analysis Results 1](https://github.com/QihuangZhang/SentTwi/blob/main/code/R/sentimentanalysis_roberta.R)
* [Sentiment Analysis Results 1](https://github.com/QihuangZhang/SentTwi/blob/main/code/R/sentimentanalysis.R)