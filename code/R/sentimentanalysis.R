# 0. Global Parameters -------------------------------------------------------

library(dplyr)
library(scales)
library(lazyeval)
library(ggridges)
library(tidyr)
library(xtable)

cityfull <- c("Toronto","Vancouver","Montreal","Calgary","New_York","Los_Angeles","Seattle","Chicago")
citylist <- c("Toronto","Vancouver","Montreal","Calgary")
provlist <- c("Ontario","British Columbia","Quebec","Alberta")

UScitylist <- c("New_York","Los_Angeles","Seattle","Chicago")
USstatlist <- c("New York","California","Washington","Illinois")

# 1. Sentiment Analysis - Example Vadar -------------------------------------------------------

CompData <- NULL


for (i in 1:8){
  cityi <- cityfull[i]
  data <- read.csv(paste0("data/",cityi,"/","Summary_",cityi,"_scored.csv"))
  data_long <- data %>% 
    select(c(3,10,24))
  CompData <- rbind(CompData, data_long)
}

CompData$Date <- as.Date(CompData$Date)

CompData$Country <- rep(c("Canada","US"), each = dim (CompData)[1]/2)

CompData$city <- factor(CompData$city, levels = rev(cityfull), 
                        labels = rev(c("Toronto","Vancouver","Montreal","Calgary","New York","Los Angeles","Seattle","Chicago")))

library(ggplot2)

CompData$Method <- "VADER"

CompDataVADER <- CompData

# pdf(file = "output/SentimentVADER.pdf",height = 8, width = 18)
# 
# # Heatmap 
# HeatSentiment <- ggplot(CompData, aes(x=Date, city, fill= compound)) + 
#   geom_tile() +
#   # scale_fill_gradient(low="#5CB270", high="#F4F269")
#   scale_fill_gradient2(low="#2F7F86", high="#E9AA38", mid = "white") +
#   facet_grid(Country~ ., scales = "free",  space = "free")+
#   labs(fill = "Sentiment\n Score") + 
#   scale_x_date(breaks = breaks_pretty(8))   +
#   # coord_cartesian(ylim=c(-0.2,1.5))+
#   theme_classic()
# 
# HeatSentiment
# 
# 
# dev.off()

CompData_both_IF <- CompData %>% mutate(Measure = "VADERSenti") %>% select(c(Date, Measure, city, compound))

CompData_both_IF_wide <- spread(CompData_both_IF,city, compound)

write.csv(CompData_both_IF_wide,file="output/VADERSenti.csv")


# 2. Sentiment Analysis - roBERTa -------------------------------------------------------

CompData <- NULL


for (i in 1:8){
  cityi <- cityfull[i]
  data <- read.csv(paste0("data/",cityi,"/","Summary_",cityi,"_robertascored.csv"))
  data_long <- data %>%
    mutate( compound = positive - negative) %>%
    select(c("Date","city","compound"))
  CompData <- rbind(CompData, data_long)
}

CompData$Date <- as.Date(CompData$Date)

CompData$Country <- rep(c("Canada","US"), each = dim (CompData)[1]/2)

CompData$city <- factor(CompData$city, levels = rev(cityfull), 
                        labels = rev(c("Toronto","Vancouver","Montreal","Calgary","New York","Los Angeles","Seattle","Chicago")))

library(ggplot2)

CompData$Method <- "roBERTa"

CompDataroBERTa <- CompData

CompDataBoth <- rbind(CompDataVADER, CompDataroBERTa) %>%
  mutate (Method = factor(Method, levels = c("VADER", "roBERTa")))

pdf(file = paste0("output/Sentimentlineplots.pdf"), height = 8, width = 18)
  # lineplot

HeatSentiment <- ggplot(CompDataBoth) +
  geom_line(aes(x=Date, y=compound, color = Method), size=1, alpha= 0.8) +
  scale_color_manual(values = c( "#EAAE37", "#2E8289"))  +
  facet_grid(city~ .)+
  labs(y = "Sentiment\n Score", color = "City") +
  scale_x_date(breaks = breaks_pretty(8))   +
  geom_hline(yintercept=0, linetype="dashed", color = "#565F41") +
  theme_bw()  +
  theme(text=element_text(size=12, family="URWHelvetica"), axis.text = element_text(size = 12, family="URWHelvetica"), panel.spacing = unit(1, "lines")) +
  theme(strip.background =element_rect(fill="#3F4536",color="#3F4536"))+ # #535b44
  theme(strip.text = element_text(colour = 'white')) +
  theme(panel.border = element_rect(colour = "#3F4536"))
print(HeatSentiment)
dev.off()


# for (country_ID in c("Canada","US")){
#   pdf(file = paste0("output/Sentimentroberta", country_ID,".pdf"), height = 4, width = 18)
#     # lineplot 
#   
#   HeatSentiment <- ggplot(CompData %>%
#                             filter (Country == country_ID) ) + 
#     geom_line(aes(x=Date, y=compound, color = city), size=1, alpha= 0.8) +
#     scale_color_manual(values = c("#EAAE37", "#B4674E", "#2E8289", "#91D5DE"))  +
#     facet_grid(city~ .)+
#     labs(y = "Sentiment\n Score", color = "City") + 
#     scale_x_date(breaks = breaks_pretty(8))   + 
#     geom_hline(yintercept=0, linetype="dashed", color = "#565F41") + 
#     theme_bw()  +
#     theme(text=element_text(size=12, family="URWHelvetica"), axis.text = element_text(size = 12, family="URWHelvetica"), panel.spacing = unit(1, "lines")) +
#     theme(strip.background =element_rect(fill="#3F4536",color="#3F4536"))+ # #535b44
#     theme(strip.text = element_text(colour = 'white')) +
#     theme(panel.border = element_rect(colour = "#3F4536"))
#   print(HeatSentiment)
#   dev.off()
#   
# }

CompData_both_IF <- CompData %>% mutate(Measure = "robertaSenti") %>% select(c(Date, Measure, city, compound))

CompData_both_IF_wide <- spread(CompData_both_IF,city, compound)

write.csv(CompData_both_IF_wide,file="output/robertaSenti.csv")


# 3. Emotion Analysis - NRC -------------------------------------------------------


# 4. Sentiment Analysis - NRC -------------------------------------------------------

# 4.1 NRC method -------------------------------------------------------

CompData <- NULL


for (i in 1:8){
  cityi <- cityfull[i]
  data <- read.csv(paste0("data/",cityi,"/","Summary_",cityi,"_robertascored.csv"))
  data_long <- gather(data[,c(3,10,14:17)], Mood, Score, anger:sadness)  
  CompData <- rbind(CompData, data_long)
}


CompData$Date <- as.Date(CompData$Date)

# CompData$Country <- rep(c("Canada","US"), each = dim (CompData)[1]/2)

CompData$city <- factor(CompData$city, levels = (cityfull), 
                        labels = (c("Toronto","Vancouver","Montreal","Calgary","New York","Los Angeles","Seattle","Chicago")))

CompData <- CompData %>%
  filter(!Mood %in% c("positive","negative"))
CompData$Mood <- factor(CompData$Mood, levels = c("joy", "trust", "anticip", "surprise", "fear", "sadness", "disgust",  "anger"), 
                        labels = c("joy", "trust", "anticipation", "surprise", "fear", "sadness", "disgust",  "anger"))



CompData <- CompData  %>%
  mutate(direction = case_when(
    Mood %in% c("joy", "trust", "anticipation", "surprise") ~ "Positive",
    Mood %in% c("fear", "sadness", "disgust",  "anger") ~ "Negative")) %>%
  mutate(Month = format(Date,"%b"))

CompData$Month <- factor(CompData$Month, levels = rev(c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",  "Sep",  "Oct")))

pdf(file = "output/SentimentNRC.pdf",height = 10, width = 18)

# Ridgesmap 
RidgesSentiment <- ggplot(CompData, aes(x=Score, y=Month, fill = direction)) + 
  # scale_fill_gradient(low="#5CB270", high="#F4F269")
  geom_density_ridges(alpha = 0.6)   +
  facet_grid(city~ Mood)+
  scale_fill_cyclical(values = c("#2F7F86","#E9AA38"), name = "Mood\nDirection", guide = "legend")+
  labs(y="City") +
  # coord_cartesian(ylim=c(-0.2,1.5))+
  theme_bw()

RidgesSentiment


dev.off()


# 4.2 NRC method - tweetwise -------------------------------------------------------


tweetwisedata <- NULL

start <- as.Date("24-02-20",format="%d-%m-%y")
end   <- as.Date("14-10-20",format="%d-%m-%y")

theDate <- start


for (i in 1:8){
  cityi <- cityfull[i]
  cat(paste0(cityi,"\n"))
  theDate <- start
  while (theDate <= end)
  { cat(paste0(theDate,"\n"))
    data <- read.csv(paste0("data/",cityi,"/",cityi,"_sentiscore_",format(theDate, "%y-%m-%d"),".csv"))
    if (dim(data)[1]!=0) {
      data_long <- data[,c(30:40)] %>% 
        mutate(Date= format(theDate, "%y-%m-%d"))  %>%
        mutate(City = cityi)
      data_moodes <- apply(data_long[,c(1:11)], MARGIN = 1, FUN = function(x){
        x[-length(x)]/x[length(x)]
      })
      data_long[,1:10] <- t(data_moodes)
      tweetwisedata <- rbind(tweetwisedata, data_long)
    }
    theDate <- theDate +1
  }
}


tweetlongtable <- gather(tweetwisedata, Mood, Score,anticip:trust)  

# save(tweetlongtable,file= "output/tweetlongtable.RData")
load(file= "output/tweetlongtable.RData")


tweetlongtable$Date <- as.Date(tweetlongtable$Date)

# CompData$Country <- rep(c("Canada","US"), each = dim (CompData)[1]/2)

tweetlongtable$city <- factor(tweetlongtable$City, levels = (cityfull), 
                        labels = (c("Toronto","Vancouver","Montreal","Calgary","New York","Los Angeles","Seattle","Chicago")))

tweetlongtable <- tweetlongtable %>%
  filter(!Mood %in% c("positive","negative"))
tweetlongtable$Mood <- factor(tweetlongtable$Mood, levels = c("joy", "trust", "anticip", "surprise", "fear", "sadness", "disgust",  "anger"), 
                        labels = c("joy", "trust", "anticipation", "surprise", "fear", "sadness", "disgust",  "anger"))

tweetlongtable <- tweetlongtable %>%
  mutate(direction = case_when(
    Mood %in% c("joy","trust","anticip","surprise") ~ "Positive",
    Mood %in% c("fear", "sadness", "disgust", "anger") ~ "Negative"))

pdf(file = "output/SentimentNRCbytweet.pdf",height = 12, width = 18)

# Ridgesmap 
RidgesSentiment <- ggplot(tweetlongtable, aes(x=Score, y=Mood, fill = direction)) + 
  # scale_fill_gradient(low="#5CB270", high="#F4F269")
  geom_density_ridges()   +
  facet_grid(.~ city)+ 
  scale_fill_cyclical(values = c("#E9AA38","#2F7F86")) +
  theme(text=element_text(size=30), axis.text = element_text(size = 20)) +
  theme_bw()
RidgesSentiment


dev.off()


# 4.3 Compute the average of sentiment score based tweetwise data stratified by time period -------------------------------------------------------

tweetwisedata <- NULL

start <- as.Date("24-02-20",format="%d-%m-%y")
end   <- as.Date("14-10-20",format="%d-%m-%y")

theDate <- start


for (i in 1:4){
  cityi <- cityfull[i]
  cat(paste0(cityi,"\n"))
  theDate <- start
  while (theDate <= end)
  { cat(paste0(theDate,"\n"))
    data <- read.csv(paste0("data/",cityi,"/",cityi,"_robertascore_",format(theDate, "%y-%m-%d"),".csv"))
    if (dim(data)[1]!=0) {
      data_long <- data[,c("negative", "neutral", "positive")] %>% 
        mutate(Date= format(theDate, "%y-%m-%d"))  %>%
        mutate(City = cityi)
      tweetwisedata <- rbind(tweetwisedata, data_long)
    }
    theDate <- theDate +1
  }
}


# save(tweetwisedata,file= "output/tweetlongtablesentiment.RData")
load(file= "output/tweetlongtablesentiment.RData")


tweetwisedata$Date <- as.Date(tweetwisedata$Date)

# CompData$Country <- rep(c("Canada","US"), each = dim (CompData)[1]/2)

tweetwisedata$city <- factor(tweetwisedata$City, levels = c("Toronto","Vancouver","Montreal","Calgary"), 
                              labels = (c("Toronto","Vancouver","Montreal","Calgary")))

# tweetlongtable <- tweetlongtable %>%
#   filter(!Mood %in% c("positive","negative"))
# tweetlongtable$Mood <- factor(tweetlongtable$Mood, levels = c("joy", "trust", "anticip", "surprise", "fear", "sadness", "disgust",  "anger"), 
#                               labels = c("joy", "trust", "anticipation", "surprise", "fear", "sadness", "disgust",  "anger"))

# tweetlongtable <- tweetlongtable %>%
#   mutate(direction = case_when(
#     Mood =="joy" | "trust"| "anticip"| "surprise" ~ "Positive",
#     Mood =="fear"| "sadness"| "disgust"|  "anger" ~ "Negative"))

twwise_period <- tweetwisedata %>% 
  mutate(Period = case_when(
    City == "Toronto" & Date <= as.Date("20-03-17") ~ "Period 1",
    City == "Toronto" & Date > as.Date("20-03-17") &  Date<= as.Date("20-05-19") ~ "Period 2",
    City == "Toronto" & Date > as.Date("20-05-19") ~ "Period 3",
    City == "Vancouver" & Date <= as.Date("20-03-18") ~ "Period 1",
    City == "Vancouver" & Date > as.Date("20-03-18") &  Date<= as.Date("20-05-19") ~ "Period 2",
    City == "Vancouver" & Date > as.Date("20-05-19") ~ "Period 3",
    City == "Montreal" & Date <= as.Date("20-03-12") ~ "Period 1",
    City == "Montreal" & Date > as.Date("20-03-12") &  Date <= as.Date("20-05-11") ~ "Period 2",
    City == "Montreal" & Date > as.Date("20-05-11") ~ "Period 3",
    City == "Calgary" & Date <= as.Date("20-03-17") ~ "Period 1",
    City == "Calgary" & Date > as.Date("20-03-17") &  Date<= as.Date("20-05-12") ~ "Period 2",
    City == "Calgary" & Date >as.Date("20-05-12") ~ "Period 3")) 


   
Tweetbyprdcity_daily <- twwise_period %>%
  mutate (compound = positive - negative) %>%
  group_by(Period, City, Date) %>%
  dplyr::summarize( ndaily = length(compound)) %>%
  data.frame() %>%
  group_by(Period, City) %>%
  dplyr::summarize(MeanCount = mean(ndaily, na.rm=TRUE)) %>%
  data.frame()


Tweetbyprdcity <- twwise_period %>%
  mutate (compound = positive - negative) %>%
  group_by(Period, City) %>%
  dplyr::summarize(Mean = mean(compound, na.rm=TRUE), sd = sd(compound, na.rm=TRUE)) %>%
  data.frame() %>%
  mutate(MeanCount = Tweetbyprdcity_daily$MeanCount)


Tweetbyprdcity_long <- gather(Tweetbyprdcity, measure, value, Mean:MeanCount, factor_key=TRUE)

Tweetbyprdcity_wide <- spread(Tweetbyprdcity_long, City, value)


xtable(Tweetbyprdcity_wide)


# 2.4 Compute the average of sentiment score based tweetwise data stratified by time period - US -------------------------------------------------------

tweetwisedata <- NULL

start <- as.Date("24-02-20",format="%d-%m-%y")
end   <- as.Date("14-10-20",format="%d-%m-%y")

theDate <- start


for (i in 5:8){
  cityi <- cityfull[i]
  cat(paste0(cityi,"\n"))
  theDate <- start
  while (theDate <= end)
  { cat(paste0(theDate,"\n"))
    data <- read.csv(paste0("data/",cityi,"/",cityi,"_robertascore_",format(theDate, "%y-%m-%d"),".csv"))
    if (dim(data)[1]!=0) {
      data_long <- data[,c("negative", "neutral", "positive")] %>% 
        mutate(Date= format(theDate, "%y-%m-%d"))  %>%
        mutate(City = cityi)
      tweetwisedata <- rbind(tweetwisedata, data_long)
    }
    theDate <- theDate +1
  }
}


# save(tweetwisedata,file= "output/tweetlongtablesentiment_US.RData")
load(file= "output/tweetlongtablesentiment_US.RData")


tweetwisedata$Date <- as.Date(tweetwisedata$Date)

# CompData$Country <- rep(c("Canada","US"), each = dim (CompData)[1]/2)

tweetwisedata$city <- factor(tweetwisedata$City, levels = c("New_York","Los_Angeles","Seattle","Chicago"), 
                             labels = (c("New York","Los Angeles","Seattle","Chicago")))

# tweetlongtable <- tweetlongtable %>%
#   filter(!Mood %in% c("positive","negative"))
# tweetlongtable$Mood <- factor(tweetlongtable$Mood, levels = c("joy", "trust", "anticip", "surprise", "fear", "sadness", "disgust",  "anger"), 
#                               labels = c("joy", "trust", "anticipation", "surprise", "fear", "sadness", "disgust",  "anger"))

# tweetlongtable <- tweetlongtable %>%
#   mutate(direction = case_when(
#     Mood =="joy" | "trust"| "anticip"| "surprise" ~ "Positive",
#     Mood =="fear"| "sadness"| "disgust"|  "anger" ~ "Negative"))

twwise_period <- tweetwisedata %>% 
  mutate(Period = case_when(
    City == "New_York" & Date <= as.Date("0020-03-07") ~ "Period 1",
    City == "New_York" & Date > as.Date("0020-03-07") &  Date<= as.Date("0020-06-08") ~ "Period 2",
    City == "New_York" & Date > as.Date("0020-06-08") ~ "Period 3",
    City == "Los_Angeles" & Date <= as.Date("0020-03-04") ~ "Period 1",
    City == "Los_Angeles" & Date > as.Date("0020-03-04") &  Date<= as.Date("0020-05-07") ~ "Period 2",
    City == "Los_Angeles" & Date > as.Date("0020-05-07") ~ "Period 3",
    City == "Seattle" & Date <= as.Date("0020-02-29") ~ "Period 1",
    City == "Seattle" & Date > as.Date("0020-02-29") &  Date <= as.Date("0020-05-01") ~ "Period 2",
    City == "Seattle" & Date > as.Date("0020-05-01") ~ "Period 3",
    City == "Chicago" & Date <= as.Date("0020-03-09") ~ "Period 1",
    City == "Chicago" & Date > as.Date("0020-03-09") &  Date<= as.Date("0020-05-05") ~ "Period 2",
    City == "Chicago" & Date >as.Date("0020-05-05") ~ "Period 3")) 




Tweetbyprdcity_daily <- twwise_period %>%
  mutate (compound = positive - negative) %>%
  group_by(Period, City, Date) %>%
  dplyr::summarize( ndaily = length(compound)) %>%
  data.frame() %>%
  group_by(Period, City) %>%
  dplyr::summarize(MeanCount = mean(ndaily, na.rm=TRUE)) %>%
  data.frame()


Tweetbyprdcity <- twwise_period %>%
  mutate (compound = positive - negative) %>%
  group_by(Period, City) %>%
  dplyr::summarize(Mean = mean(compound, na.rm=TRUE), sd = sd(compound, na.rm=TRUE)) %>% #, ncount = length(compound), ndate = length(unique(Date))) %>%
  data.frame() %>%
  mutate(MeanCount = Tweetbyprdcity_daily$MeanCount) %>%
  mutate(City = factor(City, levels = c("New_York","Los_Angeles","Seattle","Chicago"), 
                       labels = (c("New York","Los Angeles","Seattle","Chicago")))  )
  


Tweetbyprdcity_long <- gather(Tweetbyprdcity, measure, value, Mean:MeanCount, factor_key=TRUE)

Tweetbyprdcity_wide <- spread(Tweetbyprdcity_long, City, value)


xtable(Tweetbyprdcity_wide)


xtable(Tweetbyprdcity_wide)

# 3. Other Topics - Example Vader -------------------------------------------------------

wordlist <- c("mask", "lockdown", "vaccine")
wordlist2 <- c("COVID19", "mask", "lockdown", "vaccine")
wordlist3 <- c("", "mask+", "lockdown+", "vaccine+")


DataAll <- NULL

for (j in 1:length(wordlist)){
  CompData <- NULL
  
  for (i in 1:8){
    cityi <- cityfull[i]
    data <- read.csv(paste0("data/",cityi,"/",wordlist[j],"+Summary_",cityi,"_robertascored.csv"))
    data_long <- data %>%
      mutate( compound = positive - negative) %>%
      select(c("Date","city","compound"))
    CompData <- rbind(CompData, data_long)
  }
  
  CompData$Date <- as.Date(CompData$Date)
  
  CompData$Country <- rep(c("Canada","US"), each = dim (CompData)[1]/2)
  
  CompData$city <- factor(CompData$city, levels = rev(cityfull), 
                          labels = rev(c("Toronto","Vancouver","Montreal","Calgary","New York","Los Angeles","Seattle","Chicago")))
  
  
  CompData$Topic <- wordlist[j]
  
  DataAll <- rbind(DataAll, CompData)
  
  }
  

library(ggplot2)
  
  
  
pdf(file = paste0("output/Sentimentline_topics.pdf"), height = 14, width = 18)
# lineplot

HeatSentiment <- ggplot(DataAll) +
  geom_line(aes(x=Date, y=compound, color = Topic), size=1, alpha= 0.8) +
  scale_color_manual(values = c( "#EAAE37", "#2E8289", "#B4674E"))  +
  facet_grid(city~ .)+
  labs(y = "Sentiment\n Score", color = "City") +
  scale_x_date(breaks = breaks_pretty(8))   +
  geom_hline(yintercept=0, linetype="dashed", color = "#565F41") +
  theme_bw()  +
  theme(text=element_text(size=22), axis.text = element_text(size = 20), panel.spacing = unit(1, "lines")) +
  theme(strip.background =element_rect(fill="white",color="#3F4536"))+ # #535b44
  theme(strip.text = element_text(colour = '#3F4536')) +
  theme(panel.border = element_rect(colour = "#3F4536"))
print(HeatSentiment)
dev.off()

  
  # CompData_both_IF <- CompData %>% mutate(Measure = "VADERSenti") %>% select(c(Date, Measure, city, compound))
  # 
  # CompData_both_IF_wide <- spread(CompData_both_IF,city, compound)
  # 
  # write.csv(CompData_both_IF_wide,file=paste0("output/VADERSenti", wordlist[j],".csv"))
  


pdf(file = paste0("output/Sentimentlineplots.pdf"), height = 14, width = 18)
# lineplot

HeatSentiment <- ggplot(CompDataBoth) +
  geom_line(aes(x=Date, y=compound, color = Method), size=1, alpha= 0.8) +
  scale_color_manual(values = c( "#EAAE37", "#2E8289"))  +
  facet_grid(city~ .)+
  labs(y = "Sentiment\n Score", color = "City") +
  scale_x_date(breaks = breaks_pretty(8))   +
  geom_hline(yintercept=0, linetype="dashed", color = "#565F41") +
  theme_bw()  +
  theme(text=element_text(size=22), axis.text = element_text(size = 20), panel.spacing = unit(1, "lines")) +
  theme(strip.background =element_rect(fill="white",color="#3F4536"))+ # #535b44
  theme(strip.text = element_text(colour = '#3F4536')) +
  theme(panel.border = element_rect(colour = "#3F4536"))
print(HeatSentiment)
dev.off()


# ------------- Tweetwise Summary by new keywords -------



start <- as.Date("24-02-20",format="%d-%m-%y")
end   <- as.Date("14-10-20",format="%d-%m-%y")

tweetall <- NULL

for (j in 1:length(wordlist3)){
  tweetwisedata <- NULL
  theDate <- start
  
  for (i in 1:4){
    cityi <- cityfull[i]
    cat(paste0(cityi,"\n"))
    theDate <- start
    while (theDate <= end)
    { cat(paste0(theDate,"\n"))
      data <- read.csv(paste0("code/SentTwi/",cityi,"/",wordlist3[j],cityi,"_sentiscore_",format(theDate, "%y-%m-%d"),".csv"))
      if (dim(data)[1]!=0) {
        data_long <- data[,c(27:29)] %>% 
          mutate(Date= format(theDate, "%y-%m-%d"))  %>%
          mutate(City = cityi)
        tweetwisedata <- rbind(tweetwisedata, data_long)
      }
      theDate <- theDate +1
    }
  }
  
  
  save(tweetwisedata,file= paste0("output/tweetlongtablesentiment", wordlist2[j],".RData"))
  # load(file= "output/tweetlongtablesentiment.RData")
  
  
  tweetwisedata$Date <- as.Date(tweetwisedata$Date)
  
  # CompData$Country <- rep(c("Canada","US"), each = dim (CompData)[1]/2)
  
  tweetwisedata$city <- factor(tweetwisedata$City, levels = c("New_York","Los_Angeles","Seattle","Chicago"), 
                               labels = (c("New York","Los Angeles","Seattle","Chicago")))
  
  # tweetlongtable <- tweetlongtable %>%
  #   filter(!Mood %in% c("positive","negative"))
  # tweetlongtable$Mood <- factor(tweetlongtable$Mood, levels = c("joy", "trust", "anticip", "surprise", "fear", "sadness", "disgust",  "anger"), 
  #                               labels = c("joy", "trust", "anticipation", "surprise", "fear", "sadness", "disgust",  "anger"))
  
  # tweetlongtable <- tweetlongtable %>%
  #   mutate(direction = case_when(
  #     Mood =="joy" | "trust"| "anticip"| "surprise" ~ "Positive",
  #     Mood =="fear"| "sadness"| "disgust"|  "anger" ~ "Negative"))
  
  twwise_period <- tweetwisedata %>% 
    mutate(Period = case_when(
      City == "New_York" & Date <= as.Date("0020-03-07") ~ "Period 1",
      City == "New_York" & Date > as.Date("0020-03-07") &  Date<= as.Date("0020-06-08") ~ "Period 2",
      City == "New_York" & Date > as.Date("0020-06-08") ~ "Period 3",
      City == "Los_Angeles" & Date <= as.Date("0020-03-04") ~ "Period 1",
      City == "Los_Angeles" & Date > as.Date("0020-03-04") &  Date<= as.Date("0020-05-07") ~ "Period 2",
      City == "Los_Angeles" & Date > as.Date("0020-05-07") ~ "Period 3",
      City == "Seattle" & Date <= as.Date("0020-02-29") ~ "Period 1",
      City == "Seattle" & Date > as.Date("0020-02-29") &  Date <= as.Date("0020-05-01") ~ "Period 2",
      City == "Seattle" & Date > as.Date("0020-05-01") ~ "Period 3",
      City == "Chicago" & Date <= as.Date("0020-03-09") ~ "Period 1",
      City == "Chicago" & Date > as.Date("0020-03-09") &  Date<= as.Date("0020-05-05") ~ "Period 2",
      City == "Chicago" & Date >as.Date("0020-05-05") ~ "Period 3")) 
  
  Tweetbyprdcity <- twwise_period %>%
    group_by(Period, city) %>%
    dplyr::summarize(Mean = mean(compound, na.rm=TRUE), sd = sd(compound, na.rm=TRUE), ncount = length(compound))
  
  
  Tweetbyprdcity_long <- gather(Tweetbyprdcity, measure, value, Mean:ncount, factor_key=TRUE)
  
  Tweetbyprdcity_wide <- spread(Tweetbyprdcity_long, City, value)
  
  
  tweetall <- rbind(tweetall, Tweetbyprdcity_wide)
  
  
}



xtable(tweetall)




# CompData <- NULL
# 
# for (j in 1:length(wordlist)){
#   
#   
#   if (j==1) {
#     datanow <- read.csv(paste0("output/VADERSenti.csv"))
#   } else{ 
#     datanow <- read.csv(paste0("output/VADERSenti", wordlist[j],".csv"))
#   }
#   
#   datanow$keyword <- wordlist2[j]
#   
#   CompData <- rbind(CompData, datanow)
# }
