
# 0. Global Parameters -------------------------------------------------------



library(dplyr)
library(tidyr)
library(ggplot2)
require(ggthemes)


citylist <- c("Toronto","Vancouver","Montreal","Calgary")
provlist <- c("Ontario","British Columbia","Quebec","Alberta")

UScitylist <- c("New_York","Los_Angeles","Seattle","Chicago")
USstatlist <- c("New York","California","Washington","Illinois")

varlist <- c("Infected_Cases", "Like", "Reply", "Retweet", "Tweets", "robertaSenti") # "VADERSenti")
varofficial <- c("Infected Cases", "Like", "Reply", "Retweet", "Tweets",  "Sentiment")
varlist2 <- c("Infected_Cases", "VADERSenti", "VADERSentilockdown", "VADERSentimask", "VADERSentivaccine")
varofficial2 <- c("Infected Cases", "Sentiment COVID-19", "Sentiment lockdown", "Sentiment mask", "Sentiment vaccine")

# 1 Canadian Statistics -------------------------------------------------

Casenumber <- read.csv("data/CanadaData_prov.csv")


CompData <- gather(Casenumber[,c("Data","ON","BC","AB","QB")], Prov, Count, ON, BC, AB, QB)


CompData <- CompData %>% 
            mutate(Province = case_when(
                   Prov=="ON" ~ "Ontario",
                   Prov=="BC" ~ "British Columbia",
                   Prov=="AB" ~ "Alberta",
                   Prov=="QB" ~ "Quebec")) %>% 
            mutate(city = case_when(
                   Prov=="ON" ~ "Toronto",
                   Prov=="BC" ~ "Vancouver",
                   Prov=="AB" ~ "Calgary",
                   Prov=="QB" ~ "Montreal")) %>% 
            mutate(Measure="Infected") %>% 
            mutate(Date=Data) %>% select(c(Date, city, Measure, Count, Province))




for (i in 1:4){
  cityi <- citylist[i]
  data <- read.csv(paste0("data/",cityi,"/","Summary_",cityi,".csv"))
  data_long <- gather(data[,2:9], Measure, Count,ntweet:interactcount) %>% 
     mutate(Province=provlist[i])
  CompData <- rbind(CompData, data_long)
}

CompData$Date <- as.Date(CompData$Date)

# CompData <- CompData %>% filter(!Measure %in% c("quotecoun","interactcount"))
CompData_s <- CompData %>% filter(Measure %in% c("Infected","ntweet","likecount","replycount", "retweetcount")) # %>%
  # mutate(Count=ifelse(Measure=="likecount",Count/10,Count))

CompData_s$city <- factor(CompData_s$city, levels = c("Toronto", "Vancouver", "Montreal","Calgary"))
CompData_s$Measure <- factor(CompData_s$Measure, levels = c("Infected","ntweet","likecount","replycount", "retweetcount"),
                             labels = c("Infected","Tweets","Like","Reply", "Retweet"))


totaln <- sum(CompData_s %>% filter(Measure=="Tweets") %>% select("Count"))

pdf(file = "output/descriptiveplot.pdf",height = 12, width = 16)


AllReportD <- ggplot(CompData_s)  +
  geom_line(aes(x=Date, y=Count, color = Measure), size=1, alpha= 0.8)+
  # geom_point(aes(x=Date, y=Count, color = Measure, shape= Measure),size=2) + 
  scale_color_manual(values = c("#565F41", "#EAAE37", "#B4674E", "#2E8289", "#91D5DE"))  +
  # scale_shape_manual(values = c(15, 17, 19))  +
  labs(x = "Date", y = "Count") +
  theme(text=element_text(size=13, family="mono"))+
  # scale_alpha_manual(values=c(0.7, 0.5),
  #                    guide=guide_legend(override.aes = list(colour=c("#6D882B","#150718")))) +
  # scale_linetype_manual(values=c(3, 5),guide = FALSE) +
  # labs(color = "Type of\n Measure", shape = "Type of\n Measure") +
  facet_grid(Measure~city, scales = "free_y")  +
  # coord_cartesian(ylim=c(-0.2,1.5))+
  scale_y_continuous(trans='log10') + 
  theme_bw()  +
  theme(text=element_text(size=30), axis.text = element_text(size = 20), panel.spacing = unit(1, "lines")) +
  theme(strip.background =element_rect(fill='white',color="#3F4536"))+ # #535b44
  theme(strip.text = element_text(colour = '#3F4536')) +
  theme(panel.border = element_rect(colour = "#3F4536"))
# scale_fill_gradient(low = "#FDEDEC", high = "#B03A2E", na.value = NA,limits = c(0,3.5))
# scale_color_manual(values =c("#A8CDEC", "#F6955E", "#682C37"))

AllReportD


dev.off()



CompData_CA <- CompData_s


# 2 US Statistics -------------------------------------------------

Casenumber <- read.csv("data/all-states-history.csv")


CompData <- Casenumber %>% filter(state %in% c("NY","CA","IL","WA")) %>%
  mutate(Date=as.Date(date))    %>%
  mutate(Province = case_when(
    state =="NY" ~ "New York",
    state  =="CA" ~ "California",
    state =="IL" ~ "Illinois",
    state =="WA" ~ "Washington")) %>%
  mutate(Measure = "Infected") %>%
  mutate(Count = positiveIncrease) %>%
  mutate(city = case_when(
    state =="NY" ~ "New York",
    state =="CA" ~ "Los Angeles",
    state =="IL" ~ "Chicago",
    state =="WA" ~ "Seattle")) %>%
  select(c(Date, city, Measure, Count, Province)) %>%
  filter(Date >= as.Date("2020-02-24") & Date <= as.Date("2020-10-14")) %>%
  complete(Date,nesting(city, Measure, Province)) %>%
  mutate(Count = ifelse(is.na(Count),0,Count)) %>%
  arrange(Date)


 


for (i in 1:4){
  cityi <- UScitylist[i]
  data <- read.csv(paste0("data/",cityi,"/","Summary_",cityi,".csv"))
  data_long <- gather(data[,2:9], Measure, Count,ntweet:interactcount) %>% 
    mutate(Province=USstatlist[i])
  CompData <- rbind(CompData, data_long)
}

CompData <- CompData %>%
  mutate(Date = as.Date(CompData$Date)) %>%
  mutate(city = case_when(
    city == "New_York" ~ "New York",
    city == "Los_Angeles" ~ "Los Angeles",
    TRUE ~ city))

# CompData <- CompData %>% filter(!Measure %in% c("quotecoun","interactcount"))
CompData_s <- CompData %>% filter(Measure %in% c("Infected","ntweet","likecount","replycount", "retweetcount")) #%>%
  # mutate(Count=ifelse(Measure=="likecount",Count,Count))

CompData_s$city <- factor(CompData_s$city, levels = c("New York","Los Angeles","Seattle","Chicago"))
CompData_s$Measure <- factor(CompData_s$Measure, levels = c("Infected","ntweet","likecount","replycount", "retweetcount"),
                             labels = c("Infected","Tweets","Like","Reply", "Retweet"))


totaln <- sum(CompData_s %>% filter(Measure=="Tweets") %>% select("Count"))


pdf(file = "output/descriptiveplotUS.pdf",height = 12, width = 16)


AllReportD <- ggplot(CompData_s)  +
  geom_line(aes(x=Date, y=Count, color = Measure), size=1, alpha= 0.8)+
  scale_color_manual(values = c("#565F41", "#EAAE37", "#B4674E", "#2E8289", "#91D5DE"))  +
  # scale_color_manual(values = c("#C4878C", "#469BEC","#6D882B"))  +
  # scale_shape_manual(values = c(15, 17, 19))  +
  labs(x = "Date", y = "Count") +
  theme(text=element_text(size=13, family="mono"))+
  # scale_alpha_manual(values=c(0.7, 0.5),
  #                    guide=guide_legend(override.aes = list(colour=c("#6D882B","#150718")))) +
  # scale_linetype_manual(values=c(3, 5),guide = FALSE) +
  # labs(color = "Type of\n Measure", shape = "Type of\n Measure") +
  facet_grid(Measure~city, scales = "free_y")  +
  scale_y_continuous(trans='log10') + 
  # coord_cartesian(ylim=c(-0.2,1.5))+
  theme_bw()  +
  theme(text=element_text(size=30), axis.text = element_text(size = 20), panel.spacing = unit(1, "lines")) +
  theme(strip.background =element_rect(fill='white',color="#3F4536"))+ # #535b44
  theme(strip.text = element_text(colour = '#3F4536')) +
  theme(panel.border = element_rect(colour = "#3F4536"))

# scale_fill_gradient(low = "#FDEDEC", high = "#B03A2E", na.value = NA,limits = c(0,3.5))
# scale_color_manual(values =c("#A8CDEC", "#F6955E", "#682C37"))

AllReportD


dev.off()


CompData_US <- CompData_s

# 3 Prepare a wide table for Python analysis -------------------------------------------------

CompData_both <- rbind(CompData_CA, CompData_US)

CompData_both_IF <- CompData_both %>% filter(Measure == "Infected Cases") %>% select(c(Date, Measure, city, Count))

CompData_both_IF_wide <- spread(CompData_both_IF,city, Count)

write.csv(CompData_both_IF_wide,file="output/Infected_Cases.csv")


CompData_both_IF <- CompData_both %>% filter(Measure == "Tweets") %>% select(c(Date, Measure, city, Count))

CompData_both_IF_wide <- spread(CompData_both_IF,city, Count)

write.csv(CompData_both_IF_wide,file="output/Tweets.csv")


CompData_both_IF <- CompData_both %>% filter(Measure == "Like") %>% select(c(Date, Measure, city, Count))

CompData_both_IF_wide <- spread(CompData_both_IF,city, Count)

write.csv(CompData_both_IF_wide,file="output/Like.csv")


CompData_both_IF <- CompData_both %>% filter(Measure == "Reply") %>% select(c(Date, Measure, city, Count))

CompData_both_IF_wide <- spread(CompData_both_IF,city, Count)

write.csv(CompData_both_IF_wide,file="output/Reply.csv")


CompData_both_IF <- CompData_both %>% filter(Measure == "Retweet") %>% select(c(Date, Measure, city, Count))

CompData_both_IF_wide <- spread(CompData_both_IF,city, Count)

write.csv(CompData_both_IF_wide,file="output/Retweet.csv")



# 4 - Plot the tau plots (cross-mapping) example ---Study 1 -----------------------------
# itt <- read.csv("output/rhorecordinfecttotweetcount.csv")
# head(itt)
# 
# itt$direction <- "X:infected cases Y:tweet count"
# 
# maxitt.index <- which(itt$rho==max(itt$rho))
# itt$maxx <- itt$tau[maxitt.index]
# 
# tti <- read.csv("output/rhorecordtweetcounttoinfect.csv")
# head(tti)
# 
# tti$direction <- "X:tweet count Y:infected cases"
# 
# maxtti.index <- which(tti$rho==max(tti$rho))
# tti$maxx <- tti$tau[maxtti.index]
# 
# 
# data <- rbind(itt, tti)
# 
# library(ggplot2)
# # Basic line plot with points
# 
# pdf(file = "output/tauplot.pdf",height = 6, width = 12)
# 
# p1 <- ggplot(data=data, aes(x=tau, y=rho)) +
#   geom_vline(aes(xintercept =maxx), linetype="dashed", size = 1, color = "#ABB2B9") +
#          geom_line(aes(color=direction), size = 2, show.legend = FALSE) +
#       facet_grid(.~ direction, scales = "free",  space = "free") +
#       scale_color_manual(values=c("#2F7F86","#E9AA38"))+
#   labs(x = expression(tau), y = expression(rho)) +
#   theme_bw() +
#   theme(text=element_text(size=20, family="mono"), axis.text = element_text(size = 12, family="mono"), panel.spacing = unit(1, "lines"))
# 
# p1
# 
# dev.off()




for (i in 1:(length(varlist)-1)){
  for (j in (i+1):length(varlist)) {#(i+1):length(varlist)) {
    itt <- read.csv(paste0("output/causalresults/rho",varlist[i],"_to_",varlist[j],".csv"))
    head(itt)
    
    itt$direction <- paste0("Direction 1: X (",varofficial[i],") <- Y (",varofficial[j],")")
    
    maxitt.index <- which(itt$rho==max(itt$rho))
    itt$maxx <- itt$tau[maxitt.index]
    itt$maxxlabel <- paste0("Optimal lag: ", itt$maxx)
    itt$annotatey <- quantile(itt$rho, probs = 0.25)
    
    tti <- read.csv(paste0("output/causalresults/rho",varlist[j],"_to_",varlist[i],".csv"))
    head(tti)
    
    tti$direction <- paste0("Direction 2: Y (",varofficial[j],") <- X (",varofficial[i],")")
    
    maxtti.index <- which(tti$rho==max(tti$rho))
    tti$maxx <- tti$tau[maxtti.index]
    tti$maxxlabel <- paste0("Optimal lag: ", tti$maxx)
    tti$annotatey <- quantile(tti$rho, probs = 0.25)
    
    data <- rbind(itt, tti)
    
   
    # Basic line plot with points
    
    
    pdf(file = paste0("output/causalresults/tauplot_", varlist[i], "_and_", varlist[j],".pdf"), height = 4, width = 12)
    
    p1 <- ggplot(data=data, aes(x=tau, y=rho)) +
      geom_vline(aes(xintercept = maxx), linetype="dashed", size = 1, color = "#ABB2B9") + #"#ABB2B9",color=
      geom_line(aes(color=direction), size = 2, show.legend = FALSE ) +
      facet_grid(.~ direction, scales = "free",  space = "free") +
      scale_color_manual(values=c("#2F7F86","#b4674e")) + ##b4674e
      labs(x = expression(tau), y = expression(rho)) +
      geom_label(aes(label=maxxlabel, y=annotatey), x =0) +
      theme_bw() +
      theme(text=element_text(size=23), axis.text = element_text(size = 20, family="mono"), panel.spacing = unit(1, "lines")) +
      theme(strip.background =element_rect(fill="white",color="#3F4536"))+ # #535b44
      theme(strip.text = element_text(colour = '#3F4536')) #+
      #theme(panel.border = element_rect(colour = "#3F4536"))
    
    print(p1)
    
    dev.off()
  }
}




# 5 - Plot the tau plots (cross-mapping) example ---Study 2 -----------------------------

for (i in 1:(length(varlist2)-1)){
  for (j in (i+1):length(varlist2)) {
    itt <- read.csv(paste0("output/causalresults/rho",varlist2[i],"_to_",varlist2[j],".csv"))
    head(itt)
    
    itt$direction <- paste0("X:",varofficial2[i]," Y:",varofficial2[j])
    
    maxitt.index <- which(itt$rho==max(itt$rho))
    itt$maxx <- itt$tau[maxitt.index]
    itt$maxxlabel <- paste0("Optimal lag: ", itt$maxx)
    itt$annotatey <- quantile(itt$rho, probs = 0.25)
    
    tti <- read.csv(paste0("output/causalresults/rho",varlist2[j],"_to_",varlist2[i],".csv"))
    head(tti)
    
    tti$direction <- paste0("X:",varofficial2[j]," Y:",varofficial2[i])
    
    maxtti.index <- which(tti$rho==max(tti$rho))
    tti$maxx <- tti$tau[maxtti.index]
    tti$maxxlabel <- paste0("Optimal lag: ", tti$maxx)
    tti$annotatey <- quantile(tti$rho, probs = 0.25)
    
    data <- rbind(itt, tti)
    
    
    # Basic line plot with points
    
    
    pdf(file = paste("output/causalresults/tauplot", varlist2[i], "and", varlist2[j],".pdf"), height = 4, width = 12)
    
    p1 <- ggplot(data=data, aes(x=tau, y=rho)) +
      geom_vline(aes(xintercept = maxx), linetype="dashed", size = 1, color = "#ABB2B9") + #"#ABB2B9",color=
      geom_line(aes(color=direction), size = 2, show.legend = FALSE ) +
      facet_grid(.~ direction, scales = "free",  space = "free") +
      scale_color_manual(values=c("#2F7F86","#b4674e")) + ##b4674e
      labs(x = expression(tau), y = expression(rho)) +
      geom_label(aes(label=maxxlabel, y=annotatey), x =0) +
      theme_bw() +
      theme(text=element_text(size=20, family="mono"), axis.text = element_text(size = 12, family="mono"), panel.spacing = unit(1, "lines")) +
      theme(strip.background =element_rect(fill="#3F4536",color="#3F4536"))+ # #535b44
      theme(strip.text = element_text(colour = 'white')) +
      theme(panel.border = element_rect(colour = "#3F4536"))
    
    print(p1)
    
    dev.off()
  }
}


