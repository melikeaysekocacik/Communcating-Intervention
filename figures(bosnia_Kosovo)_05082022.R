###########################################################################
#Figures of Communicating Intervention: Theoretical and 
#Empirical Investigation on Third Party Statements for Intrastate Conflicts
###########################################################################


#libraries used#####
library(readxl)
library(dplyr)
library(quanteda)
library(stringi)
library(tm)
library(SnowballC)
library(quanteda)
library(stringi)
library(tidyverse)
library(udpipe)
library(ggplot2)
library(cowplot)
#The main data is in the form of actor-date and the same statements which include more than one actors are introduced as different observations.
setwd("C:/Users/melikeaysekocacik/Dropbox/My PC (LAPTOP-I12HGKJ4)/Desktop/Political Communication Paper Submission/Data-github")
domestic<- read_excel("maindata(bosnia_kosovo)_05082022.xlsx")
#View(main_data)

domestic$date2<-format(as.Date(nsa$mdate, format="%Y-/%m-/%d"),"%Y")
domestic$date2<-toString(nsa$date2)
domestic$speechText<-as.character(domestic$speechText)
domestic$text<-stri_enc_toutf8(domestic$speechText, is_unknown_8bit = FALSE, validate = FALSE)
dom_corpus<-corpus(domestic, text_field = "speechText", metacorpus = NULL, compress = FALSE)

#RAKE figures for different actors #####

ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)


bosnia<- subset(domestic, cname=="Bosnia")
bosnia$speechText<-as.character(bosnia$speechText)
bosnia$text<-stri_enc_toutf8(bosnia$speechText, is_unknown_8bit = FALSE, validate = FALSE)
x_bosnia <- udpipe_annotate(ud_model, x = bosnia$text, doc_id = bosnia$actor_num)
x_bosnia <- as.data.frame(x_bosnia)
stats_bosnia <- keywords_rake(x = x_bosnia, term = "lemma", group = "doc_id", 
                               relevant = x_bosnia$upos %in% c("NOUN", "ADJ"))
stats_bosnia$key <- factor(stats_bosnia$keyword, levels = rev(stats_bosnia$keyword))
stats_bosnia<-stats_bosnia[1:10, ]

bosnia<-ggplot(data = stats_bosnia, aes(x = nrow(stats_bosnia):1, y = rake)) +
  geom_point() +
  coord_flip() +
  ggtitle("Bosnia") +
  scale_x_continuous(breaks = nrow(stats_bosnia):1,
                     labels = stats_bosnia$key) +
  labs(x = NULL, y = "RAKE")

bosnian<- subset(domestic, nagname=="Bosnian Muslims")
bosnian$speechText<-as.character(bosnian$speechText)
bosnian$text<-stri_enc_toutf8(bosnian$speechText, is_unknown_8bit = FALSE, validate = FALSE)
x_bosnian <- udpipe_annotate(ud_model, x = bosnian$text, doc_id = bosnian$actor_num)
x_bosnian<- as.data.frame(x_bosnian)
stats_bosnian <- keywords_rake(x = x_bosnian, term = "lemma", group = "doc_id", 
                       relevant = x_bosnian$upos %in% c("NOUN", "ADJ"))
stats_bosnian$key <- factor(stats_bosnian$keyword, levels = rev(stats_bosnian$keyword))
stats_bosnian<-stats_bosnian[1:10, ]

bosniam<-ggplot(data = stats_bosnian, aes(x = nrow(stats_bosnian):1, y = rake)) +
  geom_point() +
  coord_flip() +
  ggtitle("Bosnian Muslims") +
  scale_x_continuous(breaks = nrow(stats_bosnian):1,
                     labels = stats_bosnian$key) +
  labs(x = NULL, y = "RAKE")

bosnianc<- subset(domestic, nagname=="Bosnian Croats")
bosnianc$speechText<-as.character(bosnianc$speechText)
bosnianc$text<-stri_enc_toutf8(bosnianc$speechText, is_unknown_8bit = FALSE, validate = FALSE)
x_bosnianc <- udpipe_annotate(ud_model, x = bosnianc$text, doc_id = bosnianc$actor_num)
x_bosnianc<- as.data.frame(x_bosnianc)
stats_bosnianc <- keywords_rake(x = x_bosnianc, term = "lemma", group = "doc_id", 
                               relevant = x_bosnianc$upos %in% c("NOUN", "ADJ"))
stats_bosnianc$key <- factor(stats_bosnianc$keyword, levels = rev(stats_bosnianc$keyword))
stats_bosnianc<-stats_bosnianc[1:10, ]

bosniac<-ggplot(data = stats_bosnianc, aes(x = nrow(stats_bosnianc):1, y = rake)) +
  geom_point() +
  coord_flip() +
  ggtitle("Bosnian Croats") +
  scale_x_continuous(breaks = nrow(stats_bosnianc):1,
                     labels = stats_bosnianc$key) +
  labs(x = NULL, y = "RAKE")

bosnians<- subset(domestic, nagname=="Bosnian Serbs")
bosnians$speechText<-as.character(bosnians$speechText)
bosnians$text<-stri_enc_toutf8(bosnians$speechText, is_unknown_8bit = FALSE, validate = FALSE)
x_bosnians <- udpipe_annotate(ud_model, x = bosnians$text, doc_id = bosnians$actor_num)
x_bosnians<- as.data.frame(x_bosnians)
stats_bosnians <- keywords_rake(x = x_bosnians, term = "lemma", group = "doc_id", 
                               relevant = x_bosnians$upos %in% c("NOUN", "ADJ"))
stats_bosnians$key <- factor(stats_bosnians$keyword, levels = rev(stats_bosnians$keyword))
stats_bosnians<-stats_bosnians[1:10, ]

bosnias<-ggplot(data = stats_bosnians, aes(x = nrow(stats_bosnians):1, y = rake)) +
  geom_point() +
  coord_flip() +
  ggtitle("Bosnian Serbs") +
  scale_x_continuous(breaks = nrow(stats_bosnians):1,
                     labels = stats_bosnians$key) +
  labs(x = NULL, y = "RAKE")


plot_grid(bosnia, bosniam, bosniac, bosnias, ncol=2)


#Kosovar conflict - RAKE figures

serbia<- subset(domestic, cname=="Serbia")
serbia$speechText<-as.character(serbia$speechText)
serbia$text<-stri_enc_toutf8(serbia$speechText, is_unknown_8bit = FALSE, validate = FALSE)
x_serbia <- udpipe_annotate(ud_model, x = serbia$text, doc_id = serbia$actor_num)
x_serbia<- as.data.frame(x_serbia)
stats_serbia <- keywords_rake(x = x_serbia, term = "lemma", group = "doc_id", 
                          relevant = x_serbia$upos %in% c("NOUN", "ADJ"))
stats_serbia$key <- factor(stats_serbia$keyword, levels = rev(stats_serbia$keyword))
stats_serbia<-stats_serbia[1:10, ]

serbia<-ggplot(data = stats_serbia, aes(x = nrow(stats_serbia):1, y = rake)) +
  geom_point() +
  coord_flip() +
  ggtitle("Serbia") +
  scale_x_continuous(breaks = nrow(stats_serbia):1,
                     labels = stats_serbia$key) +
  labs(x = NULL, y = "RAKE")

kosovo<- subset(domestic, nagname=="Kosovar Albanians")
kosovo$speechText<-as.character(kosovo$speechText)
kosovo$text<-stri_enc_toutf8(kosovo$speechText, is_unknown_8bit = FALSE, validate = FALSE)
x_kosovo<- udpipe_annotate(ud_model, x = kosovo$text, doc_id = kosovo$actor_num)
x_kosovo<- as.data.frame(x_kosovo)
stats_kosovo <- keywords_rake(x = x_kosovo, term = "lemma", group = "doc_id", 
                           relevant = x_kosovo$upos %in% c("NOUN", "ADJ"))
stats_kosovo$key <- factor(stats_kosovo$keyword, levels = rev(stats_kosovo$keyword))
stats_kosovo<-stats_kosovo[1:10, ]

kosovo<-ggplot(data = stats_kosovo, aes(x = nrow(stats_kosovo):1, y = rake)) +
  geom_point() +
  coord_flip() +
  ggtitle("Kosovar Albanians") +
  scale_x_continuous(breaks = nrow(stats_kosovo):1,
                     labels = stats_kosovo$key) +
  labs(x = NULL, y = "RAKE")

plot_grid(serbia, kosovo, ncol=2)


#Kosovo and Bosnian civilians

kosovoc<- subset(domestic, nagname=="Kosovar people")
kosovoc$speechText<-as.character(kosovoc$speechText)
kosovoc$text<-stri_enc_toutf8(kosovoc$speechText, is_unknown_8bit = FALSE, validate = FALSE)
x_kosovoc<- udpipe_annotate(ud_model, x = kosovoc$text, doc_id = kosovoc$actor_num)
x_kosovoc<- as.data.frame(x_kosovoc)
stats_kosovoc <- keywords_rake(x = x_kosovoc, term = "lemma", group = "doc_id", 
                              relevant = x_kosovoc$upos %in% c("NOUN", "ADJ"))
stats_kosovoc$key <- factor(stats_kosovoc$keyword, levels = rev(stats_kosovoc$keyword))
stats_kosovoc<-stats_kosovoc[1:10, ]

kosovoc<-ggplot(data = stats_kosovoc, aes(x = nrow(stats_kosovoc):1, y = rake)) +
  geom_point() +
  coord_flip() +
  ggtitle("Kosovar Civilians") +
  scale_x_continuous(breaks = nrow(stats_kosovoc):1,
                     labels = stats_kosovoc$key) +
  labs(x = NULL, y = "RAKE")

bosniac<- subset(domestic, nagname=="Bosnian people")
bosniac$speechText<-as.character(bosniac$speechText)
bosniac$text<-stri_enc_toutf8(bosniac$speechText, is_unknown_8bit = FALSE, validate = FALSE)
x_bosniac<- udpipe_annotate(ud_model, x = bosniac$text, doc_id = bosniac$actor_num)
x_bosniac<- as.data.frame(x_bosniac)
stats_bosniac <- keywords_rake(x = x_bosniac, term = "lemma", group = "doc_id", 
                               relevant = x_bosniac$upos %in% c("NOUN", "ADJ"))
stats_bosniac$key <- factor(stats_bosniac$keyword, levels = rev(stats_bosniac$keyword))
stats_bosniac<-stats_bosniac[1:10, ]

bosniac<-ggplot(data = stats_bosniac, aes(x = nrow(stats_bosniac):1, y = rake)) +
  geom_point() +
  coord_flip() +
  ggtitle("Bosnian Civilians") +
  scale_x_continuous(breaks = nrow(stats_bosniac):1,
                     labels = stats_bosniac$key) +
  labs(x = NULL, y = "RAKE")

plot_grid(kosovoc, bosniac, ncol=2)


