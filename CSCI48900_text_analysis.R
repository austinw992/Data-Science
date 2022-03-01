library(tidytext)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(textdata)

setwd("C:\\Users\\Panda\\Desktop\\CSCI48900 Data Science Data Sheets")

songs=read.csv("C:\\Users\\Panda\\Desktop\\CSCI48900 Data Science Data Sheets\\songdata.csv")
songs$text=as.character(songs$text)
songs$song=as.character(songs$song)
songs$artist=as.character(songs$artist)


#convert into tidy format data frame
songs=as_tibble(songs)
song_text <- songs %>% dplyr::select(lyrics = text, song, artist)

#need to analyze words...un-nest
song_words = songs %>%
  unnest_tokens(word, text)

#reduce dimension and keep informative words by removing stop words like the
data(stop_words)

song_words_no_stop <- song_words %>%
  anti_join(stop_words)

#now we can maybe start analyzing this data

# song frequency by artist in dataset
songs %>% 
  dplyr::count(artist,sort=T) %>% 
  print(n = 20)

# frequent words by Nirvana
song_words_no_stop %>% 
  filter(word=="love") %>%
  dplyr::count(artist,sort=T) %>% 
  print(n = 20)

# frequency of word by artist
frequency = song_words_no_stop %>% dplyr::count(artist, word) %>%
  group_by(artist) %>%
  mutate(proportion = n / sum(n)) %>% 
  filter(artist %in% c("Nirvana","Taylor Swift")) %>%
  dplyr::select(-n) %>%
  spread(artist,proportion) 

# plot freq of nirvana words against swift
frequency[is.na(frequency)]=0
names(frequency)[3]="Swift"
ggplot(frequency,aes(x=Nirvana,y=Swift)) + 
  geom_abline()+
  geom_text(aes(label=word),check_overlap=T)+
  scale_x_log10() +
  scale_y_log10() 

# sentiments of words
song_words_sentiment <- song_words_no_stop %>%
  inner_join(get_sentiments("afinn")) 

# average over artist
song_words_sentiment %>%
  group_by(artist) %>%
  dplyr::summarise(avg_sentiment = mean(value)) %>%
  arrange(desc(avg_sentiment)) %>% 
  print(n=20)

song_words_no_stop %>% dplyr::count(word,artist) %>% group_by(artist) %>% top_n(n=20) %>% 
 arrange(desc(artist)) %>%  print(n=40) 
  


song_words_no_stop %>% group_by(artist) %>% dplyr::count("love", sort=T)
