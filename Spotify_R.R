library(ggplot2)
library(tidyverse)
library(wordcloud2)
library(tm)
library(dplyr)
library(pROC)
library(reshape2)
library(ggbiplot)
library(textdata)
library(tidytext)
library(wordcloud)
library(nnet) 
library(caret)
library(randomForest)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

#importing the spotify dataset
spotify <- read.csv("Spotify.csv")
View(spotify)

#filter out tracks with 0 popularity
filterd_spotify <- spotify %>% filter(popularity != 0)
filterd_spotify
View(filterd_spotify)
spotify$time_signature

ggplot(filterd_spotify, aes(x = valence, y = popularity, colour = track_genre)) +
  geom_point()
#way too many catagories, need to filter out some of the genres or see if any are correlated some other way

#creating a heatmap of each of the varibales to see whoch to compare
heat <- filterd_spotify[, c("popularity", "duration_ms", "danceability", 
                            "energy", "loudness", "mode", "speechiness", 
                            "acousticness", "instrumentalness", "liveness", 
                            "valence", "tempo", "time_signature")]

#creating the correlation matrix
cor_matrix <- cor(heat)

#making it so only half the matrix is shown, prevents repeating each value 
cor_matrix[upper.tri(cor_matrix)] <- NA

#Convert the correlation matrix to long format to make it easier for ggplot
cor_data <- melt(cor_matrix, na.rm = TRUE)

#plotting the heatmap
ggplot(cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "green", high = "red", mid = "white", midpoint = 0,
                       limits = c(-0.8, 0.8)) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +  
  labs(title = "Correlation Heatmap",
       x = "Variables",
       y = "Variables",
       fill = "Correlation") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 20, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)
  )

#loudness/energy are the only two varibles that are strongly correlated across the whole filtered dataset, no matter the track genre
ggplot(filterd_spotify, aes(x=loudness, y=energy, color = track_genre)) +
  geom_point() +
  labs(x="Loudness", y="Energy", title = "Scatterplot of Loudness vs Energy") +
  theme(
    legend.position = "none",
      plot.title = element_text(size = 16, face = "bold"),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 16),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 16),
)

#what if we filtered down to just two track genres

#drum and bass and ambient songs are sufficeinty distinct
a_spotify <- filterd_spotify %>% filter(track_genre == c("ambient","drum-and-bass"))






#heatmap for jsut two track genres

heat_data <- a_spotify[, c("popularity", "duration_ms", "danceability", 
                           "energy", "loudness", "mode", "speechiness", 
                           "acousticness", "instrumentalness", "liveness", 
                           "valence", "tempo", "time_signature")]
cor_matrix <- cor(heat_data, use = "complete.obs")
cor_matrix[upper.tri(cor_matrix)] <- NA
cor_data <- melt(cor_matrix, na.rm = TRUE)


ggplot(cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "green", high = "red", mid = "white", midpoint = 0,
                       limits = c(-0.99, 1)) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) + 
  labs(title = "Correlation Heatmap of 15 varibles (Ambient and DnB)",
       x = "Variables",
       y = "Variables",
       fill = "Correlation") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(angle = 0)
  )

col_pal <- c("ambient" = "#00A4CCFF", "drum-and-bass" = "#F95700FF")


ggplot(a_spotify, aes(x=loudness, y=energy, colour = track_genre)) +
  geom_point() +
  scale_colour_manual(values = col_pal, name = "Genres", labels =c("ambient"="Ambient","drum-and-bass"="DnB")) +
  labs(x="Loudness", y="Energy", title = "Scatterplot of Loudness vs Energy (Ambient and DnB)") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16)
    ) +
  guides(color = guide_legend(reverse = TRUE))


ggplot(a_spotify, aes(x=acousticness, y=energy, colour = track_genre))+
  geom_point()

#logistic regression can therefore be done based on energy/loudness as well as acousticsness/energy
set.seed(123)
a_class <- a_spotify %>% select(loudness, energy, track_genre)

#randomise the test set
a_class <- a_class[sample(1:nrow(a_class)),]

#split into train and test dataset
train_size = 0.7
a_class_train <- a_class[1:(train_size*nrow(a_class)),]
a_class_test <- a_class[(nrow(a_class_train)+1):nrow(a_class),]

#binary label the track genre so ambient is 1, dnb is 0
a_class_train$track_genre <- a_class_train$track_genre == 'ambient'
a_class_train$track_genre <- a_class_train$track_genre *1

#Creating the linear regression model
a_class_model <- glm(
  track_genre ~ loudness+energy,
  family=binomial(link = logit),
  data = a_class_train
)

#Testing the model
a_class_prob <- predict(
  a_class_model,
  newdata = a_class_test,
  type = 'response'
)

#binarising the output
a_class_prob <- ifelse(
  a_class_prob>0.5,
  1,
  0
)

#converting the test track genres into 1s and 0s
a_class_test$track_genre <- a_class_test$track_genre == 'ambient'
a_class_test$track_genre <- a_class_test$track_genre *1

#evaluating the error
a_class_error <- mean(
  a_class_prob != a_class_test$track_genre
)
print(paste('Accuracy ', 1-a_class_error))
#ROC curve/auc anaylsis
roc_curve <- roc(a_class_test$track_genre, a_class_prob)
plot(roc_curve, main = "ROC Curve")
auc_value <- auc(roc_curve)
cat("AUC:", auc_value)


#PCA analysis of the two correlated variables

a_spotify_PCA <- prcomp(a_spotify[, c("loudness", "energy", "acousticness")], center = TRUE, scale. = TRUE)
summary(a_spotify_PCA) 

#Visualization the PCA
ggplot(as.data.frame(a_spotify_PCA$x), aes(PC1, PC2,)) +
  geom_point() +
  labs(title = "PCA: First six Components")

group <- factor(a_spotify$track_genre)
ggbiplot(a_spotify_PCA, 
         ellipse = TRUE,
         groups = group, 
         labels = rownames(data),  
         var.axes = TRUE) +
  scale_color_discrete(name = group)

ggplot(as.data.frame(a_spotify_PCA$x), aes(PC1, PC2, color = group)) +
  geom_point() +
  labs(title = "PCA: First two Components by Genre")


#more fun with PCA

b_spotify <- filterd_spotify %>% filter(track_genre == c("drum-and-bass","ambient", "iranian"))

b_spotify_PCA <- prcomp(b_spotify[, c("loudness", "energy", "acousticness", "popularity", "danceability")], center = TRUE, scale. = TRUE)
summary(b_spotify_PCA) 

#Visualization the PCA
ggplot(as.data.frame(b_spotify_PCA$x), aes(PC1, PC2,)) +
  geom_point() +
  labs(title = "PCA: First two Components")

group <- factor(b_spotify$track_genre)
ggbiplot(b_spotify_PCA, 
         ellipse = TRUE,
         groups = group, 
         labels = rownames(data),  
         var.axes = TRUE) +
  scale_color_discrete(name = group)

ggplot(as.data.frame(b_spotify_PCA$x), aes(PC1, PC2, color = group)) +
  geom_point() +
  labs(title = "PCA: First two Components by Genre")




print(filterd_spotify$track_genre)


#multinomial regression
pca_data <- as.data.frame(b_spotify_PCA$x)

#add track_genre back as  afactor
pca_data$track_genre <- factor(b_spotify$track_genre, levels = c("drum-and-bass", "ambient", "iranian"))

#fit the model
model <- multinom(track_genre ~ PC1 + PC2 + PC3, data = pca_data)

#summary of the model
summary(model)

#get pridcted calsses for confusion matrix
predicted_classes <- predict(model, type = "class")

#create the confsuioj matrix
confusion_matrix <- table(Predicted = predicted_classes, Actual = pca_data$track_genre)

#plot the matrix
ggplot(as.data.frame(confusion_matrix), aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Confusion Matrix: Predicted vs Actual Genres") +
  theme_minimal() +
  geom_text(aes(label = Freq), color = "black", size = 4)







#Creating a bar chart of number of tracks per small number of Genres
tracks <- summarise(filterd_spotify, track_genre)
no_tracks <- table


tracks_per_genre <- tracks %>%
  group_by(track_genre) %>%
  summarise(
    Count = n()
  )
print(tracks_per_genre)
new_tracks <- tracks_per_genre[20:25,]

ggplot(new_tracks, aes(x = track_genre, y = Count, fill = track_genre)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Tracks per Genre", x = "Track Genre", y = "Number of Tracks") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")











#Predicitng weather a song is explicit

explicit_class <- filterd_spotify %>% select(explicit, loudness, popularity, duration_ms, danceability, energy, liveness, valence, tempo, acousticness) 
View(explicit_class)


ggplot(explicit_class, aes(x=energy, y=acousticness, colour = explicit))+
  geom_point()
#randomise
explicit_class <- explicit_class[sample(1:nrow(explicit_class)),]

#split into test and train
train_size_2 = 0.8
train_explicit <- explicit_class[1:(train_size_2*nrow(explicit_class)),]
test_explicit <- explicit_class[(nrow(train_explicit)+1):nrow(explicit_class),]

#binarise explicity
train_explicit$explicit <- train_explicit$explicit == 'True'
train_explicit$explicit <- train_explicit$explicit * 1

#creating the model
explicit_model <- glm(
  explicit ~ loudness+popularity+duration_ms+danceability+energy+liveness+valence+tempo+acousticness,
  family = binomial(logit),
  data = train_explicit
)

#testing the model
explicit_prob <- predict(
  explicit_model,
  newdata = test_explicit,
  type = 'response'
)

explicit_prob <- ifelse(
  explicit_prob>0.5,
  1,
  0
)

#converting test exlpicit into binary
test_explicit$explicit <- test_explicit$explicit == 'True'
test_explicit$explicit <- test_explicit$explicit * 1

#evaluating the model
explicit_error <- mean(
  explicit_prob != test_explicit$explicit
)
print(paste('Accuracy', 1- explicit_error))



#creating a rock curve

roc_curve <- roc(test_explicit$explicit, explicit_prob)
plot(roc_curve, main = "ROC Curve")
auc_value <- auc(roc_curve)
cat("AUC:", auc_value, "\n")

#checking balance of explicit to non explicit
table(test_explicit$explicit)










#most popualar artists, top 20 most frequent artists in the dataset
artist_frequency <- table(filterd_spotify$artists)
artist_frequency_sorted <- sort(artist_frequency, decreasing = TRUE)
top_20 <- head(artist_frequency_sorted, 20)


#convert back to data fram
top_20_df <- data.frame(top_20)
colnames(top_20_df) <-c('artist', 'freq')


ggplot(top_20_df, aes(x= reorder(artist, -freq), y=freq, , fill = artist)) +
  geom_bar(stat = 'identity') +
  coord_flip()+
  labs(title = "Top 20 Artists by Number of Tracks", x = "Artist", y = "Frequency", caption = "Spotify 2022") +
  theme_minimal() +
  theme(legend.position = "none")






#top 5 artists coloured by genre
artist_frequency_5 <- table(filterd_spotify$artists)
artist_frequency_sorted_5 <- sort(artist_frequency_5, decreasing = TRUE)
top_5 <- head(artist_frequency_sorted, 5)


#convert back to data fram
top_5_df <- data.frame(top_5)
colnames(top_5_df) <-c('artist', 'freq')

colours <- c(
  "The Beatles" = "#BEB8EB",
  "George Jones" = "#5299D3",
  "Linkin Park" = "#0B5563",
  "Prateek Kuhad" = "#A2BCE0",
  "Håkan Hellström" = "#5E5C6C"
)

top_5_df$genres <- c("British pop", "Honky Tonk", "Rock", "Indian", "Punk Rock")
ggplot(top_5_df, aes(x= reorder(artist, -freq), y=freq, fill = artist)) +
  geom_bar(stat = 'identity') +
  coord_flip()+ 
  scale_fill_manual(values = colours, labels = c("British pop", "Honky Tonk", "Rock", "Indian", "Punk Rock")) +
  labs(title = "Top 5 Artists by Number of Tracks", x = "Artist", y = "Number of Tracks", caption = "Spotify 2022", 
       fill = "Genre of Artist") +
  theme_minimal() +
  theme(legend.position = "left") +
  guides(fill = guide_legend(reverse = TRUE))



#most popular artists, colored by genre#most Genrespopular artists, colored by genre

artist_frequency_genre <- table(filterd_spotify$artists, filterd_spotify$track_genre)

artist_frequency_genre_df <- as.data.frame(artist_frequency_genre)
colnames(artist_frequency_genre_df) <- c("artist", "track_genre", "freq")


top_20_genre <- artist_frequency_genre_df %>%
  arrange(desc(freq)) %>%  
  head(20)


ggplot(top_20_genre, aes(x= reorder(artist, -freq), y=freq, , fill = track_genre)) +
  geom_bar(stat = 'identity') +
  coord_flip()+
  labs(title = "Top 20 Artists by Number of Tracks", x = "Artist", y = "Frequency", caption = "Spotify 2022") +
  theme_minimal()

#annoying, split beetles songs into the different gernes they produced



#Sort all songs by genres, find the average popularity of each genre and see what i can do with it?
popularity_genre <- filterd_spotify %>% select(popularity, track_genre)

mean_popularity <- aggregate(popularity ~ track_genre, data = popularity_genre, FUN = mean)


#bar chart of popularity
ggplot(mean_popularity, aes(x= reorder(track_genre, -popularity), y=popularity, , fill = track_genre)) +
  geom_bar(stat = 'identity') +
  coord_flip()+
  labs(title = "Mean Popularity of Each Genre", x = "Genre", y = "Popularity", caption = "Spotify 2022") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 6.5))


#what can i do with the two extremes, pop-film and iranian, can they dtermine populairty
pop_film_iranian <- filterd_spotify %>% filter(track_genre == c("pop-film","iranian"))

#line graph of popualrit maybe
ggplot(pop_film_iranian, aes(x = track_name, y = popularity, colour = track_genre)) +
  geom_point(size = 1) + 
  labs(title = "Comapring Popularity of iranian and Pop-film", x = "Tracks", y = "Popularity") +
  theme_minimal() +
  theme(legend.position = "left",
        axis.text.x = element_blank())
#clear divide in popularity

#heat map to maybe see the cause
heat_pop <- pop_film_iranian[, c("popularity", "duration_ms", "danceability", 
                                 "energy", "loudness", "mode", "speechiness", 
                                 "acousticness", "instrumentalness", "liveness", 
                                 "valence", "tempo", "time_signature")]
cor_matrix_3 <- cor(heat_pop, use = "complete.obs")
cor_matrix_3[upper.tri(cor_matrix_3)] <- NA
cor_data_3 <- melt(cor_matrix_3, na.rm = TRUE)


ggplot(cor_data_3, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "green", high = "red", mid = "white", midpoint = 0,
                       limits = c(-0.99, 1)) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) + 
  labs(title = "Correlation Heatmap of 15 varibles (Pop-film/Iranian)",
       x = "Variables",
       y = "Variables",
       fill = "Correlation") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(angle = 0)
  )

#a lot are correlated
ggplot(pop_film_iranian, aes(x=danceability, y=popularity, colour =  track_genre)) +
  geom_point()
ggplot(a_spotify, aes(x=danceability, y=popularity, colour=track_genre)) +
  geom_point()




#Trends of top UK singles Each Year over time (based on most weeks spent at number 1 each year) - potential Idea

johnie <- filterd_spotify[79652,]
paul <- filterd_spotify[77883,]


#Abandoned idea :(









#sentiment analysis maybe sad music names has sadder words than pop?

sad_party <- filterd_spotify %>% filter(track_genre %in% c("sad"))
sad_party <- sad_party %>% select(Column5 = 5)
sad_party <- sad_party %>% rename(track_name = Column5)
words_remove <- c("feat.", "feat")



#split into words
sad_text <- sad_party %>% unnest_tokens(word, track_name)

#clean up the data
sad_text_clean <- sad_text %>%
  filter(!word %in% tolower(words_remove))

#remove stopwords
sad_text_clean_2 <- sad_text_clean %>%
  anti_join(stop_words, by = "word")

#get sentiment, using bing
sentiment <- get_sentiments("bing")

#sentimet classification
sentiment_analysis <- sad_text_clean_2 %>%
  inner_join(sentiment, by = "word") %>%
  count(sentiment, sort = TRUE)

#visualistaion of sad tracks
ggplot(sentiment_analysis, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col() +
  labs(title = "Sentiment Analysis of Sad song track names", x = "Sentiment", y = "Count")


#wordcloud

word_cloud <- sad_text_clean_2 %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  mutate(colour = ifelse(sentiment == "positive", "#63A088", "#FF333A"),
         freq = as.numeric(n)) %>%
  select(word, freq, colour)

wordcloud2(word_cloud, color = word_cloud$colour)

#split word cloud

sad_text_clean_2 %>%
  unnest_tokens(word, word) %>%  
  count(word, sort = TRUE) %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(sentiment = factor(sentiment, levels = c("positive", "negative"))) %>%
  reshape2::acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#63A088", "#FF333A"),
                   max.words = 100, random.order = FALSE, 
                   title.bg.colors = "white", title.colors = "black", 
                   title.size = 1.5)



#same sentiment analysis but for pop songs
pop <- filterd_spotify %>% filter(track_genre %in% c("pop"))
pop <- pop %>% select(Column5 = 5)
pop <- pop %>% rename(track_name = Column5)
words_remove <- c("feat.", "feat")

#split into words
pop_text <- pop %>% unnest_tokens(word, track_name)

#clean up the data
pop_text_clean <- pop_text %>%
  filter(!word %in% tolower(words_remove))

#remove stopwords
pop_text_clean_2 <- pop_text_clean %>%
  anti_join(stop_words, by = "word")

#get sentiment, using bing
sentiment <- get_sentiments("bing")

#sentimet classification
sentiment_analysis2 <- pop_text_clean_2 %>%
  inner_join(sentiment, by = "word") %>%
  count(sentiment, sort = TRUE)

#visualistaion of sad tracks
ggplot(sentiment_analysis2, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col() +
  labs(title = "Sentiment Analysis of Pop song track names", x = "Sentiment", y = "Count")




#word cloud on every word in every track name
all_tracks <- filterd_spotify %>% select(Column5 = 5)
all_tracks <- all_tracks %>% rename(track_name = Column5)

#split into words
all_text <- all_tracks %>% unnest_tokens(word, track_name)

#clean up the data
all_text_clean <- all_text %>%
  filter(!word %in% tolower(words_remove))

#remove stopwords
all_text_clean_2 <- all_text_clean %>%
  anti_join(stop_words, by = "word")

#get sentiment, using bing
sentiment <- get_sentiments("bing")

#sentimet classification
sentiment_analysis3 <- all_text_clean_2 %>%
  inner_join(sentiment, by = "word") %>%
  count(sentiment, sort = TRUE)

#visualistaion of all tracks
ggplot(sentiment_analysis3, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col() +
  labs(title = "Sentiment Analysis of all tracks", x = "Sentiment", y = "Count")

#word cloud

word_cloud <- all_text_clean_2 %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  mutate(colour = ifelse(sentiment == "positive", "#63A088", "#FF333A"),
         freq = as.numeric(n)) %>%
  select(word, freq, colour)

wordcloud2(word_cloud, color = word_cloud$colour)


#split word cloud
all_text_clean_2 %>%
  unnest_tokens(word, word) %>%  
  count(word, sort = TRUE) %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(sentiment = factor(sentiment, levels = c("positive", "negative"))) %>%
  reshape2::acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#63A088", "#FF333A"),
                   random.order = FALSE, 
                   title.bg.colors = "white", title.colors = "blue", 
                   title.size = 1.5)


view(sentiment_analysis)


#Clustered bar chart of all three bing sentiment analyses
total_sum <- sum(sentiment_analysis$n)
total_sum2 <- sum(sentiment_analysis2$n)
total_sum3 <- sum(sentiment_analysis3$n)
View(sentiment_analysis)

#New collumn with percentages
sentiment_analysis$percentage <- (sentiment_analysis$n / total_sum) * 100
sentiment_analysis <- sentiment_analysis %>% select(-2)
colnames(sentiment_analysis)[2] <- "Sad_pecentage"


sentiment_analysis2$percentage <- (sentiment_analysis2$n / total_sum2) * 100
sentiment_analysis2 <- sentiment_analysis2 %>% select(-2)
colnames(sentiment_analysis2)[2] <- "Pop_percentage"

sentiment_analysis3$percentage <- (sentiment_analysis3$n / total_sum3) * 100
sentiment_analysis3 <- sentiment_analysis3 %>% select(-2)
colnames(sentiment_analysis3)[2] <- "All_percentage"

merged_sentiment <- merge(sentiment_analysis, sentiment_analysis2, by = "sentiment")
merged_sentiment <- merge(merged_sentiment, sentiment_analysis3, by = "sentiment")
colnames(merged_sentiment) <- c("sentiment", ".Sad", ".Pop", ".All")


#transforming the data

merged_sentiment_long <- merged_sentiment %>%
  pivot_longer(cols = starts_with("."),  
               names_to = "group",           
               values_to = "count")          


color_pal <- c(".All" = "#BAC1B8", ".Pop" = "#58A4B0", ".Sad" = "#0C7C59")


ggplot(merged_sentiment_long, aes(x = sentiment, y = count, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = color_pal, name = "Genres", labels =c(".Pop"="Pop",".All"="All Genres",".Sad"="Sad")) +
  theme_minimal() +
  labs(x = "Sentiment", y = "Percentage of Words (%)", fill = "Genre", title = "Grouped Bar Chart showing the sentiment of words in song titles by genre using the Bing Lexicon") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)
  )



#more meaningfull sentiment analysis using NRC
nrc_lexicon <- get_sentiments("nrc")

sentiment_analysis <- sad_text_clean_2 %>%
  inner_join(nrc_lexicon, by = "word")



emotion_counts <- sentiment_analysis %>%
  count(sentiment) %>%
  arrange(desc(n))

#count of each emotion
print(emotion_counts)

#visualtation of sentiment
ggplot(emotion_counts, aes(x = reorder(sentiment, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() +
  labs(title = "Emotion Distribution of words in sad song genre", x = "Emotion", y = "Frequency")


#versus all genre catagories

sentiment_analysis <- all_text_clean_2 %>%
  inner_join(nrc_lexicon, by = "word")


emotion_counts2 <- sentiment_analysis %>%
  count(sentiment) %>%
  arrange(desc(n))

#count of each emotion
print(emotion_counts2)

#visualtation of sentiment
ggplot(emotion_counts2, aes(x = reorder(sentiment, n), y = n)) +
  geom_bar(stat = "identity", fill = "#FF6961", color = "black") +
  coord_flip() +
  labs(title = "Emotion Distribution of words in all genres", x = "Emotion", y = "Frequency")







#do a objectivey happy genre
party <- filterd_spotify %>% filter(track_genre %in% c("pop"))
party <- party %>% select(Column5 = 5)
party <- party %>% rename(track_name = Column5)
words_remove <- c("feat.", "feat")



#split into words
party_text <- party %>% unnest_tokens(word, track_name)

#clean up the data
party_text_clean <- party_text %>%
  filter(!word %in% tolower(words_remove))

#remove stopwords
party_text_clean_2 <- party_text_clean %>%
  anti_join(stop_words, by = "word")

sentiment_analysis <- party_text_clean_2 %>%
  inner_join(nrc_lexicon, by = "word")


emotion_counts3 <- sentiment_analysis %>%
  count(sentiment) %>%
  arrange(desc(n))

#count of each emotion
print(emotion_counts3)

#visualtation of sentiment
ggplot(emotion_counts3, aes(x = reorder(sentiment, n), y = n)) +
  geom_bar(stat = "identity", fill = "#BEE3BA", color = "black") +
  coord_flip() +
  labs(title = "Emotion Distribution of words in pop song genre", x = "Emotion", y = "Frequency")


#grouped bar chart
total_sum <- sum(emotion_counts$n)
total_sum3 <- sum(emotion_counts3$n)
total_sum2 <- sum(emotion_counts2$n)


#New collumn with percentages
emotion_counts$percentage <- (emotion_counts$n / total_sum) * 100
emotion_counts <- emotion_counts %>% select(-2)

emotion_counts3$percentage <- (emotion_counts3$n / total_sum3) * 100
emotion_counts3 <- emotion_counts3 %>% select(-2)

emotion_counts2$percentage <- (emotion_counts2$n / total_sum2) * 100
emotion_counts2 <- emotion_counts2 %>% select(-2)

merged_emotion <- merge(emotion_counts, emotion_counts3, by = "sentiment", suffixes = c("_sad", "_pop"))
merged_emotion <- merge(merged_emotion, emotion_counts2, by = "sentiment", suffixes = c("", "_all"))
colnames(merged_emotion) <- c("sentiment", ".Sad", ".Pop", ".All")


#transforming the data

merged_emotion_long <- merged_emotion %>%
  pivot_longer(cols = starts_with("."),  
               names_to = "group",           
               values_to = "count")          


color_pal <- c(".All" = "#BAC1B8", ".Pop" = "#58A4B0", ".Sad" = "#0C7C59")


ggplot(merged_emotion_long, aes(x = sentiment, y = count, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = color_pal, name = "Genres", labels =c(".Pop"="Pop",".All"="All Genres",".Sad"="Sad")) +
  theme_minimal() +
  labs(x = "Emotion", y = "Percentage of Words (%)", fill = "Genre", title = "Grouped Bar Chart Sentiment of words in song titles by genre using the NRC lexicon") +
  theme(plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 20, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16)
  )
  












#Does music genre vary by region

genre_summary <- filterd_spotify %>%
  group_by(track_genre) %>%
  summarize(count = n())

#find out music gernes based on country
print(n= 120, genre_summary)

genres <- c("british", "brazil", "french", "german", "indian", "iranian", 
            "malay", "spanish", "swedish", "turkish")

#filter them into new variable
world_music <- filterd_spotify %>%
  filter(track_genre %in% genres)

View(world_music)

#initial descriptive statistics
genre_summary <- world_music %>%
  group_by(track_genre) %>%
  summarize(
    mean_duration = mean(energy),
    median_duration = median(energy),
    sd_duration = sd(energy),
    min_duration = min(energy),
    max_duration = max(energy)
  )
print(genre_summary)

#run PCA on all variables
world_PCA <- prcomp(world_music[, c("loudness", "energy", "acousticness", "popularity", "danceability", "duration_ms", "speechiness", "instrumentalness",
                                    "liveness", "valence", "tempo")], center = TRUE, scale. = TRUE)
summary(world_PCA) 

#Visulalistion the PCA
ggplot(as.data.frame(world_PCA$x), aes(PC1, PC2,)) +
  geom_point() +
  labs(title = "PCA of First two Components")

group <- factor(world_music$track_genre)
ggbiplot(world_PCA, 
         ellipse = TRUE,
         groups = group, 
         labels = rownames(data),  
         var.axes = TRUE) +
  scale_color_discrete(name = group)

ggplot(as.data.frame(world_PCA$x), aes(PC1, PC2, color = group)) +
  geom_point() +
  labs(title = "PCA: First two Components by Genre")



#too many are too close, what if repeated but used british too represent all of europe
genres <- c("indian", "iranian", "chicago-house", "latino")

#filter them into new variable
new_world_music <- filterd_spotify %>%
  filter(track_genre %in% genres)


new_world_PCA <- prcomp(new_world_music[, c("loudness", "energy", "acousticness", "popularity", "danceability", "duration_ms", "speechiness", "instrumentalness",
                                            "liveness", "valence", "tempo")], center = TRUE, scale. = TRUE)
summary(new_world_PCA) 

#Visulalistion the PCA
ggplot(as.data.frame(new_world_PCA$x), aes(PC1, PC2,)) +
  geom_point() +
  labs(title = "New world music, PCA of First two Components")

group <- factor(new_world_music$track_genre)
ggbiplot(new_world_PCA, 
         ellipse = TRUE,
         groups = group, 
         labels = rownames(data), 
         var.axes = TRUE) +
  scale_color_discrete(name = group)

ggplot(as.data.frame(new_world_PCA$x), aes(PC1, PC2, color = group)) +
  geom_point() +
  labs(title = "PCA: First two Components by Genre")

#iranian very distince, rest are too close together, maybe try heatmap to see what vriables to remove

world_heat <- world_music[, c("popularity", "duration_ms", "danceability", 
                              "energy", "loudness", "mode", "speechiness", 
                              "acousticness", "instrumentalness", "liveness", 
                              "valence", "tempo", "time_signature")]

#creating the correlation matrix
cor_matrix <- cor(world_heat)

#making it so only half the matrix is shown, prevents repeating each value 
cor_matrix[upper.tri(cor_matrix)] <- NA

#Convert the correlation matrix to long format to make it easier for ggplot
cor_data <- melt(cor_matrix, na.rm = TRUE)

#plotting the heatmap
ggplot(cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "green", high = "red", mid = "white", midpoint = 0,
                       limits = c(-0.8, 0.8)) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +  
  labs(title = "Correlation Heatmap of all Genres",
       x = "Variables",
       y = "Variables",
       fill = "Correlation") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
  )

#using this remove all un correalted variables and different genres
genres <- c("indian", "iranian", "chicago-house", "j-idol")

ggplot(filterd_spotify, aes(x = valence, y = popularity, colour = track_genre)) +
  geom_point()

#filter them into new variable
new_world_music <- filterd_spotify %>%
  filter(track_genre %in% genres)

ggplot(new_world_music, aes(x = valence, y = loudness, colour = track_genre)) +
  geom_point()

new_world_PCA <- prcomp(new_world_music[, c("loudness", "energy", "acousticness", "popularity", "danceability", "speechiness", "instrumentalness",
                                            "liveness", "valence", "tempo")], center = TRUE, scale. = TRUE)
summary(new_world_PCA) 

#Visulalistion the PCA
ggplot(as.data.frame(new_world_PCA$x), aes(PC1, PC2,)) +
  geom_point() +
  labs(title = "New world music, PCA of First two Components")

group <- factor(new_world_music$track_genre)


ggbiplot(new_world_PCA, 
         ellipse = TRUE,
         groups = group, 
         labels = rownames(data), 
         var.axes = TRUE) +
  scale_color_discrete(name = "Genre") +
  ggtitle("Principle Component Analysis of Genre Variation") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)
  )

#custom colours
color_palette <- c("j-idol" = "#BC002D", "chicago-house" = "#002147", "iranian" = "#239F40", "indian" = "#FF671F")

ggbiplot(new_world_PCA, 
         ellipse = TRUE,
         groups = group, 
         labels = rownames(data),  
         var.axes = TRUE) +
  scale_color_manual(name = "Genres", values = color_palette) +  
  scale_fill_manual(values = color_palette) + 
  theme_minimal()

ggplot(as.data.frame(new_world_PCA$x), aes(PC1, PC2, color = group)) +
  geom_point() +
  labs(title = "PCA: First two Components by Genre")



#random Forrest classification
#getting PCA scores from PCA
pca_score <- new_world_PCA$x


#Ensuring the genre column is a factor
new_world_music$genre <- as.factor(new_world_music$track_genre)

#combining the PCA scores with the Genre 
pca_data_frame <- data.frame(pca_score, genre = new_world_music$genre)


#split data set
set.seed(182) 
PCAIndex <- createDataPartition(pca_data_frame$genre, p = 0.8, list = FALSE)
train_PCA <- pca_data_frame[PCAIndex, ]
test_PCA <- pca_data_frame[-PCAIndex, ]


#Use the PCA score to predict the genre
PCA_rf_model <- randomForest(genre ~ ., data = train_PCA, ntree = 100)

#Model summary
print(PCA_rf_model)

predict_rf <- predict(PCA_rf_model, test_PCA)

#model evaluation
confusionMatrix(predict_rf, test_PCA$genre)


accuracy <- sum(predict_rf == test_PCA$genre) / nrow(test_PCA)
print(paste("Accuracy: ", accuracy))
conferance_m <- confusionMatrix(predict_rf, test_PCA$genre)
print(conferance_m)

conferance_m_table <- as.data.frame(conferance_m$table)


#visualizing the model
ggplot(conferance_m_table, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") + 
  scale_fill_gradient(low = "white", high = "lightblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(),
        axis.text.y = element_text(size = 12)) +
  labs(x = "Actual Genres", y = "Predicted Genres", fill = "Count") +
  ggtitle("Confusion Matrix Heatmap of WorldWide Genres") +
  geom_text(aes(label = Freq), color = "black", size = 6) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)
  )




#creating the world map
#get world map
world_map <- ne_countries(scale = "medium", returnclass = "sf")

#Highlight  specific countries in the PCA
PCA_countries <- c("United States of America", "India", "Japan", "Iran")

#Select countries on world map
countries_mapped <- world_map[world_map$name %in% PCA_countries, ]

ggplot(data = world_map) +
  geom_sf(fill = "lightgray") +  
  geom_sf(data = countries_mapped, aes(fill = name), color = "black", size = 0.2) +  
  scale_fill_manual(values = c("United States of America" = "#F4948E",  
                               "India" = "#B4CD76",               
                               "Japan" = "#CC8CFC",                
                               "Iran" = "#6FD4D6")) +           
  theme_minimal() +
  ggtitle("Map Highlighting the Nations from which the music was compared") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size = 14)
  )
