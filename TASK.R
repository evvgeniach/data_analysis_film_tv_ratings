library(readr)

#read in the data
cast_cred <- read_csv("C:/Users/Evgenia Charalambous/OneDrive/evgenia/Έγγραφα/Nepa_Task/casting_credits.csv")
movies_shows <- read_csv("C:/Users/Evgenia Charalambous/OneDrive/evgenia/Έγγραφα/Nepa_Task/movies_and_shows.csv")

# function to replace strings with lists
clean_func <- function(x) {
  cleaned_str <- gsub("\\[|\\]|\'", "", x)
  split_str <- strsplit(cleaned_str, ", ")
  return(split_str[[1]])
}

movies_shows$genres <- lapply(movies_shows$genres, clean_func)
movies_shows$production_countries <- lapply(movies_shows$production_countries, clean_func)


## Q1

library(dplyr)
library(tidyr)


# mean values for each column
mean_imdb_score <- mean(movies_shows$imdb_score, na.rm = TRUE)
mean_tmdb_score <- mean(movies_shows$tmdb_score, na.rm = TRUE)
mean_imdb_votes <- mean(movies_shows$imdb_votes, na.rm = TRUE)
mean_tmdb_popularity <- mean(movies_shows$tmdb_popularity, na.rm = TRUE)


# mean IMDb and TMDb scores by genre
mean_scores_by_genre <- movies_shows %>%
  unnest(genres) %>%
  group_by(genres) %>%
  summarise(mean_imdb_by_genre = mean(imdb_score, na.rm = TRUE),
    mean_tmdb_by_genre = mean(tmdb_score, na.rm = TRUE),
    mean_imdb_votes_by_genre = mean(imdb_votes, na.rm = TRUE),
    mean_tmdb_popularity_by_genre = mean(tmdb_popularity, na.rm = TRUE))

# fill NA values with genre-specific mean values
movies_shows <- movies_shows %>%
  rowwise() %>%
  mutate(imdb_score = ifelse(is.na(imdb_score) & length(unlist(genres)) > 0, mean(mean_scores_by_genre$mean_imdb_by_genre[mean_scores_by_genre$genres %in% unlist(genres)], na.rm = TRUE), imdb_score),
    tmdb_score = ifelse(is.na(tmdb_score) & length(unlist(genres)) > 0, mean(mean_scores_by_genre$mean_tmdb_by_genre[mean_scores_by_genre$genres %in% unlist(genres)], na.rm = TRUE), tmdb_score),
    imdb_votes = ifelse(is.na(imdb_votes) & length(unlist(genres)) > 0, mean(mean_scores_by_genre$mean_imdb_votes_by_genre[mean_scores_by_genre$genres %in% unlist(genres)], na.rm = TRUE), imdb_votes),
    tmdb_popularity = ifelse(is.na(tmdb_popularity) & length(unlist(genres)) > 0, mean(mean_scores_by_genre$mean_tmdb_popularity_by_genre[mean_scores_by_genre$genres %in% unlist(genres)], na.rm = TRUE), tmdb_popularity))

movies_shows <- movies_shows %>%
  rowwise() %>%
  mutate(imdb_score = ifelse(length(unlist(genres)) == 0, mean_imdb_score, imdb_score),
    tmdb_score = ifelse(length(unlist(genres)) == 0, mean_tmdb_score, tmdb_score),
    imdb_votes = ifelse(length(unlist(genres)) == 0, mean_tmdb_score, imdb_votes),
    tmdb_popularity = ifelse(length(unlist(genres)) == 0, mean_tmdb_popularity, tmdb_popularity))

# filter cast_cred to only include directors and join with movies_shows
joined_data<- cast_cred %>%
  filter(role == "DIRECTOR") %>%
  inner_join(movies_shows, by = "film_id") %>%
  filter(type == "MOVIE")


joined_data<- joined_data %>%
  mutate(weighted_imdb = imdb_score * imdb_votes,
    weighted_tmdb = tmdb_score * tmdb_popularity)

#  weighted score for each director
director_ratings<- joined_data %>%
  group_by(name) %>%
  summarise(total_imdb_votes = sum(imdb_votes, na.rm = TRUE),
    total_tmdb_popularity = sum(tmdb_popularity, na.rm = TRUE),
    sum_weighted_imdb = sum(weighted_imdb, na.rm = TRUE),
    sum_weighted_tmdb = sum(weighted_tmdb, na.rm = TRUE),
    film_count=n()) %>%
  mutate(avg_weighted_imdb = sum_weighted_imdb / total_imdb_votes,
    avg_weighted_tmdb = sum_weighted_tmdb / total_tmdb_popularity,
    overall_weighted_avg = (avg_weighted_imdb + avg_weighted_tmdb) / 2) %>%
  arrange(desc(overall_weighted_avg))



print(director_ratings[1,])
library(ggplot2)
ggplot(director_ratings[1:10, ], aes(x = reorder(name, overall_weighted_avg), y = overall_weighted_avg)) +
  geom_bar(stat = "identity", aes(fill = name)) +
  scale_fill_brewer(palette = "Set3") +
  coord_flip() +
  xlab("Director") +
  ylab("Overall Weighted Score") +
  ggtitle("Top 10 Directors") +
  theme_bw() +
  theme(legend.position = "none", 
        legend.title = element_blank(),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, hjust = 0.5, face = "bold"))


##### Q2 ######

## filter movies_shows to only include show, comedy in the US
q2<-movies_shows[(movies_shows$type == "SHOW" & movies_shows$genres == "comedy" & movies_shows$production_countries == "US"),]

q2<- q2 %>%
  mutate(new_score = (imdb_score + tmdb_score) / 2) %>%
  arrange(desc(new_score))
## Seinfeld
q2<- q2[1:10,]

ggplot(q2[order(-q2$new_score), ], aes(x = reorder(title, new_score), y = new_score)) +
  geom_bar(stat = "identity", aes(fill = title)) +
  scale_fill_brewer(palette = "Set3") +
  coord_flip() +
  xlab("TV Show Title") +
  ylab("Average Score") +
  ggtitle("TV Shows Rated by Score") +
  theme_bw() +
  theme(legend.position = "none")

ggplot(q2[order(-q2$tmdb_popularity), ], aes(x = reorder(title, tmdb_popularity), y = tmdb_popularity)) +
  geom_bar(stat = "identity", aes(fill = title)) +
  scale_fill_brewer(palette = "Set3") +
  coord_flip() +
  xlab("TV Show Title") +
  ylab("Popularity") +
  ggtitle("TV Shows Ranked by Popularity") +
  theme_bw() +
  theme(legend.position = "none")

ggplot(q2[order(-q2$release_year), ], aes(x = reorder(title, -release_year), y = release_year)) +
  geom_point(size = 4, color = "steelblue") +
  coord_flip() +
  xlab("TV Show Title") +
  ylab("Year of Release") +
  ggtitle("TV Shows by Year of Release") +
  theme_bw() +
  theme(legend.position = "none")


## Q3

more_than_120<- movies_shows %>%
  filter(type == "MOVIE", runtime_minutes > 120)
less_than_120<-movies_shows %>%
  filter(type == "MOVIE", runtime_minutes <= 120)


movies<-movies_shows %>%
  filter(type == "MOVIE")

# scatter plot
ggplot(movies, aes(x = runtime_minutes, y = imdb_score)) +
  theme_bw()+
  geom_point() +
  geom_smooth(method = 'lm') +
  ggtitle("Scatter Plot of Runtime vs IMDB Score") +
  xlab("Runtime minutes")+
  ylab("IMDb Score")


# mean, median,  for movies with runtime more than 120 minutes
mean_more_than_120<- mean(more_than_120$imdb_score)
median_more_than_120<- median(more_than_120$imdb_score)

# mean, median, for movies with runtime less than 120 minutes
mean_less_than_120<- mean(less_than_120$imdb_score)
median_less_than_120<- median(less_than_120$imdb_score)

# boxplot
ggplot() +
  theme_bw() +
  geom_boxplot(data = more_than_120, aes(x = "More than 120", y = imdb_score)) +
  geom_boxplot(data = less_than_120, aes(x = "Less than 120", y = imdb_score)) +
  ggtitle("Boxplot of IMDb Scores by Runtime Category") +
  ylab("IMDb Score") +
  xlab("Runtime Category")

##
t.test(more_than_120$imdb_score, less_than_120$imdb_score)

# correlation between runtime and imdb score
cor_result <- cor(movies$runtime_minutes, movies$imdb_score, use = "complete.obs", method = "pearson")

## Machine learning
library(caret)
library(tidyverse)
x<- movies$runtime_minutes  
y<- movies$imdb_score
dataX <- data.frame(runtime_minutes = x, imdb_score = y)
## Linear Regression ##
# splitting the data into training and test sets (80% training, 20% test)
set.seed(3222)
trainIndex<- createDataPartition(dataX$imdb_score, p = 0.8, list = FALSE)
train<-dataX[trainIndex,]
test<-dataX[-trainIndex,]

# train the model
mod <- lm(imdb_score ~ ., data = train)

# evaluate on the test set
predictions <- predict(mod, newdata = test)
rmse <- sqrt(mean((as.vector(predictions) - test$imdb_score)^2))

# predict IMDb score for a movie with runtime longer than 120 minutes (140)
pred_long_runtime <- predict(mod, newdata = data.frame(runtime_minutes = 140))

# Predict IMDb score for a movie with runtime shorter than 120 minutes (90)
pred_short_runtime <- predict(mod, newdata = data.frame(runtime_minutes = 90))
