if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr")
if(!require(lubridate)) install.packages("lubridate")

library(lubridate)
library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(Metrics)
library(e1071)

movies <- read.csv("movies.csv",stringsAsFactors=FALSE)
ratings <- read.csv("ratings.csv")
movtag <- read.csv("ratings.csv")
tagscore <- read.csv("ratings.csv")
tags <- read.csv("tags.csv")

head(movies)
head(ratings)
head(tags)

str(movies)
str(ratings)

data.table(movies)
data.table(ratings)

summary(movies)
summary(ratings)

#Lets see mean rating for each movie ID
meanRating = aggregate( ratings$rating ~ ratings$movieId,data=ratings, FUN= mean) 
meanRating


#Get all the genres (Adventure|Animation|Children|Comedy|Fantasy) into a dataframe
genres <- as.data.frame(movies$genres,stringsASFactors = FALSE)
genre <- as.data.frame(tstrsplit(genres[,1], '[|]', 
                                        type.convert=TRUE), 
                              stringsAsFactors=FALSE) 

#list out all genre so i can create list
unique(genre[c(1)]) 
unique(genre[c(2)])

head(genres)
colnames(genre) <- c(1:10)
genrelist <- c("Action","Adventure","Animation","Comedy","Children","Crime","Drama","Documentary","Fantasy","Film-Noir",
               "Horror","Musical","Mystery","Romance","Sci-Fi","Thriller","war","western")

genrelist

genre_matl <- matrix(0,10330,18)
genre_matl[1,] <- genrelist
colnames(genre_matl) <- genrelist

for (index in 1:nrow(genre)){
  for(col in 1:ncol(genre)){
    gen_col = which(genre_matl[1,] == genre[index,col])
    genre_matl[index+1,gen_col] <- 1
  }
}

genre_mat2 <- as.data.frame(genre_matl[-1,], stringsAsFactors=FALSE)

for (col in 1:ncol(genre_mat2)) {
  genre_mat2[,col] <- as.integer(genre_mat2[,col])
}

str(genre_mat2)

movielens <- left_join(ratings, movies, by = "movieId")
head(movielens)

movielens %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram( bins=20, color = "blue") +
  scale_x_log10() + 
  ggtitle("Number of Ratings for each Movies") +
  xlab("Movie ID") +
  ylab("Number of Ratings")


movielens %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram( bins=20, color = "blue") +
  scale_x_log10() + 
  ggtitle("Number of Ratings given by each Users") +
  xlab("User ID") +
  ylab("Number of Ratings")


movielens %>% 
  count(genres) %>% 
  ggplot(aes(n)) + 
  geom_histogram( bins=20, color = "blue") +
  scale_x_log10()+ 
  ggtitle("Number of Ratings for each Genres") +
  xlab("Genres") +
  ylab("Number of Ratings")

movielens %>% count(rating) %>%
  ggplot(aes(rating,n)) + 
  geom_line() +
  geom_point() +
  ggtitle("Number of Ratings for each Rating") +
  xlab("Rating") +
  ylab("Number of Ratings")

#spilt dataset into 8:2 ratio
samp <- createDataPartition(movielens$rating, p = 0.8, list = FALSE)
training <- movielens[samp,]
testing <- movielens[-samp,]

#memory.limit(size=56000)

#lm1 <- train(rating~., data = training, method = "lm")
#rf1 <- train(rating~., data = training, method = "rf")

#rf1$results
#lm1$results

svm.model <- svm(formula = ~ ., data = training, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, testing[,-10])

rmse(data$actual, data$predicted)
