
##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


# Study the number and average rating per user:

User <- edx %>% select(userId, rating) %>% 
  group_by(userId) %>% 
  summarize(n = n(), avgRating = mean(rating)) %>% 
  arrange(desc(n))

User %>% ggplot(aes(n)) + 
  geom_histogram(color = "blue", fill = "white", size = 1) + 
  scale_x_log10() + 
  theme_bw() + 
  labs(x = "Nr of ratings (log scale)", y = "Nr of users", title = "Repartition of the number of ratings per user")


User %>% ggplot(aes(avgRating)) + 
         geom_histogram(bins = 50, color = "blue", fill = "white", size = 1) + 
         theme_bw() + 
         labs(x = "Average rating", y = "Nr of users", title = "Repartition of the users average rating")


# Study the number and average rating per movie:

Mov <- edx %>% 
  select(movieId, rating) %>% 
  group_by(movieId) %>% 
  summarize(n = n(), avgRating = mean(rating)) %>% 
  arrange(desc(n))

Mov %>% ggplot(aes(n)) + 
  geom_histogram(bins = 20, color = "blue", fill = "white", size = 1) + 
  scale_x_log10() + 
  theme_bw() + 
  labs(x = "Nr of ratings (log scale)", y = "Nr of movies", title = "Repartition of the number of ratings per movie")


Mov %>% ggplot(aes(avgRating)) + 
        geom_histogram(bins = 50, color = "blue", fill = "white", size = 1) + 
        theme_bw() + 
        labs(x = "Average rating", y = "Nr of movies", title = "Repartition of the movies average rating")



# Loading libraries for timestamp processing

if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(anytime)) install.packages("anytime", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")

library(lubridate)
library(anytime)
library(scales)

options(digits = 5)


# Study evolution of ratings with time.

RatingThroughYear <- edx %>% mutate(TimeFactor = year(anytime(timestamp))) %>% 
  group_by(TimeFactor) %>% 
  summarise(n = n(), avgRating = mean(rating)) %>%
  mutate(var = "Year")

RatingThroughMonth <- edx %>% mutate(TimeFactor = month(anytime(timestamp))) %>% 
  group_by(TimeFactor) %>% 
  summarise(n = n(), avgRating = mean(rating)) %>% 
  mutate(var = "Month")

RatingThroughTime <- rbind(RatingThroughYear, RatingThroughMonth)

RatingThroughTime %>% ggplot(aes(TimeFactor, n)) + 
  geom_line(color = "blue", size = 1) + 
  theme_bw() + 
  scale_x_continuous(breaks = pretty_breaks()) + 
  xlab("Time (month on the left graph / year on the right graph)") + 
  facet_grid(cols = vars(var), scales = "free") + 
  labs(x = "rating month (left graph) / rating year (right graph)", y = "Nr of ratings", title = "Fluctuation of the number of ratings across time")



RatingThroughTime %>% ggplot(aes(TimeFactor, avgRating)) + 
                      geom_line(color = "blue", size = 1) + 
                      theme_bw() + 
                      xlab("Time (month on the left graph / year on the right graph)") + 
                      facet_grid(cols = vars(var), scales = "free") + 
                      labs(x = "rating month (left graph) / rating year (right graph)", y = "Average rating", title = "Fluctuation of the average rating across time")



# Study the number and average rating per combination of genres:

MovperGenre <- edx %>% select(genres, movieId) 

MovperGenre <- unique(MovperGenre) %>% 
  group_by(genres) %>% 
  summarize(nMovies = n())

MovperGenre %>% arrange(desc(nMovies)) %>% top_n(10)
MovperGenre %>% arrange(nMovies) %>% top_n(-10)

MovperGenre %>% ggplot(aes(nMovies)) + 
  geom_histogram(bins = 15, color = "blue", fill = "white", size = 1) + 
  scale_x_log10() + 
  theme_bw() + 
  labs(x = "Number of movies (log scale)", y = "Nr of genres", title = "Dispatch of the number of movies per genres")



AvgRperGenre <- edx %>% select(genres, rating) %>% 
                        group_by(genres) %>% 
                        summarize(avgRating = mean(rating))

AvgRperGenre %>% arrange(desc(avgRating)) %>% top_n(10)
AvgRperGenre %>% arrange(avgRating) %>% top_n(-10)

AvgRperGenre %>% ggplot(aes(avgRating)) + 
                 geom_histogram(bins = 50, color = "blue", fill = "white", size = 1) + 
                 scale_x_log10() + 
                 theme_bw() + 
                 labs(x = "Average rating", y = "Nr of genres", title = "Dispatch of the average ratings per genres")



# Study the number and average rating per movie production decade:

MovieDecades <- edx %>% mutate(movieYear = substring(title, nchar(title)-4, nchar(title)-1)) %>%
                        mutate(movieDecade = 10*floor(as.numeric(movieYear)/10)) %>% select(movieDecade, movieId)

MovieDecades <- unique(MovieDecades)

MovieDecades %>% ggplot(aes(movieDecade)) + 
                 geom_histogram(bins = 10, color = "blue", fill = "white", size = 1) + 
                 theme_bw() + 
                 scale_x_continuous(breaks = pretty_breaks()) + 
                 labs(x = "Decade", y = "Number of movies produced", title = "Dispatch of movies per production decade")


MovieDecades <- edx %>% mutate(movieYear = substring(title, nchar(title)-4, nchar(title)-1)) %>%
  mutate(movieDecade = 10*floor(as.numeric(movieYear)/10)) %>% 
  group_by(movieDecade) %>% 
  summarize(n = n(), avgRating = mean(rating))

MovieDecades %>% ggplot(aes(movieDecade, avgRating)) + 
  geom_line(color = "blue", size = 1) + 
  theme_bw() + 
  scale_x_continuous(breaks = pretty_breaks()) + 
  labs(x = "Movie decade", y = "Average rating", title = "Average rating of movies per production decade")



# A bit of cleanup.

rm(MovieDecades, MovperGenre, AvgRperGenre, RatingThroughTime, RatingThroughYear, RatingThroughMonth, Mov, User)


# Creation of the training and test sets.

  GenerateSubSet <- function(DF, n) {
    
    set.seed(2020, sample.kind="Rounding")
    DFSample <- sample(nrow(DF), size = n, replace = FALSE)
    DF[DFSample,]
  }
  
  
  SubSet <- GenerateSubSet(edx, nrow(edx))
  indexes <- createDataPartition(y = SubSet$userId, times = 1, p = 0.1, list = FALSE)
  
  TrainingSet <- SubSet[-indexes,]
  TestSet <- SubSet[indexes,]
  
  rm(SubSet, indexes)



# Function that will extract the year from the timestamp and the decade of the movie production and add to the data set .

ExtractMovieYearAndRatingYear <- function(DF) {
  
  DF %>% mutate(ratingYear = year(anytime(timestamp))) %>% 
         mutate(movieYear = substring(title, nchar(title)-4, nchar(title)-1)) %>% 
         mutate(movieDecade = 10*floor(as.numeric(movieYear)/10))
  
}

  TrainingSet <- ExtractMovieYearAndRatingYear(TrainingSet)



# Function that extract the average movie rating of all movies and all users.

GetTheBasis <- function(DF) {
  
  AvgOverall <- DF %>% summarize(avgOverall = mean(rating)) %>% pull(avgOverall)  
  
}


# Compute for the Training Set the average rating for all movies and all users.

  AvgOverall <- GetTheBasis(TrainingSet)
   
  TrainingSet <- TrainingSet %>% mutate(Y_hat = AvgOverall)



# Function that get the number of ratings for each movie, the average rating for each movie for all users and the difference between this movie and the average rating over all movies.

  GetTheMovieAverageRating <- function(DF) {
  
  AvgOverall <- GetTheBasis(DF)
  
  DF %>% group_by(movieId) %>% 
         summarize(avgRatingMovie = mean(rating), nMovie = n()) %>% 
         mutate(DiffRatingMovie = avgRatingMovie - AvgOverall) %>%
         select(movieId, DiffRatingMovie, nMovie)
  
}


# Compute for the Training Set the average rating of each movie for all users.
  
  Mavg <- GetTheMovieAverageRating(TrainingSet)
  
  TrainingSet <- TrainingSet %>% 
                  left_join(Mavg, by = "movieId") %>% 
                  mutate(DiffRatingMovie = ifelse(!is.na(DiffRatingMovie), DiffRatingMovie, 0), nMovie = ifelse(!is.na(nMovie), nMovie, 0))
  
  TrainingSet <- TrainingSet %>% mutate(Y_hat = AvgOverall + DiffRatingMovie)



# Function that computes the number of rating for each user, his averge rating across all movies and the difference between his average rating and the overall rating average.

GetTheUserAverageRating <- function(DF) {
  
  AvgOverall <- GetTheBasis(DF)
  
  DF %>% group_by(userId) %>% 
         summarize(avgRatingUser = mean(rating), avgY_hat = mean(Y_hat), nUser = n()) %>% 
         mutate(DiffRatingUser = avgRatingUser - avgY_hat) %>%
         select(userId, DiffRatingUser, nUser)
  
}


# Compute for the Training Set the average rating of each user for all movies. 
  
  Uavg <- GetTheUserAverageRating(TrainingSet)
    
  TrainingSet <- TrainingSet %>% 
                  left_join(Uavg, by = "userId") %>% 
                  mutate(DiffRatingUser = ifelse(!is.na(DiffRatingUser), DiffRatingUser, 0), nUser = ifelse(!is.na(nUser), nUser, 0))
  
  TrainingSet <- TrainingSet %>% mutate(Y_hat = AvgOverall + DiffRatingMovie + DiffRatingUser)



# Function that computes the number of rating for each user for each genre, his averge rating across all movies of this genre and the difference between this average rating and the rating coming from the previous step.

GetTheGenreUserAverageRating <- function(DF) {
  
  DF %>% group_by(genres, userId) %>% 
         summarize(avgGenresUser = mean(rating), avgY_hat = mean(Y_hat), nGenresUser = n()) %>% 
         mutate(DiffGenresUser = avgGenresUser - avgY_hat) %>%
         select(genres, userId, DiffGenresUser, nGenresUser)
  
}


# Compute for the Training Set the average rating of each genre for each user.
  
  Gavg <- GetTheGenreUserAverageRating(TrainingSet)
  
  TrainingSet <- TrainingSet %>% 
                  left_join(Gavg, by = c("userId", "genres")) %>% 
                  mutate(DiffGenresUser = ifelse(!is.na(DiffGenresUser), DiffGenresUser, 0), nGenresUser = ifelse(!is.na(nGenresUser), nGenresUser, 0))
  
  TrainingSet <- TrainingSet %>% 
                  mutate(Y_hat = AvgOverall + DiffRatingMovie + DiffRatingUser + DiffGenresUser)
  

  
# Function that computes the number of rating for each user for each year, his averge rating across all movies he watched that year and the difference between this average rating and the rating coming from the previous step.

GetTheUserYearAverageRating <- function(DF) {
  
  DF %>% group_by(userId, ratingYear) %>% 
         summarize(avgUserYear = mean(rating), avgY_hat = mean(Y_hat), nUserYear = n()) %>% 
         mutate(DiffUserYear = avgUserYear - avgY_hat) %>%
         select(userId, ratingYear, DiffUserYear, nUserYear)
  
}


# Compute for the Training Set for each user (and all movies) the average rating per year.
  
  UYavg <- GetTheUserYearAverageRating(TrainingSet)
    
  TrainingSet <- TrainingSet %>% 
                  left_join(UYavg, by = c("userId", "ratingYear")) %>% 
                  mutate(DiffUserYear = ifelse(!is.na(DiffUserYear), DiffUserYear, 0), nUserYear = ifelse(!is.na(nUserYear), nUserYear, 0))
  
  TrainingSet <- TrainingSet %>% 
                  mutate(Y_hat = AvgOverall + DiffRatingMovie + DiffRatingUser + DiffGenresUser + DiffUserYear)


  
# Function that computes the number of rating for each movie for each year, his averge rating across all users having watched it that year and the difference between this average rating and the rating coming from the previous step.

GetTheMovieYearAverageRating <- function(DF) {
  
  DF %>% group_by(movieId, ratingYear) %>% 
         summarize(avgMovieYear = mean(rating), avgY_hat = mean(Y_hat), nMovieYear = n()) %>% 
         mutate(DiffMovieYear = avgMovieYear - avgY_hat) %>%
         select(movieId, ratingYear, DiffMovieYear, nMovieYear)
  
}


# Compute for the Training Set for each movie (across all users) the average rating per year.

  MYavg <- GetTheMovieYearAverageRating(TrainingSet)
  
  TrainingSet <- TrainingSet %>% 
                  left_join(MYavg, by = c("movieId", "ratingYear")) %>% 
                  mutate(DiffMovieYear = ifelse(!is.na(DiffMovieYear), DiffMovieYear, 0), nMovieYear = ifelse(!is.na(nMovieYear), nMovieYear, 0))
  
  TrainingSet <- TrainingSet %>% 
                  mutate(Y_hat = AvgOverall + DiffRatingMovie + DiffRatingUser + DiffGenresUser + DiffUserYear + DiffMovieYear)


  
# Function that computes the number of rating for each user across all movies realized a certain decade, his averge rating for those and the difference between this average rating and the rating coming from the previous step.

GetTheUserDecadeAverageRating <- function(DF) {
 
  DF %>% group_by(userId, movieDecade) %>% 
         summarize(avgUserDecade = mean(rating), avgY_hat = mean(Y_hat), nUserDecade = n()) %>% 
         mutate(DiffUserDecade = avgUserDecade - avgY_hat) %>%
         select(userId, movieDecade, DiffUserDecade, nUserDecade)
  
}


# Compute for the Training Set for each user the average rating given for movies of a particular decade.

  DUavg <- GetTheUserDecadeAverageRating(TrainingSet)
  
  TrainingSet <- TrainingSet %>% 
                  left_join(DUavg, by = c("userId", "movieDecade")) %>% 
                  mutate(DiffUserDecade = ifelse(!is.na(DiffUserDecade), DiffUserDecade, 0), nUserDecade = ifelse(!is.na(nUserDecade), nUserDecade, 0))
  
  TrainingSet <- TrainingSet %>% 
                  mutate(Y_hat = AvgOverall + DiffRatingMovie + DiffRatingUser + DiffGenresUser + DiffUserYear + DiffMovieYear + DiffUserDecade)
  

  
# Apply the data of the training to the Test Set.

  TestSet <- ExtractMovieYearAndRatingYear(TestSet)
  
  TestSet <- TestSet %>% mutate(Y_hat = AvgOverall)
  
  Results <- data.frame(Step = "Average overall", RMSE = sqrt(mean((TestSet$Y_hat - TestSet$rating)^2)))
  Results



  TestSet <- TestSet %>% 
              left_join(Mavg, by = "movieId") %>% 
              mutate(DiffRatingMovie = ifelse(!is.na(DiffRatingMovie), DiffRatingMovie, 0), nMovie = ifelse(!is.na(nMovie), nMovie, 0)) %>% 
              mutate(Y_hat = AvgOverall + DiffRatingMovie)
  
  Results <- rbind(Results, data.frame(Step = "Average per movie", RMSE = sqrt(mean((TestSet$Y_hat - TestSet$rating)^2))))
  Results

  
  
  TestSet <- TestSet %>% 
              left_join(Uavg, by = "userId") %>% 
              mutate(DiffRatingUser = ifelse(!is.na(DiffRatingUser), DiffRatingUser, 0), nUser = ifelse(!is.na(nUser), nUser, 0)) %>% 
              mutate(Y_hat = AvgOverall + DiffRatingMovie + DiffRatingUser)
  
  Results <- rbind(Results, data.frame(Step = "Average per user", RMSE = sqrt(mean((TestSet$Y_hat - TestSet$rating)^2))))
  Results

  
  
  TestSet <- TestSet %>% 
              left_join(Gavg, by = c("userId", "genres")) %>% 
              mutate(DiffGenresUser = ifelse(!is.na(DiffGenresUser), DiffGenresUser, 0), nGenresUser = ifelse(!is.na(nGenresUser), nGenresUser, 0)) %>% 
              mutate(Y_hat = AvgOverall + DiffRatingMovie + DiffRatingUser + DiffGenresUser)
  
  Results <- rbind(Results, data.frame(Step = "Average per genre and per user", RMSE = sqrt(mean((TestSet$Y_hat - TestSet$rating)^2))))
  Results

  
  
TestSet <- TestSet %>% 
  left_join(UYavg, by = c("userId", "ratingYear")) %>% 
  mutate(DiffUserYear = ifelse(!is.na(DiffUserYear), DiffUserYear, 0), nUserYear = ifelse(!is.na(nUserYear), nUserYear, 0)) %>% 
  mutate(Y_hat = AvgOverall + DiffRatingMovie + DiffRatingUser + DiffGenresUser + DiffUserYear)

Results <- rbind(Results, data.frame(Step = "Average per user per year", RMSE = sqrt(mean((TestSet$Y_hat - TestSet$rating)^2))))
Results



TestSet <- TestSet %>% 
  left_join(MYavg, by = c("movieId", "ratingYear")) %>% 
  mutate(DiffMovieYear = ifelse(!is.na(DiffMovieYear), DiffMovieYear, 0), nMovieYear = ifelse(!is.na(nMovieYear), nMovieYear, 0)) %>% 
  mutate(Y_hat = AvgOverall + DiffRatingMovie + DiffRatingUser + DiffGenresUser + DiffUserYear + DiffMovieYear)

Results <- rbind(Results, data.frame(Step = "Average per movie per year", RMSE = sqrt(mean((TestSet$Y_hat - TestSet$rating)^2))))
Results



TestSet <- TestSet %>% 
  left_join(DUavg, by = c("userId", "movieDecade")) %>% 
  mutate(DiffUserDecade = ifelse(!is.na(DiffUserDecade), DiffUserDecade, 0), nUserDecade = ifelse(!is.na(nUserDecade), nUserDecade, 0)) %>% 
  mutate(Y_hat = AvgOverall + DiffRatingMovie + DiffRatingUser + DiffGenresUser + DiffUserYear + DiffMovieYear+ DiffUserDecade)

Results <- rbind(Results, data.frame(Step = "Average per user per movie decade", RMSE = sqrt(mean((TestSet$Y_hat - TestSet$rating)^2))))
Results



## Step of determination of regularization factors.  
  
FineTune <- data.frame(fact = "0", k = 0, RMSE = 0, stringsAsFactors = FALSE)

# Determination of the optimal regularization for movie average rating.

for(kMovie in seq(0.1, 10, 0.1)) {
    
    TestSet <- TestSet %>% mutate(Y_hat = AvgOverall + 
                                    (nMovie*DiffRatingMovie)/(nMovie + kMovie))
    
    RMSE <- sqrt(mean((TestSet$rating - TestSet$Y_hat)^2))
    
    FineTune <- rbind(FineTune, data.frame(fact = "kMovie", k = kMovie, RMSE = RMSE))
    
  }

FineTune %>% filter(fact == "kMovie") %>% 
             ggplot(aes(k, RMSE)) + 
             geom_point(color = "blue") + 
             geom_point(color = "blue") + 
             theme_bw() + 
             labs(x = "kMovie", y = "RMSE", title = "Determination of the optimal regularization factor for movies average rating")

M <- FineTune %>% filter(fact == "kMovie") %>% 
                  summarize(min(RMSE)) %>% 
                  filter(row_number()==1) %>% pull()

kMovieOpt <- FineTune %>% filter(fact == "kMovie" & RMSE == M) %>% pull(k)

SummaryOptimalk <- data.frame(Factor = "kMovie", Optimalk = kMovieOpt, RMSE = M)

SummaryOptimalk



# Determination of the optimal regularization for user average rating.

for(kUser in seq(0.5, 50, 0.5)) {
  
  TestSet <- TestSet %>% mutate(Y_hat = AvgOverall + 
                                  (nMovie*DiffRatingMovie)/(nMovie + kMovieOpt) + 
                                  (nUser*DiffRatingUser)/(nUser + kUser))
  
  RMSE <- sqrt(mean((TestSet$rating - TestSet$Y_hat)^2))
  
  FineTune <- rbind(FineTune, data.frame(fact = "kUser", k = kUser, RMSE = RMSE))
  
}

FineTune %>% filter(fact == "kUser") %>% 
             ggplot(aes(k, RMSE)) + 
             geom_point(color = "blue") + 
             geom_point(color = "blue") + 
             theme_bw() + 
             labs(x = "kUser", y = "RMSE", title = "Determination of the optimal regularization factor for users average rating")

M <- FineTune %>% filter(fact == "kUser") %>% 
                  summarize(min(RMSE)) %>% 
                  filter(row_number()==1) %>% pull()

kUserOpt <- FineTune %>% filter(fact == "kUser" & RMSE == M) %>% pull(k)

SummaryOptimalk <- rbind(SummaryOptimalk, data.frame(Factor = "kUser", Optimalk = kUserOpt, RMSE = M))

SummaryOptimalk



# Determination of the optimal regularization for couple user / genre average rating.

for(kGenresUser in seq(0.1, 10, 0.1)) {
  
  TestSet <- TestSet %>% 
              mutate(Y_hat = AvgOverall + 
                     (nMovie*DiffRatingMovie)/(nMovie + kMovieOpt) + 
                     (nUser*DiffRatingUser)/(nUser + kUserOpt) + 
                     (nGenresUser*DiffGenresUser)/(nGenresUser + kGenresUser))
  
  RMSE <- sqrt(mean((TestSet$rating - TestSet$Y_hat)^2))
  
  FineTune <- rbind(FineTune, data.frame(fact = "kGenresUser", k = kGenresUser, RMSE = RMSE))
  
}

FineTune %>% filter(fact == "kGenresUser") %>% 
             ggplot(aes(k, RMSE)) + 
             geom_point(color = "blue") + 
                        theme_bw() + 
                        labs(x = "kGenresUser", y = "RMSE", title = "Determination of the optimal regularization factor for user / genres rating")

M <- FineTune %>% filter(fact == "kGenresUser") %>% 
                  summarize(min(RMSE)) %>% filter(row_number()==1) %>% pull()
kGenresUserOpt <- FineTune %>% filter(fact == "kGenresUser" & RMSE == M) %>% pull(k)

SummaryOptimalk <- rbind(SummaryOptimalk, data.frame(Factor = "kGenresUser", Optimalk = kGenresUserOpt, RMSE = M))

SummaryOptimalk



# Determination of the optimal regularization for couple user / rating year average rating.

for(kUserYear in seq(0.1, 10, 0.1)) {
  
  TestSet <- TestSet %>% 
              mutate(Y_hat = AvgOverall + 
                     (nMovie*DiffRatingMovie)/(nMovie + kMovieOpt) + 
                     (nUser*DiffRatingUser)/(nUser + kUserOpt) + 
                     (nGenresUser*DiffGenresUser)/(nGenresUser + kGenresUserOpt) + 
                     (nUserYear*DiffUserYear)/(nUserYear + kUserYear))
  
  RMSE <- sqrt(mean((TestSet$rating - TestSet$Y_hat)^2))
  
  FineTune <- rbind(FineTune, data.frame(fact = "kUserYear", k = kUserYear, RMSE = RMSE))
  
}

FineTune %>% filter(fact == "kUserYear") %>% 
             ggplot(aes(k, RMSE)) + 
             geom_point(color = "blue") + 
             theme_bw() + 
             labs(x = "kUserYear", y = "RMSE", title = "Determination of the optimal regularization factor for user / year average rating")

M <- FineTune %>% filter(fact == "kUserYear") %>% 
                  summarize(min(RMSE)) %>% filter(row_number()==1) %>% pull()

kUserYearOpt <- FineTune %>% filter(fact == "kUserYear" & RMSE == M) %>% pull(k)

SummaryOptimalk <- rbind(SummaryOptimalk, data.frame(Factor = "kUserYear", Optimalk = kUserYearOpt, RMSE = M))

SummaryOptimalk



# Determination of the optimal regularization for couple movie / rating year average rating.

for(kMovieYear in seq(0.5, 50, 0.5)) {
  
  TestSet <- TestSet %>% 
              mutate(Y_hat = AvgOverall + 
                     (nMovie*DiffRatingMovie)/(nMovie + kMovieOpt) + 
                     (nUser*DiffRatingUser)/(nUser + kUserOpt) + 
                     (nGenresUser*DiffGenresUser)/(nGenresUser + kGenresUserOpt) + 
                     (nUserYear*DiffUserYear)/(nUserYear + kUserYearOpt) + 
                     (nMovieYear*DiffMovieYear)/(nMovieYear + kMovieYear))
  
  RMSE <- sqrt(mean((TestSet$rating - TestSet$Y_hat)^2))
  
  FineTune <- rbind(FineTune, data.frame(fact = "kMovieYear", k = kMovieYear, RMSE = RMSE))
  
}

FineTune %>% filter(fact == "kMovieYear") %>% 
             ggplot(aes(k, RMSE)) + 
             geom_point(color = "blue") + 
             theme_bw() + 
             labs(x = "kMovieYear", y = "RMSE", title = "Determination of the optimal regularization factor for movie / year average rating")

M <- FineTune %>% filter(fact == "kMovieYear") %>% 
                  summarize(min(RMSE)) %>% filter(row_number()==1) %>% pull()

kMovieYearOpt <- FineTune %>% filter(fact == "kMovieYear" & RMSE == M) %>% pull(k)

SummaryOptimalk <- rbind(SummaryOptimalk, data.frame(Factor = "kMovieYear", Optimalk = kMovieYearOpt, RMSE = M))

SummaryOptimalk



# Determination of the optimal regularization for couple user / movie decade average rating.

for(kUserDecade in seq(0.1, 10, 0.1)) {
  
  TestSet <- TestSet %>% 
              mutate(Y_hat = AvgOverall + 
                     (nMovie*DiffRatingMovie)/(nMovie + kMovieOpt) + 
                     (nUser*DiffRatingUser)/(nUser + kUserOpt) + 
                     (nGenresUser*DiffGenresUser)/(nGenresUser + kGenresUserOpt) + 
                     (nUserYear*DiffUserYear)/(nUserYear + kUserYearOpt) + 
                     (nMovieYear*DiffMovieYear)/(nMovieYear + kMovieYearOpt) + 
                     (nUserDecade*DiffUserDecade)/(nUserDecade + kUserDecade))
  
  RMSE <- sqrt(mean((TestSet$rating - TestSet$Y_hat)^2))
  
  FineTune <- rbind(FineTune, data.frame(fact = "kUserDecade", k = kUserDecade, RMSE = RMSE))
  
}

FineTune %>% filter(fact == "kUserDecade") %>% 
             ggplot(aes(k, RMSE)) + 
             geom_point(color = "blue") + 
             theme_bw() + 
             labs(x = "kUserDecade", y = "RMSE", title = "Determination of the optimal regularization factor for user / movie decade average rating")

M <- FineTune %>% filter(fact == "kUserDecade") %>% 
                  summarize(min(RMSE)) %>% filter(row_number()==1) %>% pull()

kUserDecadeOpt <- FineTune %>% filter(fact == "kUserDecade" & RMSE == M) %>% pull(k)

SummaryOptimalk <- rbind(SummaryOptimalk, data.frame(Factor = "kUserMovieDecade", Optimalk = kUserDecadeOpt, RMSE = M))

SummaryOptimalk



# Computation of the final RMSE on the Test Set using all optimal regularization factors.

TestSet <- TestSet %>% 
            mutate(Y_hat = AvgOverall + 
                   (nMovie*DiffRatingMovie)/(nMovie + kMovieOpt) + 
                   (nUser*DiffRatingUser)/(nUser + kUserOpt) + 
                   (nGenresUser*DiffGenresUser)/(nGenresUser + kGenresUserOpt) + 
                   (nMovieYear*DiffMovieYear)/(nMovieYear + kMovieYearOpt) + 
                   (nUserYear*DiffUserYear)/(nUserYear + kUserYearOpt) + 
                   (nUserDecade*DiffUserDecade)/(nUserDecade + kUserDecadeOpt))

sqrt(mean((TestSet$Y_hat - TestSet$rating)^2))



# A bit of cleanup.

rm(AvgOverall, kMovie, kUser, kMovieYear, kUserYear, kGenresUser, kUserDecade, M, RMSE)
rm(Gavg, Mavg, MYavg, Uavg, UYavg, DUavg, TrainingSet, TestSet)


# Add to the Training Set two new columns: the rating year and the movie release date.

  EedxSet <- ExtractMovieYearAndRatingYear(edx)
  

# Compute for the Training Set the average rating for all movies and all users.
  
  AvgOverall <- GetTheBasis(EedxSet)
   
  EedxSet <- EedxSet %>% 
             mutate(Y_hat = AvgOverall)
  

# Compute for the Training Set the average rating of each movie for all users.
  
  Mavg <- GetTheMovieAverageRating(EedxSet)
  
  EedxSet <- EedxSet %>% 
             left_join(Mavg, by = "movieId") %>% 
             mutate(DiffRatingMovie = ifelse(!is.na(DiffRatingMovie), DiffRatingMovie, 0), nMovie = ifelse(!is.na(nMovie), nMovie, 0))
  
  EedxSet <- EedxSet %>% 
             mutate(Y_hat = AvgOverall + DiffRatingMovie)

  
# Compute for the Training Set the average rating of each user for all movies. 
  
  Uavg <- GetTheUserAverageRating(EedxSet)
    
  EedxSet <- EedxSet %>% 
             left_join(Uavg, by = "userId") %>% 
             mutate(DiffRatingUser = ifelse(!is.na(DiffRatingUser), DiffRatingUser, 0), nUser = ifelse(!is.na(nUser), nUser, 0))
  
  EedxSet <- EedxSet %>% 
             mutate(Y_hat = AvgOverall + DiffRatingMovie + DiffRatingUser)
  

# Compute for the Training Set for each movie (across all users) the average rating per year.

  Gavg <- GetTheGenreUserAverageRating(EedxSet)
  
  EedxSet <- EedxSet %>% 
             left_join(Gavg, by = c("userId", "genres")) %>% 
             mutate(DiffGenresUser = ifelse(!is.na(DiffGenresUser), DiffGenresUser, 0), nGenresUser = ifelse(!is.na(nGenresUser), nGenresUser, 0)) %>%
             mutate(Y_hat = AvgOverall + DiffRatingMovie + DiffRatingUser + DiffGenresUser)
  
  EedxSet <- EedxSet %>% 
             mutate(Y_hat = AvgOverall + DiffRatingMovie + DiffRatingUser + DiffGenresUser)
  
  
# Compute for the Training Set for each user (and all movies) the average rating per year.
  
  UYavg <- GetTheUserYearAverageRating(EedxSet)
    
  EedxSet <- EedxSet %>% 
             left_join(UYavg, by = c("userId", "ratingYear")) %>% 
             mutate(DiffUserYear = ifelse(!is.na(DiffUserYear), DiffUserYear, 0), nUserYear = ifelse(!is.na(nUserYear), nUserYear, 0))
  
  EedxSet <- EedxSet %>% 
             mutate(Y_hat = AvgOverall + DiffRatingMovie + DiffRatingUser + DiffGenresUser + DiffUserYear)


# Compute for the Training Set for each movie (across all users) the average rating per year.

  MYavg <- GetTheMovieYearAverageRating(EedxSet)
  
  EedxSet <- EedxSet %>% 
             left_join(MYavg, by = c("movieId", "ratingYear")) %>% 
             mutate(DiffMovieYear = ifelse(!is.na(DiffMovieYear), DiffMovieYear, 0), nMovieYear = ifelse(!is.na(nMovieYear), nMovieYear, 0))
  
  EedxSet <- EedxSet %>% 
             mutate(Y_hat = AvgOverall + DiffRatingMovie + DiffRatingUser + DiffGenresUser + DiffUserYear + DiffMovieYear)


# Compute for the Training Set for each user the average rating given for movies of a particular decade.

  DUavg <- GetTheUserDecadeAverageRating(EedxSet)
  
  EedxSet <- EedxSet %>% 
             left_join(DUavg, by = c("userId", "movieDecade")) %>% 
             mutate(DiffUserDecade = ifelse(!is.na(DiffUserDecade), DiffUserDecade, 0), nUserDecade = ifelse(!is.na(nUserDecade), nUserDecade, 0))
  
  EedxSet <- EedxSet %>% 
             mutate(Y_hat = AvgOverall + DiffRatingMovie + DiffRatingUser + DiffGenresUser + DiffUserYear + DiffMovieYear + DiffUserDecade)

  
  
# Apply the algorithm on the validation set.

  validation <- ExtractMovieYearAndRatingYear(validation)
  
  validation <- validation %>% 
                left_join(Mavg, by = "movieId") %>% 
                mutate(DiffRatingMovie = ifelse(!is.na(DiffRatingMovie), DiffRatingMovie, 0), nMovie = ifelse(!is.na(nMovie), nMovie, 0))
  
  validation <- validation %>% 
                left_join(Uavg, by = "userId") %>% 
                mutate(DiffRatingUser = ifelse(!is.na(DiffRatingUser), DiffRatingUser, 0), nUser = ifelse(!is.na(nUser), nUser, 0))
  
  validation <- validation %>% 
                left_join(Gavg, by = c("userId", "genres")) %>% 
                mutate(DiffGenresUser = ifelse(!is.na(DiffGenresUser), DiffGenresUser, 0), nGenresUser = ifelse(!is.na(nGenresUser), nGenresUser, 0))
  
  validation <- validation %>% 
                left_join(UYavg, by = c("userId", "ratingYear")) %>% 
                mutate(DiffUserYear = ifelse(!is.na(DiffUserYear), DiffUserYear, 0), nUserYear = ifelse(!is.na(nUserYear), nUserYear, 0))
  
  validation <- validation %>% 
                left_join(MYavg, by = c("movieId", "ratingYear")) %>% 
                mutate(DiffMovieYear = ifelse(!is.na(DiffMovieYear), DiffMovieYear, 0), nMovieYear = ifelse(!is.na(nMovieYear), nMovieYear, 0))

  validation <- validation %>% 
                left_join(DUavg, by = c("userId", "movieDecade")) %>% 
                mutate(DiffUserDecade = ifelse(!is.na(DiffUserDecade), DiffUserDecade, 0), nUserDecade = ifelse(!is.na(nUserDecade), nUserDecade, 0))

  validation <- validation %>% 
                mutate(Y_hat = AvgOverall + 
                                (nMovie*DiffRatingMovie)/(nMovie + kMovieOpt) + 
                                (nUser*DiffRatingUser)/(nUser + kUserOpt) + 
                                (nGenresUser*DiffGenresUser)/(nGenresUser + kGenresUserOpt) + 
                                (nMovieYear*DiffMovieYear)/(nMovieYear + kMovieYearOpt) + 
                                (nUserYear*DiffUserYear)/(nUserYear + kUserYearOpt) + 
                                (nUserDecade*DiffUserDecade)/(nUserDecade + kUserDecadeOpt))
  


# Ensure that all lines have a rating.

sum(!is.na(validation$rating))

sum(!is.na(validation$Y_hat))



# Compute the RMSE

sqrt(mean((validation$Y_hat - validation$rating)^2))
