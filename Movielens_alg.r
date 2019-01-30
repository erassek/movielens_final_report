
#############################################################
# Create edx set, validation set, and submission file
#############################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

# Creation of the dataset

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# Transformation to a dataframe 
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

										   
movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1)
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

# Data preparation

# "rating" column exploration
summary(movielens$rating)

# "movieId" column exploration
summary(movielens$movieId)

# "userId" column exploration
summary(movielens$userId)
	  
# Data exploration 

# number of userId and movieId 
movielens %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

# number of rating per movieId			
movielens %>% 
count(movieId) %>% 
ggplot(aes(n)) + 
geom_histogram(bins = 30, color = "black") + 
scale_x_log10() + 
ggtitle("Number of ratings per movie")

# number of rating per userId
movielens %>% 
count(userId) %>% 
ggplot(aes(n)) + 
geom_histogram(bins = 30, color = "blue") + 
scale_x_log10() + 
ggtitle("Number of ratings per user")


# Model-based approach
mu_hat <- mean(edx$rating)
mu_hat
[1] 3.512465

rmse_1<- RMSE(validation$rating, mu_hat)
rmse_1
[1] 1.061202


# Modeling movie effect
mu <- mean(edx$rating) 
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

predicted_ratings <- mu + validation %>% 
left_join(movie_avgs, by='movieId') %>%
.$b_i

rmse_2 <- RMSE(predicted_ratings, validation$rating)

#User effect

edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

user_avgs <- edx %>% 
left_join(movie_avgs, by='movieId') %>%
group_by(userId) %>%
summarize(b_u = mean(rating - mu - b_i))
  
predicted_ratings <- validation %>% 
left_join(movie_avgs, by='movieId') %>%
left_join(user_avgs, by='userId') %>%
mutate(pred = mu + b_i + b_u) %>%
.$pred

rmse_3 <- RMSE(predicted_ratings, validation$rating)

#Regularized content-based approach

lambda <- 3
mu <- mean(edx$rating)
movie_reg_avgs <- edx %>% 
group_by(movieId) %>% 
summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n())

predicted_ratings <- validation %>% 
left_join(movie_reg_avgs, by='movieId') %>%
mutate(pred = mu + b_i) %>%
.$pred
  
rmse_4 <- RMSE(predicted_ratings, validation$rating)
 

#Regularized content-based approach + user-based approach

#set the tunning parameter

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){

#starting with the mean
mu <- mean(edx$rating)
  
#adding the regularized movie effect
b_i <- edx %>% 
group_by(movieId) %>%
summarize(b_i = sum(rating - mu)/(n()+l))

#adding the user effect  
b_u <- edx %>% 
left_join(b_i, by="movieId") %>%
group_by(userId) %>%
summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
predicted_ratings <- 
validation %>% 
left_join(b_i, by = "movieId") %>%
left_join(b_u, by = "userId") %>%
mutate(pred = mu + b_i + b_u) %>%
.$pred
  
  return(RMSE(predicted_ratings, validation$rating))
})

qplot(lambdas, rmses)  


